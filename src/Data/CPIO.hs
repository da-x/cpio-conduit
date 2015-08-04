{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}

--
-- This is the main interface. APIs should be quite self-explainatory.
--

module Data.CPIO
  ( readCPIO
  , writeCPIO
  , Entry(..)
  , FormatError(..)
  , isEntryDirectory
  ) where

import           Control.Applicative    ((<$>))
import qualified Control.Exception      as E
import           Control.Monad          (forM_, when)
import           Data.Binary.Get        (getWord32be, runGet)
import           Data.Binary.Put        (putWord32be, runPut)
import           Data.Bits              ((.&.))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import           Data.Conduit
import qualified Data.Conduit.Binary    as CB
import           Data.Int               (Int64)
import           Data.Typeable          (Typeable)
import           Data.Word              (Word32)

data Entry = Entry
  { cpioInode    :: !Word32
  , cpioMode     :: !Word32
  , cpioUid      :: !Word32
  , cpioGid      :: !Word32
  , cpioNLink    :: !Word32
  , cpioMTime    :: !Word32
  , cpioFileSize :: !Word32
  , cpioDevMaj   :: !Word32
  , cpioDevMin   :: !Word32
  , cpioRDevMaj  :: !Word32
  , cpioRDevMin  :: !Word32
  , cpioCRC32    :: Maybe Word32
  , cpioFileName :: ByteString
  , cpioFileData :: BL.ByteString
  } deriving (Show, Read, Eq)

data FormatError
  = TruncatedArchive
  | InvalidMagic ByteString
  | InvalidHex ByteString
  deriving (Typeable)

instance E.Exception FormatError

isEntryDirectory :: Entry -> Bool
isEntryDirectory entry = (cpioMode entry) .&. 0o040000 /= 0

instance Show FormatError where
  show TruncatedArchive         = "truncated cpio archive"
  show (InvalidMagic s)         = "invalid magic: " ++ (show s)
  show (InvalidHex s)           = "invalid hex: " ++ (show s)

takeExactlyLazy :: Monad m => Int64 -> Consumer ByteString m BL.ByteString
takeExactlyLazy len = do
    x <- CB.take $ fromIntegral len
    if BL.length x == len
        then return  x
        else E.throw TruncatedArchive

takeExactly :: Monad m => Int64 -> Consumer ByteString m ByteString
takeExactly len = fmap (BS.concat . BL.toChunks) $ takeExactlyLazy len

trailerText :: ByteString
trailerText = "TRAILER!!!"

alignTo4 :: Integral a => a -> a
alignTo4 0 = 0
alignTo4 n = 3 - ((n - 1) `mod` 4)

readCPIO :: Monad m => Conduit ByteString m Entry
readCPIO = do
  magic <- fmap (BS.concat . BL.toChunks) $ CB.take 6
  has_crc <-
    case magic of
      "070701" -> return False
      "070702" -> return True
      _ -> E.throw (InvalidMagic magic)
  inode <- decodeR32
  mode <- decodeR32
  uid <- decodeR32
  gid <- decodeR32
  nlink <- decodeR32
  mtime <- decodeR32
  filesize <- decodeR32
  devmaj <- decodeR32
  devmin <- decodeR32
  rdevmaj <- decodeR32
  rdevmin <- decodeR32
  filenamesize <- decodeR32
  crc32 <- decodeR32
  let filenamesize_ = fromInteger $ toInteger filenamesize
  filename <- (BS.takeWhile (/= 0)) <$> takeExactly
              filenamesize_
  _ <- takeExactly $ alignTo4 $ 110 + filenamesize_
  let filesize_ = (fromInteger $ toInteger filesize)
  filedata <- takeExactlyLazy filesize_
  let entry =
        Entry inode mode uid gid nlink mtime filesize devmaj
              devmin rdevmaj rdevmin
        (if has_crc then Nothing else Just crc32) filename filedata
  when (filename /= trailerText) $ do
    yield entry
    _ <- takeExactly $ alignTo4 filesize_
    readCPIO

  where
    decodeR32 = do
      v <- takeExactly 8
      case B16.decode  v of
       (decoded,  "") ->
         return $ runGet getWord32be $ BL.fromChunks [ decoded ]
       (_, _) ->
         E.throw (InvalidHex v)

writeCPIO :: Monad m => Conduit Entry m ByteString
writeCPIO = do
  entry_ <- await
  case entry_ of
    Nothing ->
      write_entry $ Entry 0 0 0 0 0 0 0 0 0 0 0 (Just 0) trailerText ""
    Just entry -> do
      write_entry entry
      writeCPIO
  where
    encodeR32 x = yield $ B16.encode $ BS.concat $ BL.toChunks $ runPut (putWord32be x)
    write_entry entry = do
      case cpioCRC32 entry of
        Nothing -> yield "070701"
        Just _ -> yield "070702"
      encodeR32 $ cpioInode entry
      encodeR32 $ cpioMode entry
      encodeR32 $ cpioUid entry
      encodeR32 $ cpioGid entry
      encodeR32 $ cpioNLink entry
      encodeR32 $ cpioMTime entry
      let file_size = cpioFileSize entry
      encodeR32 $ file_size
      encodeR32 $ cpioDevMaj entry
      encodeR32 $ cpioDevMin entry
      encodeR32 $ cpioRDevMaj entry
      encodeR32 $ cpioRDevMin entry
      let filename_length =
            1 + (fromInteger $ toInteger $ BS.length $ cpioFileName entry)
      encodeR32 $ (filename_length :: Word32)
      case cpioCRC32 entry of
        Nothing -> encodeR32 0
        Just x -> encodeR32 x
      yield $ cpioFileName entry
      yield $ "\NUL"
      yield $ BS.replicate (alignTo4 $ 110 + filename_length) 0
      forM_ (BL.toChunks $ cpioFileData entry) yield
      yield $ BS.replicate (alignTo4 (fromInteger $ toInteger file_size)) 0
