{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable        #-}

import           Control.Monad                (when)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Typeable          (Typeable)
import qualified Data.ByteString              as BS
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.CPIO
import qualified Control.Exception      as E

fileCpio :: [Entry]
fileCpio = [Entry {cpioInode = 19005793, cpioMode = 16893, cpioUid = 1000, cpioGid = 1000, cpioNLink = 3, cpioMTime = 1438703216, cpioFileSize = 0, cpioDevMaj = 8, cpioDevMin = 8, cpioRDevMaj = 0, cpioRDevMin = 0, cpioCRC32 = Nothing, cpioFileName = "1", cpioFileData = ""},Entry {cpioInode = 19005794, cpioMode = 33204, cpioUid = 1000, cpioGid = 1000, cpioNLink = 1, cpioMTime = 1438703214, cpioFileSize = 7, cpioDevMaj = 8, cpioDevMin = 8, cpioRDevMaj = 0, cpioRDevMin = 0, cpioCRC32 = Nothing, cpioFileName = "1/README.txt", cpioFileData = "Hello\n\n"},Entry {cpioInode = 19005795, cpioMode = 16893, cpioUid = 1000, cpioGid = 1000, cpioNLink = 2, cpioMTime = 1438703220, cpioFileSize = 0, cpioDevMaj = 8, cpioDevMin = 8, cpioRDevMaj = 0, cpioRDevMin = 0, cpioCRC32 = Nothing, cpioFileName = "1/subdir", cpioFileData = ""},Entry {cpioInode = 19005796, cpioMode = 33204, cpioUid = 1000, cpioGid = 1000, cpioNLink = 1, cpioMTime = 1438703220, cpioFileSize = 0, cpioDevMaj = 8, cpioDevMin = 8, cpioRDevMaj = 0, cpioRDevMin = 0, cpioCRC32 = Nothing, cpioFileName = "1/subdir/somefile.txt", cpioFileData = ""}]

encoded = "07070101220161000041fd000003e8000003e80000000355c0de70000000000000000800000008000000000000000000000002000000001\NUL07070101220162000081b4000003e8000003e80000000155c0de6e00000007000000080000000800000000000000000000000d000000001/README.txt\NUL\NULHello\n\n\NUL07070101220163000041fd000003e8000003e80000000255c0de74000000000000000800000008000000000000000000000009000000001/subdir\NUL\NUL07070101220164000081b4000003e8000003e80000000155c0de74000000000000000800000008000000000000000000000016000000001/subdir/somefile.txt\NUL07070200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b00000000TRAILER!!!\NUL\NUL\NUL\NUL"

instance E.Exception TestError

data TestError
  = Msg String
  deriving (Typeable)

instance Show TestError where
  show (Msg s) = "test error: " ++ s

err :: String -> a
err s = E.throw $ Msg s

main :: IO ()
main = do
  let filename = "tests/file.cpio"
  lst <- runResourceT $ CB.sourceFile filename $$ readCPIO =$ CL.consume
  when (lst /= fileCpio) $ do
    err $ "Invalid output: " ++ (show lst)
  new <- fmap BS.concat $ CL.sourceList lst $$ writeCPIO =$ CL.consume
  when (new /= encoded) $ do
    putStrLn $ "new: " ++ (show new)
    err $ "Invalid content generated"
