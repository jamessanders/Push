module Push.Utils where

import System.Process
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, w2c)

git args = do
  (_, Just hout, _, ph) <- createProcess (proc "git" args){ std_out = CreatePipe }
  ec <- waitForProcess ph
  BC.hGetContents hout


unsafeUnpackBS = map w2c . BS.unpack