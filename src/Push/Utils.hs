module Push.Utils where

import System.Process
import qualified Data.ByteString.Char8 as BC

git args = do
  (_, Just hout, _, ph) <- createProcess (proc "git" args){ std_out = CreatePipe }
  ec <- waitForProcess ph
  BC.hGetContents hout
