-- | The main function for the @futhark@ command line program.
module Futhark.CLI.Main (main) where

import Futhark.CLI.Completion qualified as Completion
import Control.Exception
import Data.Maybe
import Data.Text.IO qualified as T
import Futhark.Error
import Futhark.Util (maxinum, showText)
import Futhark.Util.Options
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.Environment
import System.Exit
import System.IO
import Prelude

msg :: String
msg =
  unlines $
    ["<command> options...", "Commands:", ""]
      ++ [ "   " <> cmd <> replicate (k - length cmd) ' ' <> desc
           | (cmd, (_, desc)) <- Completion.commands
         ]
  where
    k = maxinum (map (length . fst) Completion.commands) + 3

-- | Catch all IO exceptions and print a better error message if they
-- happen.
reportingIOErrors :: IO () -> IO ()
reportingIOErrors =
  flip
    catches
    [ Handler onExit,
      Handler onICE,
      Handler onIOException,
      Handler onError
    ]
  where
    onExit :: ExitCode -> IO ()
    onExit = throwIO

    onICE :: InternalError -> IO ()
    onICE (Error CompilerLimitation s) = do
      T.hPutStrLn stderr "Known compiler limitation encountered.  Sorry."
      T.hPutStrLn stderr "Revise your program or try a different Futhark compiler."
      T.hPutStrLn stderr s
      exitWith $ ExitFailure 1
    onICE (Error CompilerBug s) = do
      T.hPutStrLn stderr "Internal compiler error."
      T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues."
      T.hPutStrLn stderr s
      exitWith $ ExitFailure 1

    onError :: SomeException -> IO ()
    onError e
      | Just UserInterrupt <- asyncExceptionFromException e =
          pure () -- This corresponds to CTRL-C, which is not an error.
      | otherwise = do
          T.hPutStrLn stderr "Internal compiler error (unhandled IO exception)."
          T.hPutStrLn stderr "Please report this at https://github.com/diku-dk/futhark/issues"
          T.hPutStrLn stderr $ showText e
          exitWith $ ExitFailure 1

    onIOException :: IOException -> IO ()
    onIOException e
      | ioe_type e == ResourceVanished =
          exitWith $ ExitFailure 1
      | otherwise = throw e

-- | The @futhark@ executable.
main :: IO ()
main = reportingIOErrors $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  setLocaleEncoding utf8
  args <- getArgs
  prog <- getProgName
  case args of
    cmd : args'
      | Just (m, _) <- lookup cmd Completion.commands -> m (unwords [prog, cmd]) args'
    _ -> mainWithOptions () [] msg (const . const Nothing) prog args
