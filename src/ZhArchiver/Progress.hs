module ZhArchiver.Progress
  ( ShowId (..),
    showValId,
    Cli (..),
    defaultCli,
    cliWithHeader,
    pushHeader,
    appendHeader,
    showMessage,
    showProgress,
    endProgress,
  )
where

import Control.Monad
import Data.Foldable
import GHC.IO.Handle
import GHC.IO.Handle.FD

data Cli = Cli
  { cliMultiline :: Bool,
    cliMaxWidth :: Int,
    cliMsgHeader :: [(String, Int)]
  }

defaultCli :: Cli
defaultCli =
  Cli
    { cliMultiline = False,
      cliMaxWidth = 80,
      cliMsgHeader = []
    }

cliWithHeader :: String -> Cli
cliWithHeader h = pushHeader h defaultCli

class ShowId a where
  showId :: a -> String
  showType :: a -> String

showValId :: (ShowId a) => a -> String
showValId a = showType a ++ " " ++ showId a

pushHeader :: String -> Cli -> Cli
pushHeader s c@(Cli {cliMsgHeader = h}) = c {cliMsgHeader = (s, length s) : h}

appendHeader :: String -> Cli -> Cli
appendHeader s c =
  c
    { cliMsgHeader =
        let orig = cliMsgHeader c
            l = length s
         in if null orig
              then [(s, l)]
              else (let (s0, l0) = head orig in (s0 ++ s, l0 + l)) : tail orig
    }

clearLine :: String
clearLine = "\ESC[2K\ESC[G"

showMsg :: Cli -> String -> IO ()
showMsg Cli {cliMsgHeader = h, cliMaxWidth = mx} msg =
  case h of
    [] ->
      if msgLen > mx
        then putStr (take (mx - 3) msg) >> putStr ".."
        else putStr msg
    (x, l) : xs ->
      if msgLen + l + 2 > mx
        then traverse_ putStr [x, ": ", take (mx - l - 2 - 3) msg, "..."]
        else truncateHdr 0 (mx - msgLen - 2 - l) (reverse xs) >> traverse_ putStr [x, ": ", msg]
  where
    msgLen = length msg
    truncateHdr _ _ [] = pure ()
    truncateHdr len lim ((s, l) : xs) =
      if len + l + 2 + 5 > lim
        then putStr "[...]"
        else traverse_ putStr ["[", s, "]"] >> truncateHdr (len + 2 + l) lim xs

showMessage :: Cli -> String -> IO ()
showMessage c@(Cli {cliMultiline = m}) msg = do
  putStr clearLine
  showMsg c msg
  when m $ putChar '\n'

showProgress :: Cli -> String -> IO ()
showProgress c msg = do
  putStr clearLine
  showMsg c msg
  hFlush stdout

endProgress :: Cli -> IO ()
endProgress Cli {cliMultiline = m} = when m $ putChar '\n'