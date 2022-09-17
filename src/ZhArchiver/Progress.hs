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
import GHC.IO.Handle
import GHC.IO.Handle.FD

data Cli = Cli
  { cliMultiline, cliHeadTruncated :: Bool,
    cliMaxHeader, cliHeaderCnt :: Int,
    cliMsgHeader :: [String]
  }

defaultCli :: Cli
defaultCli =
  Cli
    { cliMultiline = False,
      cliHeadTruncated = False,
      cliMaxHeader = 10,
      cliHeaderCnt = 0,
      cliMsgHeader = []
    }

cliWithHeader :: String -> Cli
cliWithHeader h =
  Cli
    { cliMultiline = False,
      cliHeadTruncated = False,
      cliMaxHeader = 10,
      cliHeaderCnt = 1,
      cliMsgHeader = [h]
    }

class ShowId a where
  showId :: a -> String
  showType :: a -> String

showValId :: (ShowId a) => a -> String
showValId a = showType a ++ " " ++ showId a

pushHeader :: String -> Cli -> Cli
pushHeader s c@(Cli {cliMaxHeader = mx, cliHeaderCnt = cn, cliMsgHeader = h}) =
  if cn == mx
    then c {cliMsgHeader = s : tail h, cliHeadTruncated = True}
    else c {cliHeaderCnt = cn + 1, cliMsgHeader = s : h}

appendHeader :: String -> Cli -> Cli
appendHeader s c =
  c
    { cliMsgHeader =
        let orig = cliMsgHeader c
         in if null orig
              then [s]
              else (head orig ++ s) : tail orig
    }

clearLine :: String
clearLine = "\ESC[2K\ESC[G"

showHeader :: Cli -> String
showHeader Cli {cliMsgHeader = h, cliHeadTruncated = ht} =
  case h of
    [] -> ""
    x : xs ->
      concat
        ( reverse
            ((\s -> ('[' : s) ++ "]") <$> xs)
        )
        ++ (if ht then "[...]" else "")
        ++ x
        ++ ": "

showMessage :: Cli -> String -> IO ()
showMessage c@(Cli {cliMultiline = m}) msg = do
  putStr (clearLine ++ showHeader c ++ msg)
  when m $ putChar '\n'

showProgress :: Cli -> String -> IO ()
showProgress c msg = do
  putStr (clearLine ++ showHeader c ++ msg)
  hFlush stdout

endProgress :: Cli -> IO ()
endProgress Cli {cliMultiline = m} = when m $ putChar '\n'