{--
    randomDisplay displays randomly selected non-blank lines of specified files.

    Usage:	 randomDisplay.hs --help
             randomDisplay.hs [NUMBER OF LINES TO DISPLAY] [FILES]


    Todo:
        - use the faster Data.ByteString.Lazy
        - better command line arguments handling (http://leiffrenzel.de/papers/commandline-options-in-haskell.html)


    Author: Zongheng Yang
    Date  : May 25, 2012
--}

import System.Environment (getArgs)
import System.Random (randomRs, newStdGen)
import Data.Char (isDigit, isSpace, isControl)
import Control.Applicative

usageMessage :: String
usageMessage = "randomDisplay displays randomly selected non-blank lines of specified files.\n\nUsage:\t randomDisplay.hs --help\n\t randomDisplay.hs [NUMBER OF LINES TO DISPLAY] [FILES]\n"

main :: IO ()
main = do
    rawArgs <- getArgs
    case all isDigit (head rawArgs) of
        False -> putStrLn usageMessage
        True  -> do
            let numLinesToDisplay = read . head $ rawArgs
            let fileList = tail rawArgs

            -- get contents
            contents <- (map lines) <$> mapM readFile fileList  -- [[String]]
            let concatContents = filter (\st -> not (null st || all isSpace st || all isControl st)) $ concat contents

            -- generate the random lines
            gen <- newStdGen
            -- a list of lines selected
            let randomLinesToDisplay = take numLinesToDisplay $ randomRs (0, length concatContents - 1) gen 

            -- print the lines
            mapM_ putStrLn $ map (("*** "++) . (concatContents !!)) randomLinesToDisplay
