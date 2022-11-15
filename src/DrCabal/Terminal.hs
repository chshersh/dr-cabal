{- |
Module                  : DrCabal.Terminal
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Utility functions to work with the terminal output.
-}

module DrCabal.Terminal
    ( getTerminalWidth
    , clearScreen
    ) where

import Colourista.Short (u)
import System.Console.ANSI (clearFromCursorToScreenEnd, cursorUpLine)
import qualified System.Console.Terminal.Size as Terminal


{- | Get the width of the current terminal.

This function exits the process with the message if it can't get the
width of the current terminal.
-}
getTerminalWidth :: IO Int
getTerminalWidth = Terminal.size >>= \case
    Just (Terminal.Window _height width) -> pure width
    Nothing -> do
        putText $ unlines
            [ "Error getting the terminal width. If you see this error, open an issue"
            , "in the 'dr-cabal' issue tracker and provide as many details as possible"
            , ""
            , "  * " <> u "https://github.com/chshersh/dr-cabal/issues/new"
            ]
        exitFailure

{- | Clears the the @screenHeight@ number of lines in the screen. Pass
the number of lines in the output to clear the entire screen.
-}
clearScreen :: Int -> IO ()
clearScreen screenHeight = do
    -- https://github.com/UnkindPartition/ansi-terminal/issues/141
    when (screenHeight > 0) $
        cursorUpLine screenHeight
    clearFromCursorToScreenEnd
