{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

See README for more info
-}

module DrCabal
    ( main
    ) where

import DrCabal.Cli (Command (..), readCommand)
import DrCabal.Profile (profile)
import DrCabal.Watch (watch)


main :: IO ()
main = readCommand >>= runDrCabal

runDrCabal :: Command -> IO ()
runDrCabal = \case
    Watch args   -> watch args
    Profile args -> profile args
