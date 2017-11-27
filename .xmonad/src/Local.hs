module Local where

import XMonad
import Data.List (isPrefixOf)

extraKeys :: [(String, X ())]
extraKeys = []
  where _isChromium = isPrefixOf "chromium-browser" `fmap` resource <&&>
                     className =? "Chromium-browser"
        _isFirefox = className =? "Firefox"
