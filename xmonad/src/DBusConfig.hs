module DBusConfig (mkDbusPP) where

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Hooks.DynamicLog (def, PP(..), wrap)
import Data.Maybe (fromJust)

mkDbusPP :: IO PP
mkDbusPP = do
  dbus <- D.connectSession
  getWellKnownName dbus
  pure $ prettyPrinter dbus

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = def
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

busName :: String
busName = "org.xmonad.Log"

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  _ <- D.requestName dbus (D.busName_ busName)
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

body :: String -> [D.Variant]
body s = [D.toVariant ("<b>" ++ UTF8.decodeString s ++ "</b>")]

baseSignal :: D.Signal
baseSignal = D.signal objectPath interfaceName memberName
  where
    objectPath = fromJust (D.parseObjectPath "/org/xmonad/Log")
    interfaceName = fromJust (D.parseInterfaceName busName)
    memberName = fromJust $ D.parseMemberName "Update"

signalWithBody :: String -> D.Signal
signalWithBody s = baseSignal { D.signalBody = body s }

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus s = D.emit dbus $ signalWithBody s

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
