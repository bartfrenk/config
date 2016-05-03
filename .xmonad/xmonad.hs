{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- TOOD: define layout for Gimp

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.WindowBringer
import XMonad.Layout.Tabbed
import XMonad.Layout.ComboP
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Layout.ComboP ()
import XMonad.Layout.Combo
import XMonad.Layout.ZoomRow
import XMonad.Layout.Fullscreen
import XMonad.Config.Xfce
import XMonad.Actions.WindowGo (runOrRaise)
import Data.Maybe
import XMonad.Util.WindowProperties ()
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Monoid (Endo)
import Solarized

tabTheme :: Theme
tabTheme = def {
    fontName = "xft:inconsolata:size=9:antialias=true:hinting=true",
    decoHeight = 28,
    activeTextColor = red,
    activeColor = base03,
    activeBorderColor = base02,
    inactiveTextColor = base0,
    inactiveColor = base02,
    inactiveBorderColor = base02
}

managePlacement :: Query (Endo WindowSet)
managePlacement = composeAll [title =? "Save As" --> doFloat,
                              title =? "Whisker Menu" --> doFloat]

spawnAndNotify cmd message = do
  spawn cmd
  spawn $ "notify-send -t 1000 " ++ (quote message)
  where
    quote str = "\"" ++ str ++ "\""

extraKeys :: [(String, X ())]
extraKeys = [("<XF86AudioLowerVolume>", setMasterAudio "10%-"),
             ("<XF86AudioRaiseVolume>", setMasterAudio "10%+"),
             ("<XF86AudioMute>", setMasterAudio "1+ toggle"),
             ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5"),
             ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5"),
             ("C-M-[", disableTouchPad),
             ("C-M-]", enableTouchPad),
             ("M-s", enableSpeaker),
             ("M-S-s", disableSpeaker),
             ("C-M-b", sendMessage $ ToggleStrut U),
             ("C-M-=", sendMessage $ Zoom (5/4)),
             ("C-M--", sendMessage $ Zoom (4/5)),
             ("C-M-h", sendMessage Shrink),
             ("C-M-l", sendMessage Expand),
             ("M-l", sendMessage $ Go R),
             ("M-h", sendMessage $ Go L),
             ("M-j", sendMessage $ Go D),
             ("M-k", sendMessage $ Go U),
             ("M-S-l", sendMessage $ Move R),
             ("M-S-h", sendMessage $ Move L),
             ("M-S-q", spawn "xfce4-session-logout"),
             ("M-p", spawn $ dmenu ++ " " ++ (unwords $ menuArgs "Run")),
             ("M-g", gotoMenuArgs $ menuArgs "Go"),
             ("M-b", bringMenuArgs $ menuArgs "Bring"),
             ("M-S-b", runOrRaise "chromium-browser" isChromium),
             ("M-S-e", runOrRaise "emacs" isEmacs),
             ("M-S-f", runOrRaise "thunar" isThunar)]
  where setMasterAudio cmd = spawn $ "amixer -D pulse set Master " ++ cmd
        enableSpeaker = let cmd = setSpeaker "analog-output-speaker"
                        in spawnAndNotify cmd "speaker enabled"
        disableSpeaker = let cmd = setSpeaker "analog-output-headphones"
                         in spawnAndNotify cmd "speaker disabled"
        setSpeaker speaker = "pacmd set-sink-port 1 " ++ speaker
        disableTouchPad = let cmd = "xinput --set-prop \"AlpsPS/2 ALPS DualPoint TouchPad\" \"Device Enabled\" 0"
                          in spawnAndNotify cmd "touchpad disabled"
        enableTouchPad = let cmd = "xinput --set-prop \"AlpsPS/2 ALPS DualPoint TouchPad\" \"Device Enabled\" 1"
                         in spawnAndNotify cmd "touchpad enabled"
        menuArgs s = ["-fn", "Inconsolata-11", "-p", s, "-l", "6"]
        dmenu = "/usr/local/bin/dmenu_run"
        isChromium = className =? "chromium-browser"
        isEmacs = resource =? "emacs24" <||> resource =? "emacs"
        isThunar = resource =? "thunar"

defaultPanes :: Tall a
defaultPanes = (Tall 1 0.03 0.5)

tabbedWindow = tabbedBottom shrinkText tabTheme
isTerminal = ClassName "Xfce4-terminal"

devLayout = windowNavigation $ combineTwoP defaultPanes left right condition
            where right = tabbedWindow
                  left = Mirror zoomRow
                  condition = Not isTerminal

flexLayout = windowNavigation $ combineTwo defaultPanes left right
             where right = tabbedWindow
                   left = tabbedWindow

gridLayout = windowNavigation $ combineTwo defaultPanes left right
             where right = Mirror zoomRow
                   left = Mirror zoomRow

consLayout = windowNavigation $ combineTwoP split left right condition
             where split = Mirror $ Tall 1 0.03 0.8
                   left = zoomRow
                   right = tabbedWindow
                   condition = Not isTerminal

defaultLayouts = avoidStruts $ defaultLayoutHook
    where defaultLayoutHook = noBorders (tabbedBottom shrinkText tabTheme) |||
                              noBorders Full |||
                              devLayout |||
                              flexLayout |||
                              gridLayout |||
                              consLayout

fullscreenLayouts =
    lessBorders OnlyFloat $ fullscreenFull Full

layoutRingPerWorkspace = onWorkspace "8" fullscreenLayouts $ defaultLayouts

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ xfceConfig {
        borderWidth = 3,
        normalBorderColor = base02,
        focusedBorderColor = red,
        terminal = "/usr/bin/xfce4-terminal",
        focusFollowsMouse = False,
        workspaces = map show (take 9 [(1 :: Integer)..]),
        modMask = mod4Mask,
        manageHook = managePlacement <+>
                     manageDocks <+>
                     manageHook def,
        layoutHook = layoutRingPerWorkspace,
        startupHook = setWMName "LG3D",
        logHook = dynamicLogWithPP (prettyPrinter dbus)
    } `additionalKeysP` extraKeys

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = def {
    ppOutput = dbusOutput dbus,
    ppTitle = pangoSanitize,
    ppCurrent = pangoColor "green" . wrap "[" "]" . pangoSanitize,
    ppVisible = pangoColor "yellow" . wrap "(" ")" . pangoSanitize,
    ppHidden = pangoColor "gray",
    ppUrgent = pangoColor "red",
    ppLayout = const "",
    ppSep = " "
}

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus =
  let flags = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  in do
    _ <- D.requestName dbus (D.busName_ "org.xmonad.Log") flags
    return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (fromJust (D.parseObjectPath "/org/xmonad/Log"))
                           (fromJust (D.parseInterfaceName "org.xmonad.Log"))
                           (fromJust (D.parseMemberName "Update"))) {
            D.signalBody = [D.toVariant (pangoBold (UTF8.decodeString str))]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
    where
        left = "<span foreground=\"" ++ fg ++ "\">"
        right = "</span>"

pangoBold = wrap "<b>" "</b>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
    where
        sanitize '>' xs = "&gt;" ++ xs
        sanitize '<' xs = "&lt;" ++ xs
        sanitize '\"' xs = "&quot;" ++ xs
        sanitize '&' xs = "&amp;" ++ xs
        sanitize x xs = x:xs
