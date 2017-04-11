{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- TOOD: define layout for Gimp
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.WindowBringer
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.Tabbed
import XMonad.Layout.ComboP
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.ZoomRow
import XMonad.Layout.Fullscreen
import XMonad.Config.Xfce
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Util.Dmenu (menuMapArgs)

import Data.Maybe
import Data.Foldable (forM_)
import XMonad.Util.WindowProperties ()
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Monoid (Endo)
import Colors
import RecentCommands
import qualified Local

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

-- Global since required for doFloat and runOrRaise
isAuthy = title =? "Authy"

managePlacement :: Query (Endo WindowSet)
managePlacement = composeAll [title =? "Save As" --> doFloat,
                              title =? "Whisker Menu" --> doFloat,
                              isAuthy --> doFloat]



spawnAndNotify cmd message = do
  spawn cmd
  spawn $ "notify-send -t 1000 " ++ quote message
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
             ("M-p", spawn $ dmenuRun ++ " " ++ unwords (menuArgs "Run")),
             ("M-g", gotoMenuArgs $ menuArgs "Go"),
             ("M-b", bringMenuArgs $ menuArgs "Bring"),
             ("M-S-m", runOrRaise "/home/bart/bin/emacs" isEmacs),
             ("M-S-r", recentCommandsMenu dmenu "/home/bart/.local/share/recently-used.xbel"),
             ("M-S-f", runOrRaise "thunar" isThunar),
             ("M-S-s", runOrRaise "slack" isSlack),
             ("M-S-t", spawn "~/bin/term-tmux"),
             ("M-S-a", runOrRaise "authy" isAuthy)]
  where setMasterAudio cmd = spawn $ "amixer -D pulse set Master " ++ cmd
        disableTouchPad = let cmd = "xinput --disable \"AlpsPS/2 ALPS DualPoint TouchPad\""
                          in spawnAndNotify cmd "touchpad disabled"
        enableTouchPad = let cmd = "xinput --enable \"AlpsPS/2 ALPS DualPoint TouchPad\""
                         in spawnAndNotify cmd "touchpad enabled"
        dmenuRun = "/usr/bin/dmenu_run"
        dmenu = "/usr/bin/dmenu"
        isEmacs = resource =? "emacs24" <||> resource =? "emacs"
        isThunar = resource =? "thunar"
        isSlack = className =? "Slack"

-- | Construct arguments for passing to dmenu.
menuArgs :: String -> [String]
menuArgs s = ["-fn", "SourceCodePro-9", "-p", s, "-l", "6"]

-- | Shows dmenu with recently used commands, and runs the selection.
-- First argument is the location of dmenu, second is the location of
-- the recently used file (which should be in 'xbel' format).
recentCommandsMenu :: FilePath -> FilePath -> X ()
recentCommandsMenu dmenu path = do
  cmds <- liftIO (recentCommands path)
  sel <- menuMapArgs dmenu (menuArgs "Open") cmds
  forM_ sel spawn

defaultPanes :: Tall a
defaultPanes = Tall 1 0.03 0.5

tabbedWindow = tabbedBottom shrinkText tabTheme
isTerminal = ClassName "Xfce4-terminal"

flexLayout = windowNavigation $ combineTwo defaultPanes left right
             where right = tabbedWindow
                   left = tabbedWindow

defaultLayouts = avoidStruts defaultLayoutHook
    where defaultLayoutHook = noBorders (tabbedBottom shrinkText tabTheme) |||
                              noBorders Full |||
                              flexLayout

fullscreenLayouts =
    lessBorders OnlyFloat $ fullscreenFull Full

layoutRingPerWorkspace = onWorkspace "9" fullscreenLayouts defaultLayouts

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
    } `additionalKeysP` (extraKeys ++ Local.extraKeys)

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = def {
    ppOutput = dbusOutput dbus,
    ppTitle = pangoSanitize,
    ppCurrent = pangoColor "red" . wrap "[" "]" . pangoSanitize,
    ppVisible = pangoColor "blue" . wrap "(" ")". pangoSanitize,
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
