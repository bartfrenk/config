{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}

import XMonad
  ( Default (def),
    Full (Full),
    Resize (Expand, Shrink),
    Tall (Tall),
    X,
    XConfig (..),
    className,
    composeAll,
    doFloat,
    mod4Mask,
    resource,
    sendMessage,
    title,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.GroupNavigation
  ( Direction (..),
    nextMatchOrDo,
    historyHook
  )
import XMonad.Actions.PhysicalScreens
  ( PhysicalScreen (P),
    sendToScreen,
    viewScreen,
  )
import XMonad.Config.Xfce (xfceConfig)
import XMonad.Hooks.ManageDocks
  ( Direction2D (D, L, R, U),
    ToggleStruts (ToggleStrut),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Tabbed
  ( Theme (..),
    shrinkText,
    tabbedBottom,
  )
import XMonad.Layout.WindowNavigation
  ( Navigate (Go, Move),
    windowNavigation,
  )
import XMonad.Layout.ZoomRow (ZoomMessage (Zoom))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.WindowProperties ()

import qualified Colors as C
import qualified Actions as A
import Utils (cycleOrRaise)


customKeys :: [(String, X ())]
customKeys =
  [ ("<XF86AudioLowerVolume>"  , A.decreaseVolume)
  , ("<XF86AudioRaiseVolume>"  , A.increaseVolume)
  , ("<XF86AudioMute>"         , A.mute)

  , ("C-M-b" , sendMessage $ ToggleStrut U)
  , ("C-M-=" , sendMessage $ Zoom (5 / 4))
  , ("C-M--" , sendMessage $ Zoom (4 / 5))
  , ("C-M-h" , sendMessage Shrink)
  , ("C-M-l" , sendMessage Expand)
  , ("M-l"   , sendMessage $ Go R)
  , ("M-h"   , sendMessage $ Go L)
  , ("M-j"   , sendMessage $ Go D)
  , ("M-k"   , sendMessage $ Go U)
  , ("M-S-l" , sendMessage $ Move R)
  , ("M-S-h" , sendMessage $ Move L)
  , ("M-S-w" , sendToScreen def (P 0))
  , ("M-S-e" , sendToScreen def (P 1))
  , ("M-S-r" , sendToScreen def (P 2))
  , ("M-w"   , viewScreen def (P 0))
  , ("M-e"   , viewScreen def (P 1))
  , ("M-r"   , viewScreen def (P 2))

  , ("M-S-q" , A.logout)
  , ("M-p"   , A.selectCommand)
  , ("M-g"   , A.selectGotoWindow)
  , ("M-b"   , A.selectBringWindow)
  , ("M-S-m" , cycleOrRaise "/home/bart/.local/bin/em" isEmacs)
  , ("M-S-n" , cycleOrRaise "xfce4-terminal -e tmux" isTerminal)
  , ("M-S-b" , cycleOrRaise "firefox" isFirefox)
  , ("M-S-f" , cycleOrRaise "thunar" isThunar)
  ]
  where
    isEmacs = className =? "Emacs"
    isThunar = resource =? "Thunar"
    isTerminal = className =? "Xfce4-terminal"
    isFirefox = className =? "firefox"


main :: IO ()
main = xmonad $ docks config
  where
    config = xfceConfig
      { borderWidth = 3
      , normalBorderColor = C.base02
      , focusedBorderColor = C.red
      , terminal = "/usr/bin/xfce4-terminal"
      , focusFollowsMouse = False
      , workspaces = map (show @Integer) [1 .. 9]
      , modMask = mod4Mask
      , logHook = historyHook
      , clickJustFocuses = False
      , manageHook = managePlacement <+> manageDocks <+> manageHook def
      , layoutHook = layouts
      , startupHook = setWMName "LG3D"
      } `additionalKeysP` customKeys

    layouts = avoidStruts $
        noBorders tabbedWindow ||| noBorders Full ||| flexLayout
    flexLayout = windowNavigation $
        combineTwo defaultPanes tabbedWindow tabbedWindow
    tabbedWindow = tabbedBottom shrinkText tabTheme
    defaultPanes = Tall 1 0.03 0.5

    tabTheme = def
      { fontName = "xft:inconsolata:size=9:antialias=true:hinting=true"
      , decoHeight = 28
      , activeTextColor = C.red
      , activeColor = C.base03
      , activeBorderColor = C.base02
      , inactiveTextColor = C.base0
      , inactiveColor = C.base02
      , inactiveBorderColor = C.base02
      }

    managePlacement = composeAll
      [ title =? "Save As" --> doFloat
      , title =? "Whisker Menu" --> doFloat
      ]
