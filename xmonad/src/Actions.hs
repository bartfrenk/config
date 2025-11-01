module Actions
  ( notify
  , selectCommand
  , selectGotoWindow
  , selectBringWindow
  , logout
  , increaseVolume
  , decreaseVolume
  , mute
  ) where

import XMonad (MonadIO, spawn, Default (def), X)
import XMonad.Actions.WindowBringer (WindowBringerConfig(..), gotoMenuConfig, bringMenuConfig)


notify :: MonadIO m => Int -> String -> m ()
notify expireTime message = spawn ("notify-send --expire-time " <> show expireTime <> " " <> quote message)
  where
    quote :: String -> String
    quote s = "\"" ++ s ++ "\""

selectCommand :: MonadIO m => m ()
selectCommand = spawn $ "rofi" ++ " " ++ unwords args
  where
    args = ["sidebar-mode", "-show run", "-fn", "SourceCodePro-9", "-p", "Run", "-l", "6"]

wbConfig :: String -> WindowBringerConfig
wbConfig prompt = def
  { menuCommand = "/usr/bin/rofi",
    menuArgs = ["-dmenu", "no-custom", "-p", prompt]
  }

selectGotoWindow :: X ()
selectGotoWindow = gotoMenuConfig (wbConfig "Go")

selectBringWindow :: X ()
selectBringWindow = bringMenuConfig (wbConfig "Bring")

logout :: MonadIO m => m ()
logout = spawn "xfce4-session-logout"

increaseVolume :: MonadIO m => m ()
increaseVolume = setMasterAudio "10%+"

decreaseVolume :: MonadIO m => m ()
decreaseVolume = setMasterAudio "10%-"

mute :: MonadIO m => m ()
mute = setMasterAudio "1+ toggle"

setMasterAudio :: MonadIO m => String -> m ()
setMasterAudio cmd = spawn $ "amixer -D pulse set Master " ++ cmd
