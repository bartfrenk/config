{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DBusConfig (connectDBus, createLogHook) where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (void)
import Control.Monad.State (gets, liftIO)
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Utils (sortWithKey)
import XMonad (ScreenId, Window, WindowSet, X, XState (..), appName, runQuery)
import XMonad.Actions.PhysicalScreens (PhysicalScreen (..), getScreen)
import XMonad.Hooks.DynamicLog (def, wrap)
import XMonad.StackSet (Screen (..), Stack (..), StackSet (..), Workspace (..))

busName :: String
busName = "org.xmonad.Log"

connectDBus :: IO D.Client
connectDBus = do
  dbus <- D.connectSession
  requestName dbus
  pure dbus

requestName :: D.Client -> IO ()
requestName dbus = void $ D.requestName dbus (D.busName_ busName) flags
  where
    flags = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

currentWindow :: WindowSet -> Maybe Window
currentWindow StackSet {current} = focus <$> stack (workspace current)

createLogHook :: D.Client -> X ()
createLogHook dbus = getScreenMapping >>= hook
  where
    hook screenMapping = do
      ws <- gets windowset
      let statusLine = formatWindowSet screenMapping ws
      case currentWindow ws of
        Nothing -> liftIO $ dbusOutput dbus statusLine
        Just window -> do
          name <- runQuery appName window
          liftIO $ dbusOutput dbus $ statusLine <> "  " <> pangoColor "#AAAAAA" name

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus = D.emit dbus . signalWithBody
  where
    signalWithBody s = baseSignal {D.signalBody = body s}

    baseSignal = D.signal objectPath interfaceName memberName
    objectPath = fromJust (D.parseObjectPath "/org/xmonad/Log")
    interfaceName = fromJust (D.parseInterfaceName busName)
    memberName = fromJust $ D.parseMemberName "Update"

    body s = [D.toVariant ("<b>" ++ UTF8.decodeString s ++ "</b>")]

getScreenMapping :: X (Map ScreenId PhysicalScreen)
getScreenMapping = Map.fromList <$> recur [] 0
  where
    recur :: [(ScreenId, PhysicalScreen)] -> Int -> X [(ScreenId, PhysicalScreen)]
    recur acc i = do
      let physicalScreen = P i
      getScreen def physicalScreen >>= \case
        Nothing -> pure acc
        Just sid -> recur ((sid, physicalScreen) : acc) (i + 1)

formatWindowSet :: Map ScreenId PhysicalScreen -> WindowSet -> String
formatWindowSet toPhysicalScreen (StackSet {current, visible, hidden}) =
  unwords (fmap formatScreen visibleScreens <> fmap formatWorkspace hiddenWorkspaces)
  where
    visibleScreens = sortWithKey key (extract True current : fmap (extract False) visible)
    extract isCurrent (Screen {workspace, screen}) =
      (tag workspace, Map.lookup screen toPhysicalScreen, isCurrent)
    key (_, y, _) = y

    formatScreen (workspaceId, _, True) = pangoColor "red" $ wrap "[" "]" workspaceId
    formatScreen (workspaceId, _, False) = pangoColor "white" $ wrap "[" "]" workspaceId

    hiddenWorkspaces = sort $ tag <$> filter (isJust . stack) hidden
    formatWorkspace workspaceId = pangoColor "#AAAAAA" $ wrap "[" "]" workspaceId

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left = "<span foreground=\"" ++ fg ++ "\"" ++ " size=\"large\">"
    right = "</span>"
