{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module DBusConfig (connectDBus, createLogHook) where

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Hooks.DynamicLog (def, wrap)
import Data.Maybe (fromJust, isJust)
import Control.Monad (void)
import Control.Monad.State
import XMonad.Core
import XMonad.StackSet (StackSet (..), Screen (..), Workspace (..))
import Data.Map (Map)
import qualified Data.Map as Map
import XMonad.Actions.PhysicalScreens
import Data.List (sort)
import Utils (sortWithKey)

busName :: String
busName = "org.xmonad.Log"

connectDBus :: IO D.Client
connectDBus = do
  dbus <- D.connectSession
  requestName dbus
  pure dbus

requestName :: D.Client -> IO ()
requestName dbus = void $ D.requestName dbus (D.busName_ busName) flags
  where flags = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

createLogHook :: D.Client -> X ()
createLogHook dbus = getScreenMapping >>= hook dbus
  where
    hook screenMapping = do
      statusLine <- gets (formatWindowSet screenMapping . windowset)
      liftIO $ dbusOutput dbus statusLine

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus = D.emit dbus . signalWithBody
  where
    signalWithBody s = baseSignal { D.signalBody = body s }

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
      sid' <- getScreen def physicalScreen
      case sid' of
        Nothing -> pure acc
        Just sid -> recur ((sid, physicalScreen) : acc) (i + 1)

formatWindowSet :: Map ScreenId PhysicalScreen -> WindowSet -> String
formatWindowSet toPhysicalScreen (StackSet { current, visible, hidden }) =
     unwords (fmap formatScreen visibleScreens <> fmap formatWorkspace hiddenWorkspaces)
  where
    visibleScreens = sortWithKey key  (extract True current : fmap (extract False) visible)
    extract isCurrent (Screen { workspace, screen }) =
      (tag workspace, Map.lookup screen toPhysicalScreen, isCurrent)
    key (_, y, _) = y

    formatScreen (workspaceId, _, True) = pangoColor "red" $ wrap "[" "]" workspaceId
    formatScreen (workspaceId, _, False) = pangoColor "white" $ wrap "[" "]" workspaceId

    hiddenWorkspaces = sort $ tag <$> filter (isJust . stack) hidden
    formatWorkspace workspaceId = pangoColor "#AAAAAA" $ wrap "[" "]" workspaceId

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\"" ++ " size=\"large\">"
    right = "</span>"
