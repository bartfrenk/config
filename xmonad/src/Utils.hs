-- | Utility functions for use in my XMonad configuration

module Utils where

import XMonad (Query (), X)
import XMonad.Actions.GroupNavigation (nextMatchOrDo, Direction(..))
import XMonad.Util.Run (safeSpawn)

splitCommand :: String -> (Maybe String, [String])
splitCommand cmd = case words cmd of
  prog:args -> (Just prog, args)
  _ -> (Nothing, [])


cycleOrRaise :: String -> Query Bool -> X ()
cycleOrRaise command query =
  let (prog', args) = splitCommand command
  in case prog' of
    Nothing -> pure ()
    Just prog -> nextMatchOrDo History query $ safeSpawn prog args
