-- | Utility functions for use in my XMonad configuration

module Utils where

import XMonad (Query (), X)
import XMonad.Actions.GroupNavigation (nextMatchOrDo, Direction(..))
import XMonad.Util.Run (safeSpawn)
import Data.List (sortBy)

splitCommand :: String -> (Maybe String, [String])
splitCommand cmd = case words cmd of
  prog:args -> (Just prog, args)
  _ -> (Nothing, [])

cycleOrRaise :: String -> Query Bool -> X ()
cycleOrRaise command query =
  let (prog', args) = splitCommand command
  in case prog' of
    Nothing -> pure ()
    Just prog -> nextMatchOrDo Backward query $ safeSpawn prog args

sortWithKey :: Ord b => (a -> b) -> [a] -> [a]
sortWithKey key = sortBy (\x y -> key x `compare` key y)
