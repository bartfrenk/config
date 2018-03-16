{-# LANGUAGE RecordWildCards #-}

module Track where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List.Split        (splitOn)

import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           XMonad.Util.Run        (runProcessWithInput)

newHandle :: MonadIO m => Settings -> IO (Handle m)
newHandle settings = do
  ref <- liftIO (newIORef Map.empty)
  resetCache settings ref
  return $
    Handle
    { list = do
        m <- liftIO $ readIORef ref
        resetCache settings ref
        return m
    , activate = activateIssue settings
    }

--  resetCache settings ref
resetCache :: MonadIO m => Settings -> IORef (Map String Issue) -> m ()
resetCache settings ref = void (liftIO $ forkIO $ reset)
  where
    reset = writeIORef ref =<< (toIssueMap <$> fetchIssues settings)

data Handle m = Handle
  { list     :: m (Map String Issue)
  , activate :: Issue -> m ()
  }

data Issue = Issue
  { issueId     :: String
  , storyPoints :: String
  , description :: String
  } deriving (Show)

data Settings = Settings
  { trackCommand :: String
  , listArgs     :: [String]
  }

defaultSettings :: Settings
defaultSettings =
  Settings {trackCommand = "/home/bart/.local/bin/track", listArgs = ["ls"]}

parseIssue :: String -> Maybe Issue
parseIssue s =
  let t = splitOn "\t" s
  in if (length t == 3)
       then Just $ Issue (t !! 0) (t !! 1) (t !! 2)
       else Nothing

activateIssue :: MonadIO m => Settings -> Issue -> m ()
activateIssue Settings {..} Issue {..} =
  void (runProcessWithInput trackCommand ["start", issueId] "")

fetchIssues :: MonadIO m => Settings -> m [Issue]
fetchIssues Settings {..} = do
  out <- runProcessWithInput trackCommand listArgs ""
  return $ catMaybes (parseIssue <$> lines out)

toIssueMap :: [Issue] -> Map String Issue
toIssueMap issues = Map.fromList (f <$> issues)
  where
    f i@Issue {..} = (issueId ++ "  " ++ description, i)
-- issueMap :: MonadIO m => m (Map String Issue)
-- issueMap = do
--   out <- liftIO $ readFile "/tmp/issues"
--   let issues = catMaybes (parseIssue <$> lines out)
--   return $ Map.fromList (f <$> issues)
--   where
--     f i@Issue {..} = (issueId ++ "  " ++ description, i)
