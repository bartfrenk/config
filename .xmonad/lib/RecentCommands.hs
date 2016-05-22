module RecentCommands (recentCommands) where


import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml (xmlParse)

import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))
import Text.HTML.TagSoup (fromTagText, parseTags)
import Data.Time (LocalTime, defaultTimeLocale, parseTimeOrError)
import Network.URI (URI, parseURI, unEscapeString)
import System.FilePath.Posix (splitPath)
import qualified Data.Map as M

-- | Creates a map of display labels to commands from an 'xbel' file.
recentCommands :: FilePath -> IO (M.Map String String)
recentCommands path = toMap . concatMap recentCommands' . bookmarks <$> doc path
  where toMap = M.fromList . fmap (displayLabel &&& shellCommand)

-- | Computes recently used commands from bookmark node.
recentCommands' :: Element i -> [RecentCommand]
recentCommands' bookmark = do
  arg <- bookmark `attrs` N "href"
  app <- apps bookmark
  name <- app `attrs` N "name"
  exec <- app `attrs` N "exec"
  modified <- app `attrs` N "modified"
  return $ RecentCommand (toString name) (sanitize exec) (toURI arg) (toLocalTime modified)
  where toLocalTime = parseTimeOrError True defaultTimeLocale "%FT%XZ" . show
        toURI = fromJust . parseURI . show
        toString = replaceHtmlEntities . show
        sanitize = takeWhile (/= ' ') . init . tail . toString

-- | Represents a single execution of a command on a specified file.
data RecentCommand = RecentCommand {
  rcName :: String,
  rcExec :: String,
  rcUri :: URI,
  rcModified :: LocalTime
} deriving (Eq, Show)

displayLabel :: RecentCommand -> String
displayLabel cmd = rcExec cmd ++ " " ++ shortArg
  where shortArg = let arg = unEscapeString (show $ rcUri cmd)
                   in last $ splitPath arg

shellCommand :: RecentCommand -> String
shellCommand cmd = rcExec cmd ++ " " ++ loc
  where loc = drop 3 . dropWhile (/= ':') $ unEscapeString (show $ rcUri cmd)

-- | Wraps 'xmlParse' in HaXml; parses an XML document without schema.
doc :: FilePath -> IO (Document Posn)
doc path = xmlParse path <$> readFile path

-- | Computes a list of XML nodes representing bookmarks.
bookmarks :: Document i -> [Element i]
bookmarks d = flip children (N "bookmark") $ root d
  where root (Document _ _ e _) = e

-- | Filters a node's children by a predicate.
selectChildren :: (Element i -> Bool) -> Element i -> [Element i]
selectChildren p (Elem _ _ cs) = catMaybes $ getElement <$> cs
  where getElement (CElem elt _) = if p elt then Just elt else Nothing
        getElement _ = Nothing

-- | Filters a node's children by a predicate on their name.
selectChildrenByName :: (QName -> Bool) -> Element i -> [Element i]
selectChildrenByName p = selectChildren (hasName p)
  where hasName q (Elem qname _ _) = q qname

-- | Filters children on exact name.
children :: Element i -> QName -> [Element i]
elt `children` qname = selectChildrenByName (== qname) elt
infixl 4 `children`

-- | Get all children of all elements with specified name.
(|>) :: [Element i] -> QName -> [Element i]
elts |> qname = elts >>= flip children qname
infixl 4 |>

-- | Get all attribute values whose name satisy a predicate.
selectAttrsByName :: (QName -> Bool) -> Element i -> [AttValue]
selectAttrsByName p (Elem _ as _) = snd <$> filter (p . fst) as

-- | Get all values of the attribute with specified name.
attrs :: Element i -> QName -> [AttValue]
elt `attrs` qname = selectAttrsByName (== qname) elt
infixl 4 `attrs`

-- | Get the nodes representing apps that opened the bookmark.
apps :: Element i -> [Element i]
apps bookmark = (:[]) bookmark |> N "info"
                               |> N "metadata"
                               |> N "bookmark:applications"
                               |> N "bookmark:application"

-- | Helper function to replace HTML entities (i.e. &apos;)
replaceHtmlEntities :: String -> String
replaceHtmlEntities = fromTagText . head . parseTags

