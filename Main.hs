{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.Text as RE
import Text.Regex.TDFA ((=~))

import qualified MyDOM as MyDOM
import qualified NewDOM as NewDOM
import qualified Text.XML as X hiding (parseLBS)
import qualified Text.XML.Cursor as X
import Text.XML.Cursor (($/), ($//), fromDocument, followingSibling, node, element, attribute, checkElement, attributeIs)
import qualified Text.HTML.DOM as H

import Data.Text.Encoding.Error (lenientDecode,strictDecode)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import Data.List (elemIndex)
import Text.Read (readMaybe)

import qualified Data.Map as M

import Data.Char (ord)

mkList :: a -> [a]
mkList a = [a]

toList (Just x) = [x]
toList _        = []

data Message = M { messageId_ :: Text,
                   parentId_  :: Text,
                   author_    :: Text,
                   body_      :: Text,
                   date_      :: Text,
                   title_     :: Text,
                   problemId_ :: Text,
                   dateYMDHMS_ :: Text,
                   path_      :: [Text],
                   depth_     :: Int
               }
  deriving (Show)

firstOr :: a -> [a] -> a
firstOr _ (a:_) = a
firstOr a _     = a

-- | function to assist in regular expression matching
firstMatch :: Text -> (Text -> a) -> Text -> Maybe a
firstMatch regex f text =
  case RE.getAllTextSubmatches $ text RE.=~ regex of
    (_:a:_) -> Just $ f a
    _       -> Nothing

elementWithClass e c = element e >=> attributeIs "class" c

divWithClass c = elementWithClass "div" c

-- find a div with a class immediately below a cursor
findDiv :: Text -> X.Cursor -> [ X.Cursor ]
findDiv cls = ($/ divWithClass cls)

-- | return an attribute for an Element
elementAttribute :: X.Name -> X.Element -> Maybe Text
elementAttribute attr (X.Element _ amap _) = M.lookup attr amap

-- | left-justify an Int
justifyInt :: Int -> Int -> T.Text
justifyInt width x =
  let s = show x
      n = length s
  in T.pack $ replicate (width-n) '0' ++ s

-- | Abbreviated month names
monthNames :: [T.Text]
monthNames = T.words "jan feb mar apr may jun jul aug sep oct nov dec"

parseInt :: T.Text -> Maybe Int
parseInt str = readMaybe (T.unpack str)

parseMonth :: T.Text -> Maybe Int
parseMonth str = fmap (+1) $ elemIndex (T.toLower str) monthNames

parseTime :: T.Text -> Maybe (Int,Int)
parseTime str =
  let ws = T.splitOn ":" str in
  case ws of
    (hhs:mms:_) -> do hh <- parseInt hhs
                      mm <- parseInt mms
                      return (hh,mm)
    _           -> Nothing

classContainsMessage = checkElement go
  where
    go element = maybe False hasMessage $ elementAttribute "class"  element
    hasMessage :: Text -> Bool
    hasMessage t = t =~ ("message " :: Text)

-- | parse a date like "8 Nov 2013 14:11" into "YYYY-MM-DD HH:MI:SS"
parseDate :: T.Text -> Maybe T.Text
parseDate str =
  let ws = T.words str
      td = justifyInt 2 in
  case ws of
    (mdays : months : years : hhmms : _) -> do
      mday <- parseInt mdays
      mnum <- parseMonth months
      year <- parseInt years
      (hh,mi) <- parseTime hhmms
      return $ justifyInt 4 year <> "-" <> td mnum <> "-" <> td mday <> " " <> td hh <> ":" <> td mi <> ":" <> "00"
    _ -> Nothing

parseHREF :: Text -> Maybe Text
parseHREF = firstMatch  "Show\\('([0-9]+)'" id

extractText :: Text -> X.Node -> Text
extractText brText (X.NodeElement e) = goElement e
  where
    goElement (X.Element eName _ eNodes) =
      if eName == "br"
        then brText
        else T.concat $ map (extractText brText) eNodes
extractText brText (X.NodeContent t) = t
extractText _ _               = ""

-- cursor points to a forum_shift_right div
extractMessage problemId parentid cursor =
  let boards = findDiv "board" cursor
      atags  = boards >>= ($/ element "a")
      title  = firstOr "" $ atags >>= foo
      msgid  = firstOr "" $ atags >>= (attribute "href" >=> (toList . parseHREF))
      date   = firstOr "" $ boards >>= ($/ element "span" >=> foo)
      date_ymdhms = fromMaybe "???" $ parseDate date
      author = firstOr "" $ boards >>= ($/ element "b" >=> foo)
      hidden = boards >>= (followingSibling >=> elementWithClass "div" "hidden_message")
      mbody  = firstOr "" $ hidden >>= ($/ element "div" >=> classContainsMessage >=> foo')

      foo =  mkList . extractText ""   . node -- extract text under an element (<br> -> "")
      foo' = mkList . extractText "\n" . node

      message = M msgid parentid author mbody date title problemId date_ymdhms [] 0
  in if null boards
       then Nothing
       else Just message

-- cursor must point to a forum_shift_right div
extractAllMessages problemId parentid cursor =
  case extractMessage problemId parentid cursor of
    Nothing -> []
    Just m  -> let boards = findDiv "board" cursor
                   responses = boards >>= (followingSibling >=> elementWithClass "div" "forum_shift_right")
               in m : concatMap (extractAllMessages problemId (messageId_ m)) responses

-- extract all message from a Document
messagesFromDoc problemId doc =
  let allThreads = (fromDocument doc) $// (elementWithClass "div" "hidden_thread")
      roots = allThreads >>= ($/ divWithClass "thread") >>= ($/ divWithClass "forum_shift_right")
  in concatMap (extractAllMessages problemId "(no parent)") roots

checkText :: T.Text -> Bool
checkText txt = all (\c -> ord c < 65500) $ T.unpack txt

-- | check all of the fields for any bad characters
checkMessage :: Message -> Bool
checkMessage m = all checkText [ author_ m, body_ m, title_ m, date_ m ]

test6 path = do
  bytes <- BS.readFile path
  return $ T.length $ decodeUtf8With strictDecode bytes

test7 path = do
  doc <- fmap MyDOM.parseLBS $ LBS.readFile path
  let msgs = messagesFromDoc "ZZZZ" doc
  putStrLn $ "Message count: " ++ show (length msgs)
  let go txt field = undefined
  forM_ msgs $ \m -> do
    T.putStrLn $ "checking message " <> (messageId_ m)
    if not $ checkMessage m
      then print m
      else return ()
  return ()

main1 = do
  doc <- fmap NewDOM.parseLBS $ LBS.readFile "1510-3.html" -- uses strictDecode
  let bytes = X.renderLBS X.def doc
  LBS.writeFile "./output1.html" bytes
  putStrLn "output written to file output1.html"

main1a = do
  bytes <- BS.readFile "1510-3.html"
  let doc = NewDOM.parseLBS (LBS.fromStrict bytes)
      output = X.renderLBS X.def doc
  LBS.writeFile "./output1a.html" output
  putStrLn "output written to file output1a.html"

main2 = do
  doc <- H.readFile "1510-3.html"  -- from Text.HTML.DOM - uses lenientDecode
  let bytes = X.renderLBS X.def doc
  LBS.writeFile "./output2.html" bytes
  putStrLn "output written to file output2.html"

main3 = do
  let path = "1510-3.html"
  bytes <- BS.readFile path
  let chars = decodeUtf8With strictDecode bytes
      spaceCount = length $ filter (==' ') $ T.unpack chars
  putStrLn $ "in file " <> path <> ", number of bytes: " <> (show $ BS.length bytes)
                        <> " chars: " <> show (T.length chars)
                        <> " spaces: " <> (show spaceCount)
main4 = do
  test7 "1510-3.html"

main = main1

