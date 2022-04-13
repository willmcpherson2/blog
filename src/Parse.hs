module Parse
  ( Document (..),
    Date (..),
    Title (..),
    Block (..),
    Element (..),
    Error (..),
    parse,
  )
where

import Combinators
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
import Data.Time (Day, MonthOfYear)
import Data.Time.Calendar (fromGregorianValid)
import Parser (Parser)
import Parser qualified as P
import Stream (Stream (takeToken))
import Text.Read (readMaybe)

data Document = Document
  { date :: Date
  , title :: Title
  , blocks :: [Block]
  }
  deriving (Show, Eq)

instance Ord Document where
  compare x y = compare (date x) (date y)

data Date
  = Date Day
  | DateError Error
  deriving (Show, Eq)

instance Ord Date where
  compare x y = case (x, y) of
    (Date x, Date y) -> compare x y
    _ -> EQ

data Title
  = Title (NonEmpty Char)
  | TitleError Error
  deriving (Show, Eq)

data Block
  = Heading (NonEmpty Char)
  | Subheading (NonEmpty Char)
  | Code (NonEmpty Char)
  | Paragraph (NonEmpty Element)
  | BlockError Error
  deriving (Show, Eq)

data Element
  = Bold (NonEmpty Element)
  | Italics (NonEmpty Element)
  | Link (NonEmpty Element) (NonEmpty Char)
  | InlineCode (NonEmpty Char)
  | Plain (NonEmpty Char)
  | ElementError Error
  deriving (Show, Eq)

data Error
  = InvalidDay
  | InvalidMonth
  | InvalidYear
  | InvalidDate
  | EmptyTitle
  | EmptySubheading
  | EmptyHeading
  | EmptyCode
  | EmptyBold
  | EmptyItalics
  | EmptyCaption
  | EmptyLink
  | EmptyInlineCode
  | UnclosedCode
  | UnclosedBold
  | UnclosedItalics
  | UnclosedCaption
  | UnclosedLink
  | UnclosedInlineCode
  | ExpectedLink
  deriving (Show, Eq)

parse :: String -> Document
parse = P.parse parseDocument

parseDocument :: Parser String Document
parseDocument = do
  date <- parseDate
  skipSpaces
  title <- parseTitle
  skipSpaces
  blocks <- parseBlocks
  pure Document{date, title, blocks}

parseDate :: Parser String Date
parseDate = fmap (either DateError Date) . runExceptT $ do
  day <- ExceptT $ InvalidDay <<! readMaybe <$> takeToken `uptoIncluding` matchM ' '
  month <- ExceptT $ InvalidMonth <<! monthMap <$> takeToken `uptoIncluding` matchM ' '
  year <- ExceptT $ InvalidYear <<! readMaybe <$> takeToken `uptoIncluding` matchM '\n'
  ExceptT $ InvalidDate <<! pure (fromGregorianValid year month day)

monthMap :: String -> Maybe MonthOfYear
monthMap = \case
  "Jan" -> Just 1
  "Feb" -> Just 2
  "Mar" -> Just 3
  "Apr" -> Just 4
  "May" -> Just 5
  "Jun" -> Just 6
  "Jul" -> Just 7
  "Aug" -> Just 8
  "Sep" -> Just 9
  "Oct" -> Just 10
  "Nov" -> Just 11
  "Dec" -> Just 12
  _ -> Nothing

parseTitle :: Parser String Title
parseTitle = fmap (either TitleError Title) . runExceptT $ do
  ExceptT $ EmptyTitle <<! nonEmpty <$> takeToken `uptoIncluding` matchM '\n'

parseBlocks :: Parser String [Block]
parseBlocks = star $ do
  block <- parseBlock
  skipSpaces
  pure block

parseBlock :: Parser String (Maybe Block)
parseBlock =
  try parseSubheading
    <<|>> try parseHeading
    <<|>> try parseCode
    <<|>> parseParagraph

parseSubheading :: Parser String (Maybe Block)
parseSubheading = runMaybeT . fmap (either BlockError Subheading) . runExceptT $ do
  lift $ MaybeT $ matchesM "##"
  lift $ lift skipSpaces
  ExceptT $ lift $ EmptySubheading <<! nonEmpty <$> takeToken `uptoIncluding` matchM '\n'

parseHeading :: Parser String (Maybe Block)
parseHeading = runMaybeT . fmap (either BlockError Heading) . runExceptT $ do
  lift $ MaybeT $ matchM '#'
  lift $ lift skipSpaces
  ExceptT $ lift $ EmptyHeading <<! nonEmpty <$> takeToken `uptoIncluding` matchM '\n'

parseCode :: Parser String (Maybe Block)
parseCode = runMaybeT . fmap (either BlockError Code) . runExceptT $ do
  lift $ MaybeT $ matchesM "``\n"
  code <- ExceptT $ lift $ UnclosedCode <<! takeToken `endWith` matchesM "\n``"
  ExceptT $ lift $ EmptyCode <<! pure (nonEmpty code)

parseParagraph :: Parser String (Maybe Block)
parseParagraph = runMaybeT $ do
  s <- MaybeT $ nonEmpty <$> takeToken `uptoIncluding` matchesM "\n\n"
  elements <- MaybeT $ pure (P.parse parseElements (toList s))
  pure $ Paragraph elements

parseElements :: Parser String (Maybe (NonEmpty Element))
parseElements = plus parseElement

parseElement :: Parser String (Maybe Element)
parseElement =
  try parseBold
    <<|>> try parseItalics
    <<|>> try parseLink
    <<|>> try parseInlineCode
    <<|>> parsePlain

parseBold :: Parser String (Maybe Element)
parseBold = runMaybeT . fmap (either ElementError Bold) . runExceptT $ do
  lift $ MaybeT $ matchM '*'
  s <- ExceptT $ lift $ UnclosedBold <<! takeToken `endWith` matchM '*'
  ExceptT $ lift $ EmptyBold <<! pure (P.parse parseElements s)

parseItalics :: Parser String (Maybe Element)
parseItalics = runMaybeT . fmap (either ElementError Italics) . runExceptT $ do
  lift $ MaybeT $ matchM '_'
  s <- ExceptT $ lift $ UnclosedItalics <<! takeToken `endWith` matchM '_'
  ExceptT $ lift $ EmptyItalics <<! pure (P.parse parseElements s)

parseLink :: Parser String (Maybe Element)
parseLink = runMaybeT . fmap (either ElementError id) . runExceptT $ do
  lift $ MaybeT $ matchM '['
  caption <- ExceptT $ lift $ UnclosedCaption <<! takeToken `endWith` matchM ']'
  caption' <- ExceptT $ lift $ EmptyCaption <<! pure (P.parse parseElements caption)
  ExceptT $ lift $ ExpectedLink <<! matchM '('
  link <- ExceptT $ lift $ UnclosedLink <<! takeToken `endWith` matchM ')'
  link' <- ExceptT $ lift $ EmptyLink <<! pure (nonEmpty link)
  pure $ Link caption' link'

parseInlineCode :: Parser String (Maybe Element)
parseInlineCode = runMaybeT . fmap (either ElementError InlineCode) . runExceptT $ do
  lift $ MaybeT $ matchM '`'
  s <- ExceptT $ lift $ UnclosedInlineCode <<! takeToken `endWith` matchM '`'
  ExceptT $ lift $ EmptyInlineCode <<! pure (nonEmpty s)

parsePlain :: Parser String (Maybe Element)
parsePlain = runMaybeT $ do
  ch <- MaybeT takeToken
  s <-
    lift . star . try $
      parseElement <&> \case
        Just (Plain s) -> Just s
        _ -> Nothing
  pure $ Plain $ foldl (<>) (ch :| []) s

skipSpaces :: Parser String ()
skipSpaces = void $ star $ try $ satisfyM isSpace
