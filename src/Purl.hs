module Purl (
  parsePurl,
  purlText,
  Purl (..),
  Scheme (..),
  Protocol (..),
  Namespace (..),
  Name (..),
  Version (..),
  Qualifiers (..),
  Subpath (..),
) where

import Control.Monad (void)
import Data.Text (Text, concat, cons, snoc)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (lookAhead, try), ParseErrorBundle, Parsec, anySingle, manyTill, noneOf, optional, parse, some, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char)

{- |
this is the URL scheme with the constant value of "pkg".
One of the primary reason for this single scheme is to facilitate the future official registration of the "pkg" scheme for package URLs.
Required.
-}
newtype Scheme = Scheme {scheme :: Text} deriving stock (Show, Eq)

-- | the package "type" or package "protocol" such as maven, npm, nuget, gem, pypi, etc. Required.
newtype Protocol = Protocol {protocol :: Text} deriving stock (Show, Eq)

-- | some name prefix such as a Maven groupid, a Docker image owner, a GitHub user or organization. Optional and type-specific.
newtype Namespace = Namespace {namespace :: Text} deriving stock (Show, Eq)

-- | the name of the package. Required.
newtype Name = Name {name :: Text} deriving stock (Show, Eq)

-- | the version of the package. Optional.
newtype Version = Version {version :: Text} deriving stock (Show, Eq)

-- | extra qualifying data for a package such as an OS, architecture, a distro, etc. Optional and type-specific.
newtype Qualifiers = Qualifiers {qualifiers :: Text} deriving stock (Show, Eq)

-- | extra subpath within a package, relative to the package root. Optional.
newtype Subpath = Subpath {subpath :: Text} deriving stock (Show, Eq)

{- |
Components are designed such that they form a hierarchy from the most significant
component on the left to the least significant component on the right.

A purl must NOT contain a URL Authority i.e. there is no support for username, password, host and port components.
A namespace segment may sometimes look like a host but its interpretation is specific to a type.

url example: scheme:protocol/namespace/name@version?qualifiers#subpath
-}
data Purl = Purl
  { purlScheme :: Scheme
  , purlProtocol :: Protocol
  , purlNamespace :: Maybe Namespace
  , purlName :: Name
  , purlVersion :: Maybe Version
  , purlQualifiers :: Maybe Qualifiers
  , purlSubpath :: Maybe Subpath
  }
  deriving stock (Show, Eq)

type Parser = Parsec Void Text

schemeParser :: Parser Scheme
schemeParser = do
  s <- manyTill alphaNumChar ":"
  return (Scheme $ T.pack s)

protocolParser :: Parser Protocol
protocolParser = do
  p <- some (noneOf [':', '/'])
  return (Protocol $ T.pack p)

namespaceParser :: Parser (Maybe Namespace)
namespaceParser = do
  ns <- optional $ try $ manyTill anySingle (lookAhead $ char '/')
  case ns of
    Just n -> return (Just (Namespace $ T.pack n))
    Nothing -> return Nothing

nameParser :: Parser Name
nameParser = do
  n <- some (noneOf ['@', '?', '#'])
  return (Name $ T.pack n)

versionParser :: Parser (Maybe Version)
versionParser = do
  ver <- optional $ char '@' >> some (noneOf ['?', '#'])
  case ver of
    Just v -> return (Just (Version $ T.pack v))
    Nothing -> return Nothing

qualifiersParser :: Parser (Maybe Qualifiers)
qualifiersParser = do
  qual <- optional $ char '?' >> some (noneOf ['#'])
  case qual of
    Just q -> return (Just (Qualifiers $ T.pack q))
    Nothing -> return Nothing

subpathParser :: Parser (Maybe Subpath)
subpathParser = do
  sub <- optional $ char '#' >> some anySingle
  case sub of
    Just s -> return (Just (Subpath $ T.pack s))
    Nothing -> return Nothing

purlText :: Purl -> Text
purlText p =
  Data.Text.concat
    [ scheme
        (purlScheme p)
    , ":"
    , protocol
        (purlProtocol p)
    , "/"
    , nText
        (purlNamespace p)
    , name
        (purlName p)
    , vText
        (purlVersion p)
    , qText
        (purlQualifiers p)
    , sText
        (purlSubpath p)
    ]

sText :: Maybe Subpath -> Text
sText sub = case sub of
  Just s -> cons '#' (subpath s)
  Nothing -> ""

vText :: Maybe Version -> Text
vText ver = case ver of
  Just v -> cons '@' (version v)
  Nothing -> ""

nText :: Maybe Namespace -> Text
nText ns = case ns of
  Just n -> snoc (namespace n) '/'
  Nothing -> ""

qText :: Maybe Qualifiers -> Text
qText qual =
  case qual of
    Just q -> cons '?' (qualifiers q)
    Nothing -> ""

purl :: Parser Purl
purl = do
  s <- schemeParser <?> "scheme present"
  p <- protocolParser <?> "protocol"
  void $ char '/'
  ns <- namespaceParser <?> "namespace"
  void $ char '/'
  n <- nameParser <?> "name"
  v <- versionParser <?> "version"
  q <- qualifiersParser <?> "qualifiers"
  Purl s p ns n v q <$> subpathParser <?> "subpath"

parsePurl :: Text -> Either (ParseErrorBundle Text Void) Purl
parsePurl = parse purl "purl spec"