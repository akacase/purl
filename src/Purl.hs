module Purl (
  parsePurl,
  purlText,
  Package,
  Purl (..),
  Scheme (..),
  Namespace (..),
  Name (..),
  Version (..),
  Qualifiers (..),
  Subpath (..),
) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text, cons, snoc)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (lookAhead, try), ParseErrorBundle, Parsec, anySingle, choice, many, manyTill, noneOf, optional, parse, parseMaybe, satisfy, some, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, string)

{- |
Each package manager, platform, type, or ecosystem has its own conventions and protocols to identify, locate, and provision software packages.
The package type is the component of a package URL that is used to capture this information with a short string such as maven, npm, nuget, gem, pypi, etc.
These are known purl package type definitions.
Known purl type definitions are formalized here independent of the core Package URL specification. See also a candidate list [here]
(https://github.com/package-url/purl-spec/blob/master/PURL-TYPES.rst)
-}
data Package
  = ALPM
  | APK
  | BitBucket
  | CocoaPods
  | Cargo
  | Composer
  | Conan
  | Conda
  | Cran
  | Deb
  | Docker
  | Gem
  | Generic
  | GitHub
  | Golang
  | Hackage
  | Hex
  | HuggingFace
  | Maven
  | MLFlow
  | NPM
  | NuGet
  | QPKG
  | OCI
  | Pub
  | PyPI
  | RPM
  | Swid
  | Swift
  deriving stock (Show, Eq)

packages :: Parser Package
packages =
  choice
    [ ALPM <$ string "alpm"
    , APK <$ string "apk"
    , BitBucket <$ string "bitbucket"
    , CocoaPods <$ string "cocoapods"
    , Cargo <$ string "cargo"
    , Composer <$ string "composer"
    , Conan <$ string "conan"
    , Conda <$ string "conda"
    , Cran <$ string "cran"
    , Deb <$ string "deb"
    , Docker <$ string "docker"
    , Gem <$ string "gem"
    , Generic <$ string "generic"
    , GitHub <$ string "github"
    , Golang <$ string "golang"
    , Hackage <$ string "hackage"
    , Hex <$ string "hex"
    , HuggingFace <$ string "huggingface"
    , Maven <$ string "maven"
    , MLFlow <$ string "mlflow"
    , NPM <$ string "npm"
    , NuGet <$ string "nuget"
    , QPKG <$ string "qpkg"
    , OCI <$ string "oci"
    , Pub <$ string "pub"
    , PyPI <$ string "pypi"
    , RPM <$ string "rpm"
    , Swid <$ string "swid"
    , Swift <$ string "swift"
    ]

{- |
this is the URL scheme with the constant value of "pkg".
One of the primary reason for this single scheme is to facilitate the future official registration of the "pkg" scheme for package URLs.
Required.
-}
data Scheme = Pkg
  deriving stock (Show, Eq)

-- | some name prefix such as a Maven groupid, a Docker image owner, a GitHub user or organization. Optional and type-specific.
newtype Namespace = Namespace Text
  deriving stock (Eq)
  deriving (Show) via Text

-- | the name of the package. Required.
newtype Name = Name Text
  deriving stock (Eq)
  deriving (Show) via Text

-- | the version of the package. Optional.
newtype Version = Version Text
  deriving stock (Eq)
  deriving (Show) via Text

-- | extra qualifying data for a package such as an OS, architecture, a distro, etc. Optional and type-specific.
newtype Qualifiers = Qualifiers Text
  deriving stock (Eq)
  deriving (Show) via Text

-- | extra subpath within a package, relative to the package root. Optional.
newtype Subpath = Subpath Text
  deriving stock (Eq)
  deriving (Show) via Text

{- |
Components are designed such that they form a hierarchy from the most significant
component on the left to the least significant component on the right.

A purl must NOT contain a URL Authority i.e. there is no support for username, password, host and port components.
A namespace segment may sometimes look like a host but its interpretation is specific to a type.
-}
data Purl = Purl
  { purlScheme :: Scheme
  , purlPackage :: Package
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
  Pkg <$ string "pkg"

packageParser :: Parser Package
packageParser = do
  void $ satisfy (== ':') -- ':' must come after scheme
  void $ optional $ try $ some (char '/') -- strip per standard extraneous '/' after ':' if present
  start <- satisfy isAsciiLower -- Must start with a letter
  rest <- many (satisfy isValidChar) -- Followed by any number of valid characters
  let packType = T.pack (start : rest)
  case parseMaybe packages packType of
    Just p -> return p
    Nothing -> fail "Invalid package type"
  where
    isValidChar c = isAsciiLower c || isDigit c || c == '.' || c == '+' || c == '-'

namespaceParser :: Parser (Maybe Namespace)
namespaceParser = do
  ns <- optional $ try $ char '/' >> manyTill alphaNumChar (lookAhead $ char '/')
  case ns of
    Just n -> return (Just (Namespace $ T.pack n))
    Nothing -> return Nothing

nameParser :: Parser Name
nameParser = do
  void $ satisfy (== '/')
  n <- some (noneOf ['@', '?', '#', '/'])
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

{- | returns the Purl in the correct format:
  scheme:protocol\/namespace\/name\@version?qualifiers\#subpath
-}
purlText :: Purl -> Text
purlText p =
  T.concat
    [ unScheme
        (purlScheme p)
    , ":"
    , unPackage
        (purlPackage p)
    , "/"
    , unNamespace
        (purlNamespace p)
    , unName $ purlName p
    , unVersion
        (purlVersion p)
    , unQualifiers
        (purlQualifiers p)
    , unSubpath
        (purlSubpath p)
    ]

unName :: Name -> Text
unName (Name n) = n

unScheme :: Scheme -> Text
unScheme s = case s of
  Pkg -> "pkg"

unSubpath :: Maybe Subpath -> Text
unSubpath sub = case sub of
  Just (Subpath s) -> cons '#' s
  Nothing -> ""

unVersion :: Maybe Version -> Text
unVersion ver = case ver of
  Just (Version v) -> cons '@' v
  Nothing -> ""

unNamespace :: Maybe Namespace -> Text
unNamespace ns = case ns of
  Just (Namespace n) -> snoc n '/'
  Nothing -> ""

unQualifiers :: Maybe Qualifiers -> Text
unQualifiers qual =
  case qual of
    Just (Qualifiers q) -> cons '?' q
    Nothing -> ""

unPackage :: Package -> Text
unPackage p = case p of
  ALPM -> "alpm"
  APK -> "apk"
  BitBucket -> "bitbucket"
  CocoaPods -> "cocoapods"
  Cargo -> "cargo"
  Composer -> "composer"
  Conan -> "conan"
  Conda -> "conda"
  Cran -> "cran"
  Deb -> "deb"
  Docker -> "docker"
  Gem -> "gem"
  Generic -> "generic"
  GitHub -> "github"
  Golang -> "golang"
  Hackage -> "hackage"
  Hex -> "hex"
  HuggingFace -> "huggingface"
  Maven -> "maven"
  MLFlow -> "mlflow"
  NPM -> "npm"
  NuGet -> "nuget"
  QPKG -> "qpkg"
  OCI -> "oci"
  Pub -> "pub"
  PyPI -> "pypi"
  RPM -> "rpm"
  Swid -> "swid"
  Swift -> "swift"

purl :: Parser Purl
purl = do
  s <- schemeParser <?> "scheme present"
  p <- packageParser <?> "package"
  ns <- namespaceParser <?> "namespace"
  n <- nameParser <?> "name"
  v <- versionParser <?> "version"
  q <- qualifiersParser <?> "qualifiers"
  Purl s p ns n v q <$> subpathParser <?> "subpath"

-- | main parsing function
parsePurl :: Text -> Either (ParseErrorBundle Text Void) Purl
parsePurl = parse purl "purl spec"
