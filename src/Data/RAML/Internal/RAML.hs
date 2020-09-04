module Data.RAML.Parser where

import Data.Yaml (ParseException)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI.Template (TemplateEnv, URITemplate)
import Network.HTTP.Media.MediaType (MediaType)
import qualified Data.ByteString.Char8 as BC
import Data.Version (Version)
import Data.Aeson (Value)
import Text.Regex.TDFA (Regex)

data ParseError
    = UnrecognizedSpecVersion ByteString
    | YARMLParseException ParseException

data RAMLSpec = RAMLSpec
    { ramlTitle :: String
    , ramlVersion :: Maybe String
    , ramlDescription :: Maybe String
    , ramlBaseUri :: Maybe URITemplate
    , ramlBaseUriParams :: Maybe TemplateEnv
    , ramlProtocols :: Maybe Protocols
    , ramlMediaType :: Maybe [MediaType]
    , ramlDocumentation :: Maybe [UserDocumentation]
    , ramlTypes :: Maybe (Map String TypeDecl)
    , ramlTraits :: Map TraitName TraitDecl
    }

newtype Protocols = NonEmpty Protocols

data Protocol = ProtocolHTTP | ProtocolHTTPS

data UserDocumentation = UserDocumentation
    { userDocTitle :: String
    , userDocContent :: Markdown
    }

newtype Markdown = Markdown String

data TypeDecl = TypeDecl
    { typeDeclDefault :: Maybe Value
    , typeDeclType :: EitherOrNone TypeName TypeDecl
    , typeDeclExamples :: [Either RawExample Example]
    , typeDeclDisplayName :: Maybe String
    , typeDeclDescription :: Maybe String
    , typeDeclAnnotations :: Annotations
    , typeDeclFacets :: Facets
    , typeDeclXml :: XMLSerialization
    , typeDeclEnum :: Maybe (NonEmpty Value)
    }

type TypeName = String

type EitherOrNone a b = Maybe (Either a b)

newtype RawExample = RawExample Value

data Example = Example
    { exampleDisplayName :: Maybe String
    , exampleDescription :: Maybe Markdown
    , exampleAnnotations :: Annotations
    , exampleValue :: Value
    , exampleStrict :: Maybe Bool
    }

newtype Annotations = Annotations (Map String Value)

data Properties = Properties AdditionalProperties (Map PropertyName Property)

data Facets = Facets AdditionalProperties (Map PropertyName Property)

data Property
    = PropertyTypeDecl PropRequired TypeDecl
    | PropertyTypeName TypeName

data AdditionalProperties
    = AllowAdditionalProperties
    | DisallowAdditionalProperties

data PropRequired = IsRequired | NotRequired

data PropertyName
    = PropertyNameString PropertyNameString
    | PropertyNameRegex Regex
    | PropertyAllNotSpecified

data PropertyNameString = PropertyNameString
    { propertyNameHasQuestionMark :: Bool
    , propertyNameRawName :: String
    }

data XMLSerialization = XMLSerialization
    { xmlAttribute :: Maybe Bool
    , xmlWrapped :: Maybe Bool
    , xmlName :: Maybe String
    , xmlNamespace :: Maybe String
    , xmlPrefix :: Maybe String
    }

data TraitDecl = TraitDecl
    { traitUsage :: Maybe String
    , traitMethod :: Method
    }

data Method = Method
    { methodDisplayName :: Maybe String
    , methodDescription :: Maybe Markdown
    , methodAnnotations :: Annotations
    , methodQueryParameters :: Map QueryParamName Property
    }

newtype QueryParamName = QueryParamName String

decodeEither :: ByteString -> Either ParseError RAMLSpec
decodeEither x = if firstLine == "#%RAML 1.0"
    then decodeRest $ BC.unlines rest
    else Left $ UnrecognizedSpecVersion firstLine
    where (firstLine : rest) = BC.lines x

decodeEitherText :: Text -> Either ParseError RAMLSpec
decodeEitherText x = if firstLine == "#%RAML 1.0"
    then decodeRest $ T.unlines rest
    else Left $ UnrecognizedSpecVersion $ encodeUtf8 firstLine
    where (firstLine : rest) = T.lines x
