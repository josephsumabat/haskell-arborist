{-# LANGUAGE TupleSections #-}

module Main where

import AST.Cast
import AST.Haskell qualified
import AST.Haskell qualified as AST
import AST.Traversal (getDeepestContainingLineCol)
import Arborist.Debug
import Arborist.Debug.Trace
import Arborist.Files
import Arborist.Haddock
import Arborist.ModGraph
import Arborist.Renamer (RenamePhase, renamePrg)
import Arborist.Renamer.GlobalEnv
import Arborist.Scope
import Arborist.Scope.Global
import Arborist.Scope.Types
import Control.Error (mapMaybe)
import Control.Monad (forM)
import Criterion.Main
import Criterion.Types (Config (..))
import Data.ByteString qualified as BS
import Data.Either
import Data.HashMap.Lazy qualified as Map
import Data.LineCol
import Data.LineColRange
import Data.List qualified as List
import Data.Maybe (catMaybes, fromJust)
import Data.Pos
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text
import Data.Time
import Debug.Trace
import GHC.IO (unsafeInterleaveIO)
import HaskellAnalyzer
import Hir.Parse
import Hir.Types qualified as Hir
import System.IO
import Text.Pretty.Simple
import UnliftIO (mapConcurrently)

main :: IO ()
main = do
  time "everything" $ do
    let srcWithLps =
          ( [ "../mercury-web-backend/src"
            , "../mercury-web-backend/local-packages/a-mercury-prelude/src"
            , "../mercury-web-backend/local-packages/bugsnag-mtl/src"
            , "../mercury-web-backend/local-packages/cache-variable/src"
            , "../mercury-web-backend/local-packages/chalk-http/src"
            , "../mercury-web-backend/local-packages/evolve-types/src"
            , "../mercury-web-backend/local-packages/fedwire-reference/src"
            , "../mercury-web-backend/local-packages/fixed-width-parser/src"
            , "../mercury-web-backend/local-packages/fixed-width-text/src"
            , "../mercury-web-backend/local-packages/hspec-flaky/src"
            , "../mercury-web-backend/local-packages/initialize-test-databases/src"
            , "../mercury-web-backend/local-packages/linear-api/src"
            , "../mercury-web-backend/local-packages/mercury-account-number/src"
            , "../mercury-web-backend/local-packages/mercury-ach-codec/src"
            , "../mercury-web-backend/local-packages/mercury-ach-ref/src"
            , "../mercury-web-backend/local-packages/mercury-aeson/src"
            , "../mercury-web-backend/local-packages/mercury-arbitrary/src"
            , "../mercury-web-backend/local-packages/mercury-banking-base/src"
            , "../mercury-web-backend/local-packages/mercury-banking/src"
            , "../mercury-web-backend/local-packages/mercury-benchmark/src"
            , "../mercury-web-backend/local-packages/mercury-casing/src"
            , "../mercury-web-backend/local-packages/mercury-conduit/src"
            , "../mercury-web-backend/local-packages/mercury-country/src"
            , "../mercury-web-backend/local-packages/mercury-decision-engine-core/src"
            , "../mercury-web-backend/local-packages/mercury-display/src"
            , "../mercury-web-backend/local-packages/mercury-dollar/src"
            , "../mercury-web-backend/local-packages/mercury-electronic-account-type/src"
            , "../mercury-web-backend/local-packages/mercury-eventlog/src"
            , "../mercury-web-backend/local-packages/mercury-extra/src"
            , "../mercury-web-backend/local-packages/mercury-firco/src"
            , "../mercury-web-backend/local-packages/mercury-fixed/src"
            , "../mercury-web-backend/local-packages/mercury-generics/src"
            , "../mercury-web-backend/local-packages/mercury-hspec-assertions/src"
            , "../mercury-web-backend/local-packages/mercury-jsonb/src"
            , "../mercury-web-backend/local-packages/mercury-ledger/src"
            , "../mercury-web-backend/local-packages/mercury-list-utils/src"
            , "../mercury-web-backend/local-packages/mercury-moat/src"
            , "../mercury-web-backend/local-packages/mercury-network-logging/src"
            , "../mercury-web-backend/local-packages/mercury-numeric/src"
            , "../mercury-web-backend/local-packages/mercury-partner-bank/src"
            , "../mercury-web-backend/local-packages/mercury-paste/src"
            , "../mercury-web-backend/local-packages/mercury-persistent/src"
            , "../mercury-web-backend/local-packages/mercury-redis/src"
            , "../mercury-web-backend/local-packages/mercury-refined/src"
            , "../mercury-web-backend/local-packages/mercury-risk-alerts/src"
            , "../mercury-web-backend/local-packages/mercury-risk-policy/src"
            , "../mercury-web-backend/local-packages/mercury-routing-number/src"
            , "../mercury-web-backend/local-packages/mercury-settings/src"
            , "../mercury-web-backend/local-packages/mercury-slack-class/src"
            , "../mercury-web-backend/local-packages/mercury-string-variants/src"
            , "../mercury-web-backend/local-packages/mercury-stubs-internal/src"
            , "../mercury-web-backend/local-packages/mercury-swift-code/src"
            , "../mercury-web-backend/local-packages/mercury-text/src"
            , "../mercury-web-backend/local-packages/mercury-time/src"
            , "../mercury-web-backend/local-packages/mercury-tracing/src"
            , "../mercury-web-backend/local-packages/mercury-transaction-amount/src"
            , "../mercury-web-backend/local-packages/mercury-transaction-kind/src"
            , "../mercury-web-backend/local-packages/mercury-treasury/src"
            , "../mercury-web-backend/local-packages/mercury-typescript/src"
            , "../mercury-web-backend/local-packages/mercury-us-states/src"
            , "../mercury-web-backend/local-packages/mercury-uuid/src"
            , "../mercury-web-backend/local-packages/openapi3-th/src"
            , "../mercury-web-backend/local-packages/raccoon/src"
            , "../mercury-web-backend/local-packages/this-or-that/src"
            , "../mercury-web-backend/local-packages/workflow/src"
            , "../mercury-web-backend/external-deps/aeson/src"
            , "../mercury-web-backend/external-deps/aeson-pretty"
            , "../mercury-web-backend/external-deps/aeson-typescript/src"
            , "../mercury-web-backend/external-deps/alfred-margaret/src"
            , "../mercury-web-backend/external-deps/algebraic-graphs/src"
            , "../mercury-web-backend/external-deps/amazonka/src"
            , "../mercury-web-backend/external-deps/amazonka-cloudwatch/src"
            , "../mercury-web-backend/external-deps/amazonka-core/src"
            , "../mercury-web-backend/external-deps/amazonka-ec2/src"
            , "../mercury-web-backend/external-deps/amazonka-kinesis/src"
            , "../mercury-web-backend/external-deps/amazonka-s3/src"
            , "../mercury-web-backend/external-deps/amazonka-sts/src"
            , "../mercury-web-backend/external-deps/annotated-exception/src"
            , "../mercury-web-backend/external-deps/ansi-terminal/src"
            , "../mercury-web-backend/external-deps/apex-edocs-auth/src"
            , "../mercury-web-backend/external-deps/asn1-encoding"
            , "../mercury-web-backend/external-deps/asn1-types"
            , "../mercury-web-backend/external-deps/async"
            , "../mercury-web-backend/external-deps/attoparsec"
            , "../mercury-web-backend/external-deps/attoparsec-binary"
            , "../mercury-web-backend/external-deps/authenticate-oauth/authenticate"
            , "../mercury-web-backend/external-deps/authenticate-oauth/authenticate-oauth"
            , "../mercury-web-backend/external-deps/auto-update"
            , "../mercury-web-backend/external-deps/base16-bytestring"
            , "../mercury-web-backend/external-deps/base58-bytestring/src"
            , "../mercury-web-backend/external-deps/base64-bytestring"
            , "../mercury-web-backend/external-deps/bifunctors/src"
            , "../mercury-web-backend/external-deps/blaze-markup/src"
            , "../mercury-web-backend/external-deps/bugsnag-haskell/src"
            , "../mercury-web-backend/external-deps/case-insensitive"
            , "../mercury-web-backend/external-deps/cassava/src"
            , "../mercury-web-backend/external-deps/cassava-th/src"
            , "../mercury-web-backend/external-deps/cereal/src"
            , "../mercury-web-backend/external-deps/charset/src"
            , "../mercury-web-backend/external-deps/chronos/src"
            , "../mercury-web-backend/external-deps/classy-prelude/src"
            , "../mercury-web-backend/external-deps/classy-prelude-conduit/src"
            , "../mercury-web-backend/external-deps/classy-prelude-yesod/src"
            , "../mercury-web-backend/external-deps/clientsession/src"
            , "../mercury-web-backend/external-deps/clock"
            , "../mercury-web-backend/external-deps/conduit/src"
            , "../mercury-web-backend/external-deps/conduit-extra"
            , "../mercury-web-backend/external-deps/constraints/src"
            , "../mercury-web-backend/external-deps/contra-tracer/src"
            , "../mercury-web-backend/external-deps/cookie"
            , "../mercury-web-backend/external-deps/country/src"
            , "../mercury-web-backend/external-deps/criterion"
            , "../mercury-web-backend/external-deps/criterion/examples"
            , "../mercury-web-backend/external-deps/crypton"
            , "../mercury-web-backend/external-deps/crypton-connection"
            , "../mercury-web-backend/external-deps/crypton-x509"
            , "../mercury-web-backend/external-deps/crypton-x509-store"
            , "../mercury-web-backend/external-deps/crypton-x509-system"
            , "../mercury-web-backend/external-deps/crypton-x509-validation"
            , "../mercury-web-backend/external-deps/cryptostore/src"
            , "../mercury-web-backend/external-deps/currency-codes/src"
            , "../mercury-web-backend/external-deps/data-default"
            , "../mercury-web-backend/external-deps/data-default-class"
            , "../mercury-web-backend/external-deps/defaultable-map/src"
            , "../mercury-web-backend/external-deps/dependent-map/src"
            , "../mercury-web-backend/external-deps/derive-has-field/src"
            , "../mercury-web-backend/external-deps/directory-tree"
            , "../mercury-web-backend/external-deps/discover-instances/src"
            , "../mercury-web-backend/external-deps/dlist"
            , "../mercury-web-backend/external-deps/dns"
            , "../mercury-web-backend/external-deps/domain-auth"
            , "../mercury-web-backend/external-deps/either/src"
            , "../mercury-web-backend/external-deps/ekg"
            , "../mercury-web-backend/external-deps/ekg-core"
            , "../mercury-web-backend/external-deps/errors"
            , "../mercury-web-backend/external-deps/esqueleto/src"
            , "../mercury-web-backend/external-deps/esqueleto-compat/src"
            , "../mercury-web-backend/external-deps/esqueleto-streaming/src"
            , "../mercury-web-backend/external-deps/EventLog/src"
            , "../mercury-web-backend/external-deps/extra/src"
            , "../mercury-web-backend/external-deps/fast-logger"
            , "../mercury-web-backend/external-deps/fast-tagsoup"
            , "../mercury-web-backend/external-deps/fgl"
            , "../mercury-web-backend/external-deps/file-embed"
            , "../mercury-web-backend/external-deps/foreign-store/src"
            , "../mercury-web-backend/external-deps/format-numbers/src"
            , "../mercury-web-backend/external-deps/generic-arbitrary/src"
            , "../mercury-web-backend/external-deps/generic-deriving/src"
            , "../mercury-web-backend/external-deps/generic-lens/src"
            , "../mercury-web-backend/external-deps/geos/src"
            , "../mercury-web-backend/external-deps/glob-imports/src"
            , "../mercury-web-backend/external-deps/gogol/src"
            , "../mercury-web-backend/external-deps/gogol-core/src"
            , "../mercury-web-backend/external-deps/graphula/src"
            , "../mercury-web-backend/external-deps/graphviz"
            , "../mercury-web-backend/external-deps/groups/src"
            , "../mercury-web-backend/external-deps/haikunator/src"
            , "../mercury-web-backend/external-deps/hashable/src"
            , "../mercury-web-backend/external-deps/hashtables/src"
            , "../mercury-web-backend/external-deps/hashtables/benchmark/src"
            , "../mercury-web-backend/external-deps/hashtables/test"
            , "../mercury-web-backend/external-deps/HaskellNet/src"
            , "../mercury-web-backend/external-deps/haskell-src-meta/src"
            , "../mercury-web-backend/external-deps/HaXml/src"
            , "../mercury-web-backend/external-deps/hedgehog/src"
            , "../mercury-web-backend/external-deps/hedgehog-classes/src"
            , "../mercury-web-backend/external-deps/hedgehog-quickcheck/src"
            , "../mercury-web-backend/external-deps/hedis/src"
            , "../mercury-web-backend/external-deps/hermes/src"
            , "../mercury-web-backend/external-deps/heterocephalus/src"
            , "../mercury-web-backend/external-deps/h-gpgme/src"
            , "../mercury-web-backend/external-deps/hiedb-plugin/src"
            , "../mercury-web-backend/external-deps/hoauth2/src"
            , "../mercury-web-backend/external-deps/hostname"
            , "../mercury-web-backend/external-deps/hotel-california/src"
            , "../mercury-web-backend/external-deps/hourglass"
            , "../mercury-web-backend/external-deps/hsemail/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-api/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-exporter-in-memory/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-exporter-otlp/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-cloudflare/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-hspec/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-http-client/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-persistent/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-postgresql-simple/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-wai/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-instrumentation-yesod/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-propagator-w3c/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-sdk/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-utils-exceptions/src"
            , "../mercury-web-backend/external-deps/hs-opentelemetry-vendor-honeycomb/src"
            , "../mercury-web-backend/external-deps/hspec/src"
            , "../mercury-web-backend/external-deps/hspec-api/src"
            , "../mercury-web-backend/external-deps/hspec-core/src"
            , "../mercury-web-backend/external-deps/hspec-expectations/src"
            , "../mercury-web-backend/external-deps/hspec-formatter-github/src"
            , "../mercury-web-backend/external-deps/hspec-golden/src"
            , "../mercury-web-backend/external-deps/hspec-hedgehog/src"
            , "../mercury-web-backend/external-deps/hspec-megaparsec"
            , "../mercury-web-backend/external-deps/hspec-yesod/src"
            , "../mercury-web-backend/external-deps/html-entities"
            , "../mercury-web-backend/external-deps/http-api-data/src"
            , "../mercury-web-backend/external-deps/http-client"
            , "../mercury-web-backend/external-deps/http-client-tls"
            , "../mercury-web-backend/external-deps/http-conduit"
            , "../mercury-web-backend/external-deps/http-media/src"
            , "../mercury-web-backend/external-deps/http-types"
            , "../mercury-web-backend/external-deps/HUnit/src"
            , "../mercury-web-backend/external-deps/iconv"
            , "../mercury-web-backend/external-deps/indexed-traversable/src"
            , "../mercury-web-backend/external-deps/insert-ordered-containers/src"
            , "../mercury-web-backend/external-deps/iproute"
            , "../mercury-web-backend/external-deps/jose/src"
            , "../mercury-web-backend/external-deps/jose-jwt"
            , "../mercury-web-backend/external-deps/kinesis-client/src"
            , "../mercury-web-backend/external-deps/launchdarkly-server-sdk/src"
            , "../mercury-web-backend/external-deps/lens/src"
            , "../mercury-web-backend/external-deps/lens-aeson/src"
            , "../mercury-web-backend/external-deps/lens/examples"
            , "../mercury-web-backend/external-deps/lens/lens-properties/src"
            , "../mercury-web-backend/external-deps/libphonenumber/src"
            , "../mercury-web-backend/external-deps/libssh2/libssh2/src"
            , "../mercury-web-backend/external-deps/libssh2/libssh2-conduit"
            , "../mercury-web-backend/external-deps/lucid/src"
            , "../mercury-web-backend/external-deps/math-functions"
            , "../mercury-web-backend/external-deps/megaparsec"
            , "../mercury-web-backend/external-deps/memory"
            , "../mercury-web-backend/external-deps/mercury-lint/src"
            , "../mercury-web-backend/external-deps/mime-mail"
            , "../mercury-web-backend/external-deps/mmorph/src"
            , "../mercury-web-backend/external-deps/moat/src"
            , "../mercury-web-backend/external-deps/moat/examples/readme"
            , "../mercury-web-backend/external-deps/modern-uri"
            , "../mercury-web-backend/external-deps/monad-logger"
            , "../mercury-web-backend/external-deps/monad-loops/src"
            , "../mercury-web-backend/external-deps/MonadRandom"
            , "../mercury-web-backend/external-deps/mono-traversable/src"
            , "../mercury-web-backend/external-deps/natural-transformation/src"
            , "../mercury-web-backend/external-deps/network"
            , "../mercury-web-backend/external-deps/network-uri"
            , "../mercury-web-backend/external-deps/nonempty-containers/src"
            , "../mercury-web-backend/external-deps/ofac-sanctions/src"
            , "../mercury-web-backend/external-deps/openai/src"
            , "../mercury-web-backend/external-deps/openapi3/src"
            , "../mercury-web-backend/external-deps/opentelemetry-plugin/src"
            , "../mercury-web-backend/external-deps/optparse-applicative/src"
            , "../mercury-web-backend/external-deps/optparse-generic/src"
            , "../mercury-web-backend/external-deps/optparse-th/src"
            , "../mercury-web-backend/external-deps/ordered-containers"
            , "../mercury-web-backend/external-deps/ory-hydra-client"
            , "../mercury-web-backend/external-deps/parallel"
            , "../mercury-web-backend/external-deps/parsec-xbrl-sec/src"
            , "../mercury-web-backend/external-deps/parser-combinators"
            , "../mercury-web-backend/external-deps/path-pieces"
            , "../mercury-web-backend/external-deps/pcre-heavy"
            , "../mercury-web-backend/external-deps/pcre-light"
            , "../mercury-web-backend/external-deps/persistent"
            , "../mercury-web-backend/external-deps/persistent-documentation/src"
            , "../mercury-web-backend/external-deps/persistent-iproute"
            , "../mercury-web-backend/external-deps/persistent-postgis/src"
            , "../mercury-web-backend/external-deps/persistent-postgresql"
            , "../mercury-web-backend/external-deps/persistent-qq/src"
            , "../mercury-web-backend/external-deps/persistent-query/src"
            , "../mercury-web-backend/external-deps/persistent-typed-db/src"
            , "../mercury-web-backend/external-deps/Plural"
            , "../mercury-web-backend/external-deps/port-utils/src"
            , "../mercury-web-backend/external-deps/postgresql-simple/src"
            , "../mercury-web-backend/external-deps/prairie/src"
            , "../mercury-web-backend/external-deps/pretty-show"
            , "../mercury-web-backend/external-deps/pretty-simple/src"
            , "../mercury-web-backend/external-deps/primitive"
            , "../mercury-web-backend/external-deps/prometheus/src"
            , "../mercury-web-backend/external-deps/prometheus-client/src"
            , "../mercury-web-backend/external-deps/prometheus-metrics-ghc/src"
            , "../mercury-web-backend/external-deps/push-notify-apn/src"
            , "../mercury-web-backend/external-deps/QuickCheck/src"
            , "../mercury-web-backend/external-deps/random/src"
            , "../mercury-web-backend/external-deps/refined/src"
            , "../mercury-web-backend/external-deps/replace-megaparsec/src"
            , "../mercury-web-backend/external-deps/require-callstack/src"
            , "../mercury-web-backend/external-deps/resource-pool/src"
            , "../mercury-web-backend/external-deps/resourcet"
            , "../mercury-web-backend/external-deps/retry/src"
            , "../mercury-web-backend/external-deps/rrule/src"
            , "../mercury-web-backend/external-deps/RSA/src"
            , "../mercury-web-backend/external-deps/safe"
            , "../mercury-web-backend/external-deps/safe-exceptions/src"
            , "../mercury-web-backend/external-deps/scientific/src"
            , "../mercury-web-backend/external-deps/semialign/src"
            , "../mercury-web-backend/external-deps/semigroupoids/src"
            , "../mercury-web-backend/external-deps/servant/src"
            , "../mercury-web-backend/external-deps/servant-client/src"
            , "../mercury-web-backend/external-deps/servant-client-core/src"
            , "../mercury-web-backend/external-deps/servant-openapi3/src"
            , "../mercury-web-backend/external-deps/servant-openapi3/example/src"
            , "../mercury-web-backend/external-deps/shakespeare"
            , "../mercury-web-backend/external-deps/singletons/src"
            , "../mercury-web-backend/external-deps/singletons-th/src"
            , "../mercury-web-backend/external-deps/slack-web/src"
            , "../mercury-web-backend/external-deps/some/src"
            , "../mercury-web-backend/external-deps/some-dict-of/src"
            , "../mercury-web-backend/external-deps/split/src"
            , "../mercury-web-backend/external-deps/splitmix/src"
            , "../mercury-web-backend/external-deps/sqlcommenter/src"
            , "../mercury-web-backend/external-deps/stm-chans/src"
            , "../mercury-web-backend/external-deps/stm-conduit"
            , "../mercury-web-backend/external-deps/streaming/src"
            , "../mercury-web-backend/external-deps/streamly-core/src"
            , "../mercury-web-backend/external-deps/strict/src"
            , "../mercury-web-backend/external-deps/string-conversions/src"
            , "../mercury-web-backend/external-deps/stringsearch"
            , "../mercury-web-backend/external-deps/string-variants/src"
            , "../mercury-web-backend/external-deps/tagged/src"
            , "../mercury-web-backend/external-deps/tagsoup/src"
            , "../mercury-web-backend/external-deps/tar"
            , "../mercury-web-backend/external-deps/temporal-codec-encryption/src"
            , "../mercury-web-backend/external-deps/temporal-sdk/src"
            , "../mercury-web-backend/external-deps/temporal-sdk-codec-server/src"
            , "../mercury-web-backend/external-deps/temporal-sdk-core/src"
            , "../mercury-web-backend/external-deps/temporal-sdk-optimal-codec/src"
            , "../mercury-web-backend/external-deps/temporary"
            , "../mercury-web-backend/external-deps/terminal-progress-bar/src"
            , "../mercury-web-backend/external-deps/terminal-size/src"
            , "../mercury-web-backend/external-deps/text-icu"
            , "../mercury-web-backend/external-deps/text-icu-translit"
            , "../mercury-web-backend/external-deps/text-manipulate"
            , "../mercury-web-backend/external-deps/text-metrics"
            , "../mercury-web-backend/external-deps/text-show/src"
            , "../mercury-web-backend/external-deps/th-abstraction/src"
            , "../mercury-web-backend/external-deps/these/src"
            , "../mercury-web-backend/external-deps/thread-utils-context/src"
            , "../mercury-web-backend/external-deps/time-manager"
            , "../mercury-web-backend/external-deps/tls"
            , "../mercury-web-backend/external-deps/transformers-base/src"
            , "../mercury-web-backend/external-deps/typed-process/src"
            , "../mercury-web-backend/external-deps/typerep-map/src"
            , "../mercury-web-backend/external-deps/tz"
            , "../mercury-web-backend/external-deps/ua-parser/src"
            , "../mercury-web-backend/external-deps/unicode-transforms"
            , "../mercury-web-backend/external-deps/unliftio/src"
            , "../mercury-web-backend/external-deps/unliftio-core/src"
            , "../mercury-web-backend/external-deps/unordered-containers"
            , "../mercury-web-backend/external-deps/uri-bytestring/src"
            , "../mercury-web-backend/external-deps/utf8-string"
            , "../mercury-web-backend/external-deps/utility-ht/src"
            , "../mercury-web-backend/external-deps/uuid/src"
            , "../mercury-web-backend/external-deps/uuid-divide/src"
            , "../mercury-web-backend/external-deps/validation/src"
            , "../mercury-web-backend/external-deps/vault/src"
            , "../mercury-web-backend/external-deps/vector/src"
            , "../mercury-web-backend/external-deps/versions"
            , "../mercury-web-backend/external-deps/vinyl"
            , "../mercury-web-backend/external-deps/wai"
            , "../mercury-web-backend/external-deps/wai-cors/src"
            , "../mercury-web-backend/external-deps/wai-extra"
            , "../mercury-web-backend/external-deps/wai-logger"
            , "../mercury-web-backend/external-deps/warp"
            , "../mercury-web-backend/external-deps/webauthn/src"
            , "../mercury-web-backend/external-deps/witch"
            , "../mercury-web-backend/external-deps/witherable/src"
            , "../mercury-web-backend/external-deps/xeno/src"
            , "../mercury-web-backend/external-deps/xenomorph/src"
            , "../mercury-web-backend/external-deps/xlsx/src"
            , "../mercury-web-backend/external-deps/xmlbf"
            , "../mercury-web-backend/external-deps/xmlbf-xeno"
            , "../mercury-web-backend/external-deps/yaml/src"
            , "../mercury-web-backend/external-deps/yesod"
            , "../mercury-web-backend/external-deps/yesod-core/src"
            , "../mercury-web-backend/external-deps/yesod-form"
            , "../mercury-web-backend/external-deps/yesod-persistent"
            , "../mercury-web-backend/external-deps/yesod-test"
            , "../mercury-web-backend/external-deps/zip-archive/src"
            , "../mercury-web-backend/external-deps/zlib"
            , "../mercury-web-backend/local-packages/x9-serialization/src"
            ]
          ) ::
            [String]
    modFileMap <- buildModuleFileMap srcWithLps
    let onlySrc = ["../mercury-web-backend/src"]
    let targetMod = parseModuleTextFromText "Handler.Expenses.Index"
        targetFile = maybe [] List.singleton (Map.lookup targetMod modFileMap)
    let src = srcWithLps

    hsFiles <- getAllHsFiles src
    -- mapM_ putStrLn hsFiles

    -- allPrgs <- lazyGetPrgs hsFiles
    justTarget <- lazyGetPrgs targetFile
    let Just target = Map.lookup targetMod justTarget
    requiredPrograms <- time "gather" $ gatherScopeDeps Map.empty target modFileMap (Just 2)
    let exportIdx2 = getExportedNames requiredPrograms Map.empty (parseModuleTextFromText "Mercury.Database.Monad.Sql")
    -- let glblAvail = getGlobalAvailableNames requiredPrograms Map.empty (fromJust $ Map.lookup (parseModuleTextFromText "Handler.User") requiredPrograms)
    -- let renameTree = renamePrg allPrgs Map.empty target
    let renameTree = renamePrg requiredPrograms Map.empty target
    let debugTreeStr = fromJust $ (debugTree . (.dynNode)) <$> renameTree
    let loc = point (LineCol (mkPos 87) (mkPos 29))
    let chosenNode = (getDeepestContainingLineCol @(AST.Variable RenamePhase) loc) . (.dynNode) =<< renameTree
    -- traceShowM ((Map.lookup (parseModuleTextFromText "Mercury.Persistent.Operation") requiredPrograms))
    -- traceShowM (length (Map.keys allPrgs))
    -- traceM $ "exports"
    -- traceM $ Text.unpack . pShowNoColor $ (fst exportIdx2)
    -- traceM $ Text.unpack . pShowNoColor $ availableNamesToScope $ (filter (\g -> g.originatingMod == parseModuleTextFromText "Import.Handler") glblAvail)

    -- benchmarkMain modIndex debugAllTreeStr prgs

    -- _ <- putStrLn debugTreeStr

    time "writefiles" $
      ( withFile "myFile.txt" AppendMode $ \h -> do
          hSetEncoding h utf8 -- Set UTF-8 encoding
          -- hPutStrLn h (Text.unpack (pShowNoColor (head prgs)))
          -- hPutStrLn h (Text.unpack $ pShowNoColor (getAvailableNames requiredPrograms (last prgs)))
          hPutStrLn h (debugTreeStr)
          hPutStrLn h (Text.unpack . pShowNoColor $ chosenNode)
      )

lazyGetPrgs :: [FilePath] -> IO ProgramIndex
lazyGetPrgs hsFiles = do
  -- traceShowM hsFiles
  entries <- forM hsFiles $ \file -> do
    prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
    pure (file, prog)
  pure $ Map.fromList $ mapMaybe (\(_, prg) -> (,prg) <$> prg.mod) entries

parseFile :: [Char] -> IO Hir.Program
parseFile file = do
  -- traceShowM $ "parsing: " <> file
  fileContents <- fmap TE.decodeUtf8 . BS.readFile $ file
  pure $ snd (parsePrg fileContents) -- Run `test` function

getPrgs :: [FilePath] -> IO [Hir.Program]
getPrgs hsFiles =
  forM hsFiles $ \file -> do
    fileContents <- Text.readFile file
    let v = parsePrg fileContents
    pure $ snd v

getParPrgs :: [FilePath] -> IO [Hir.Program]
getParPrgs hsFiles = mapConcurrently processFile hsFiles
 where
  processFile file = do
    fileContents <- Text.readFile file -- Read as ByteString (fastest method)
    pure $ snd (parsePrg fileContents) -- Run `test` function

benchmarkMain modIndex debugAllTreeStr prgs =
  defaultMainWith
    (defaultConfig {resamples = 1})
    [ bench "mem" $ nf ((fmap (.name)) . findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR") modIndex
    , bench "rename" $ nf debugAllTreeStr ()
    ]

config :: Config
config =
  defaultConfig
    { resamples = 1
    }
