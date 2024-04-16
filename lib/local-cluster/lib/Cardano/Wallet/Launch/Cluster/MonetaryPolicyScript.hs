{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Launch.Cluster.MonetaryPolicyScript
    ( genMonetaryPolicyScript
    , writeMonetaryPolicyScriptFile
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader
    ( asks
    )
import Data.Aeson
    ( object
    , (.:)
    , (.=)
    )
import Data.Generics.Labels
    ()
import Data.Text
    ( Text
    )
import Path
    ( addExtension
    , parseRelFile
    , relfile
    , toFilePath
    , (</>)
    )
import UnliftIO.Exception
    ( throwString
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: ClusterM (String, (String, String))
genMonetaryPolicyScript = do
    outputDir <- asks cfgClusterDir
    let policyPub = outputDir </> [relfile|policy.pub|]
    let policyPrv = outputDir </> [relfile|policy.prv|]

    cli
        [ "address"
        , "key-gen"
        , "--verification-key-file"
        , toFilePath policyPub
        , "--signing-key-file"
        , toFilePath policyPrv
        ]
    skey <- liftIO $ T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <-
        cliLine
            [ "address"
            , "key-hash"
            , "--payment-verification-key-file"
            , toFilePath policyPub
            ]
    script <- writeMonetaryPolicyScriptFile vkeyHash
    policyId <-
        cliLine
            [ "transaction"
            , "policyid"
            , "--script-file"
            , toFilePath script
            ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: String
    -- ^ The script verification key hash
    -> ClusterM (AbsFileOf "policy-script")
    -- ^ Returns the filename written
writeMonetaryPolicyScriptFile keyHash = do
    outputDir <- asks cfgClusterDir
    keyHashFile <- parseRelFile keyHash >>= addExtension ".script"
    let scriptFile = outputDir </> keyHashFile
    liftIO
        $ Aeson.encodeFile (toFilePath scriptFile)
        $ object
            [ "type" .= Aeson.String "sig"
            , "keyHash" .= keyHash
            ]
    pure scriptFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: AbsFileOf x -> IO Text
readKeyFromFile f = do
    textView <-
        either throwString pure
            =<< Aeson.eitherDecodeFileStrict' (toFilePath f)
    either throwString pure
        $ Aeson.parseEither
            (Aeson.withObject "TextView" (.: "cborHex"))
            textView
