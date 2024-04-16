{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ConfiguredPool
    ( ConfiguredPool (..)
    , configurePools
    )
where

import Prelude

import Cardano.Api
    ( AsType (..)
    , File (..)
    , HasTextEnvelope
    , Key (..)
    , SerialiseAsBech32
    , SerialiseAsCBOR (..)
    )
import Cardano.Binary
    ( FromCBOR (..)
    )
import Cardano.CLI.Types.Key
    ( VerificationKeyOrFile (..)
    , readVerificationKeyOrFile
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , NodePort (..)
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.BaseTypes
    ( Network (Testnet)
    , StrictMaybe (..)
    , textToUrl
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    , ShelleyGenesisStaking (sgsPools)
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , UnliftClusterM (..)
    , askNodeDir
    , askUnliftClusterM
    , traceClusterLog
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( faucetAmt
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    , RelDirOf
    , changeFileOf
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , NodeId (..)
    , setLoggingName
    )
import Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
import Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (NodeParams)
    )
import Cardano.Wallet.Launch.Cluster.Node.Process
    ( withCardanoNodeProcess
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (RunningNode)
    )
import Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( PoolMetadataServer (registerMetadataForPoolIndex, urlFromPoolIndex)
    )
import Cardano.Wallet.Launch.Cluster.PoolRecipe
    ( PoolRecipe (PoolRecipe, operatorKeys)
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
import Cardano.Wallet.Launch.Cluster.UnsafeInterval
    ( unsafeUnitInterval
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Lens
    ( over
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , asks
    )
import Control.Tracer
    ( traceWith
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.Foldable
    ( traverse_
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Data.Text
    ( Text
    )
import Data.Word.Odd
    ( Word31
    )
import GHC.TypeLits
    ( Symbol
    )
import Path
    ( fromAbsDir
    , fromAbsFile
    , parseRelDir
    , reldir
    , relfile
    , (</>)
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Codec.CBOR.Read as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ListMap as ListMap
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a
         . NodeParams
        -> (RunningNode -> ClusterM a)
        -> ClusterM a
    -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
    -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: ClusterM
            (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    , finalizeShelleyGenesisSetup :: RunningNode -> ClusterM ()
    -- ^ Submit any pool retirement certificate according to the 'recipe'
    -- on-chain.
    }

configurePools
    :: PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> ClusterM (NonEmpty ConfiguredPool)
configurePools metadataServer =
    traverse (configurePool metadataServer)

-- | Create a key pair for a node KES operational key
genKesKeyPair
    :: RelDirOf "node"
    -> ClusterM (AbsFileOf "kes-prv", AbsFileOf "kes-pub")
genKesKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let kesPrv = poolDir </> [relfile|kes.prv|]
    let kesPub = poolDir </> [relfile|kes.pub|]
    cli
        [ "node"
        , "key-gen-KES"
        , "--verification-key-file"
        , fromAbsFile kesPub
        , "--signing-key-file"
        , fromAbsFile kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair
    :: RelDirOf "node"
    -> ClusterM (AbsFileOf "vrf-prv", AbsFileOf "vrf-pub")
genVrfKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let vrfPrv = poolDir </> [relfile|vrf.prv|]
    let vrfPub = poolDir </> [relfile|vrf.pub|]
    cli
        [ "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , fromAbsFile vrfPub
        , "--signing-key-file"
        , fromAbsFile vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: RelDirOf "node"
    -> PoolRecipe
    -> ClusterM
        ( AbsFileOf "op-prv"
        , AbsFileOf "op-pub"
        , AbsFileOf "op-cnt"
        )
writeOperatorKeyPair nodeSegment recipe = do
    poolDir <- askNodeDir nodeSegment
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceClusterLog $ MsgGenOperatorKeyPair $ fromAbsDir poolDir

    let opPub = poolDir </> [relfile|op.pub|]
    let opPrv = poolDir </> [relfile|op.prv|]
    let opCount = poolDir </> [relfile|op.count|]

    liftIO $ do
        Aeson.encodeFile (fromAbsFile opPub) pub
        Aeson.encodeFile (fromAbsFile opPrv) prv
        Aeson.encodeFile (fromAbsFile opCount) count

    pure ( opPrv , opPub , opCount)

-- | Issue a node operational certificate
issueOpCert
    :: RelDirOf "node"
    -> AbsFileOf "kes-pub"
    -> AbsFileOf "op-prv"
    -> AbsFileOf "op-cnt"
    -> ClusterM (AbsFileOf "op-cert")
issueOpCert nodeSegment kesPub opPrv opCount = do
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> [relfile|op.cert|]
    cli
        [ "node"
        , "issue-op-cert"
        , "--kes-verification-key-file"
        , fromAbsFile kesPub
        , "--cold-signing-key-file"
        , fromAbsFile opPrv
        , "--operational-certificate-issue-counter-file"
        , fromAbsFile opCount
        , "--kes-period"
        , "0"
        , "--out-file"
        , fromAbsFile file
        ]
    pure file

-- | Create a stake address key pair
genStakeAddrKeyPair
    :: (AbsFileOf "stake-prv", AbsFileOf "stake-pub")
    -> ClusterM ()
genStakeAddrKeyPair (stakePrv, stakePub) = do
    Config{..} <- ask
    cli
        [ clusterEraToString cfgLastHardFork
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , fromAbsFile stakePub
        , "--signing-key-file"
        , fromAbsFile stakePrv
        ]

readFailVerificationKeyOrFile
    :: forall keyrole (s :: Symbol)
     . ( HasTextEnvelope (VerificationKey keyrole)
       , SerialiseAsBech32 (VerificationKey keyrole)
       )
    => AsType keyrole
    -> AbsFileOf s
    -> ClusterM (VerificationKey keyrole)
readFailVerificationKeyOrFile role op =
    liftIO
        $ either (error . show) id
            <$> readVerificationKeyOrFile
                role
                (VerificationKeyFilePath $ File $ fromAbsFile op)

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => AbsFileOf "op-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakePoolKey opPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => AbsFileOf "vrf-pub"
    -> ClusterM (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsVrfKey vrfPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => AbsFileOf "stake-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => AbsFileOf "stake-pub"
    -> ClusterM (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    pure
        $ Ledger.Addr
            Testnet
            (Ledger.KeyHashObj payKH)
            (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

preparePoolRetirement
    :: RelDirOf "node"
    -> [AbsFileOf "retirement-cert"]
    -> ClusterM (AbsFileOf "retirement-tx", AbsFileOf "faucet-prv")
preparePoolRetirement nodeSegment certs = do
    Config{..} <- ask
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> [relfile|tx.raw|]
    (faucetInput, faucetPrv) <- takeFaucet
    cli
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "400"
          , "--fee"
          , show faucetAmt
          , "--out-file"
          , fromAbsFile file
          ]
            ++ mconcat ((\cert -> ["--certificate-file", fromAbsFile cert]) <$> certs)

    pure (file, faucetPrv)

issuePoolRetirementCert
    :: RelDirOf "node"
    -> AbsFileOf "op-pub"
    -> Word31
    -> ClusterM (AbsFileOf "retirement-cert")
issuePoolRetirementCert nodeSegment opPub retirementEpoch = do
    lastHardFork <- asks cfgLastHardFork
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> [relfile|retirement.cert|]
    cli
        [ clusterEraToString lastHardFork
        , "stake-pool"
        , "deregistration-certificate"
        , "--cold-verification-key-file"
        , fromAbsFile opPub
        , "--epoch"
        , show retirementEpoch
        , "--out-file"
        , fromAbsFile file
        ]
    pure file

configurePool
    :: HasCallStack
    => PoolMetadataServer
    -> PoolRecipe
    -> ClusterM ConfiguredPool
configurePool metadataServer recipe = do
    UnliftClusterM withConfig Config{..} <- askUnliftClusterM

    let PoolRecipe pledgeAmt i mRetirementEpoch metadata _ _ = recipe
    liftIO $ registerMetadataForPoolIndex metadataServer i metadata
    -- Use pool-specific dir
    let name = "pool-" <> show i
        nodeId = PoolNode i
    nodeSegment <- parseRelDir name
    poolDir <- askNodeDir nodeSegment
    liftIO $ traceWith cfgTracer
        $ MsgInfo $ "Configuring " <> T.pack (show poolDir)
    liftIO $ createDirectoryIfMissing False $ fromAbsDir poolDir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair nodeSegment
    (kesPrv, kesPub) <- genKesKeyPair nodeSegment
    (opPrv, opPub, opCount) <- writeOperatorKeyPair nodeSegment recipe
    opCert <- issueOpCert nodeSegment kesPub opPrv opCount
    let ownerPub = poolDir </> [relfile|stake.pub|]
    let ownerPrv = poolDir </> [relfile|stake.prv|]
    genStakeAddrKeyPair (ownerPrv, ownerPub)

    metadataUrl <- liftIO $ T.pack <$> urlFromPoolIndex metadataServer i
    let metadataBytes = Aeson.encode metadata

        registerViaShelleyGenesis = do
            poolId <- stakePoolIdFromOperatorVerKey opPub
            vrf <- poolVrfFromFile vrfPub
            stakePubHash <- stakingKeyHashFromFile ownerPub
            pledgeAddr <- stakingAddrFromVkFile ownerPub

            let params =
                    Ledger.PoolParams
                        { ppId = poolId
                        , ppVrf = vrf
                        , ppPledge = Ledger.Coin $ intCast pledgeAmt
                        , ppCost = Ledger.Coin 0
                        , ppMargin = unsafeUnitInterval 0.1
                        , ppRewardAcnt =
                            Ledger.RewardAcnt Testnet
                                $ Ledger.KeyHashObj stakePubHash
                        , ppOwners = Set.fromList [stakePubHash]
                        , ppRelays = mempty
                        , ppMetadata =
                            SJust
                                $ Ledger.PoolMetadata
                                    ( fromMaybe (error "invalid url (too long)")
                                        $ textToUrl 128 metadataUrl
                                    )
                                    (blake2b256 (BL.toStrict metadataBytes))
                        }
            let updateStaking sgs =
                    sgs
                        { Ledger.sgsPools =
                            ListMap.ListMap [(poolId, params)] <> sgsPools sgs
                        , Ledger.sgsStake =
                            ListMap.fromList [(stakePubHash, poolId)]
                                <> Ledger.sgsStake sgs
                        }
            let poolSpecificFunds =
                    ListMap.fromList
                        [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]
            pure
                $ over #sgInitialFunds (poolSpecificFunds <>)
                    . over #sgStaking updateStaking

        finalizeShelleyGenesisSetup (RunningNode socket _ _) = do
            -- Here is our chance to respect the 'retirementEpoch' of
            -- the 'PoolRecipe'.
            --
            -- NOTE: We also submit the retirement cert in
            -- @registerViaTx@, but this seems to work regardless. (We
            -- do want to submit it here for the sake of babbage)
            let retire e = do
                    retCert <- issuePoolRetirementCert nodeSegment opPub e
                    (rawTx, faucetPrv) <-
                        preparePoolRetirement
                            nodeSegment
                            [retCert]
                    signAndSubmitTx
                        socket
                        (changeFileOf @"retirement-tx" @"tx-body" rawTx)
                        [ changeFileOf @"faucet-prv" @"signing-key" faucetPrv
                        , changeFileOf @"stake-prv" @"signing-key" ownerPrv
                        , changeFileOf @"op-prv" @"signing-key" opPrv
                        ]
                        "retirement cert"
            traverse_ retire mRetirementEpoch

        operatePool nodeParams action = do
            let NodeParams
                    genesisFiles
                    hardForks
                    (port, peers)
                    logCfg
                    nodeOutput
                    = nodeParams
            let logCfg' = setLoggingName name logCfg

            topology <- genTopology nodeSegment peers
            liftIO $ withStaticServer (fromAbsDir poolDir) $ \url -> do
                traceWith cfgTracer
                    $ MsgStartedStaticServer (fromAbsDir poolDir) url

                (nodeConfig, genesisData, vd) <-
                    withConfig
                        $ genNodeConfig
                            nodeSegment
                            (Tagged @"node-name" mempty)
                            genesisFiles
                            hardForks
                            logCfg'

                let cfg =
                        CardanoNodeConfig
                            { nodeDir = fromAbsDir poolDir
                            , nodeConfigFile = fromAbsFile nodeConfig
                            , nodeTopologyFile = fromAbsFile topology
                            , nodeDatabaseDir = fromAbsDir
                                $ poolDir </> [reldir|db|]
                            , nodeDlgCertFile = Nothing
                            , nodeSignKeyFile = Nothing
                            , nodeOpCertFile = Just $ fromAbsFile opCert
                            , nodeKesKeyFile = Just $ fromAbsFile kesPrv
                            , nodeVrfKeyFile = Just $ fromAbsFile vrfPrv
                            , nodePort = Just (NodePort port)
                            , nodeLoggingHostname = Just name
                            , nodeExecutable = Nothing
                            , nodeOutputFile = fromAbsFile <$> nodeOutput
                            }
                traceWith cfgTracer $ MsgInfo $ "Cardano node config: "
                    <> T.pack (show cfg)
                withConfig
                    $ withCardanoNodeProcess nodeId cfg
                    $ \socket -> action $ RunningNode socket genesisData vd
    pure ConfiguredPool{..}
