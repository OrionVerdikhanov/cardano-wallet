module Cardano.Wallet.Spec.Interpreters.Pure
    ( pureStory
    , PureStory
    ) where

import qualified Cardano.Wallet.Spec.Data.Mnemonic as Mnemonic
import qualified Data.Set as Set
import qualified Effectful.Error.Static as E

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , runAssertFailsFast
    )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , runQueryMock
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom
    , runRandomMock
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , recordTraceLog
    , runTracePure
    )
import Effectful
    ( Eff
    , runPureEff
    )
import Effectful.Fail
    ( Fail
    , runFail
    )
import Test.Syd
    ( TestDefM
    , expectationFailure
    , it
    )

type PureStory a =
    Eff
        [ FxQuery
        , FxRandom
        , FxAssert
        , FxTrace
        , Fail
        , E.Error SomeException
        ]
        a

pureStory :: String -> PureStory a -> TestDefM outers () ()
pureStory label story =
    it label do
        interpretStoryPure story & \case
            Left err -> expectationFailure (show err)
            Right (Left err) -> expectationFailure err
            Right (Right (_unit :: a, log)) -> recordTraceLog label log

interpretStoryPure
    :: PureStory a
    -> (Either SomeException (Either String (a, Seq Text)))
interpretStoryPure =
    runQueryMock Set.empty
        >>> runRandomMock (Mnemonic.fromWords $ "foo" :| ["bar", "baz"])
        >>> runAssertFailsFast
        >>> runTracePure
        >>> runFail
        >>> E.runErrorNoCallStack
        >>> runPureEff
