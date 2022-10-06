{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Write.TxSpec where

import Prelude

import Cardano.Api.Gen
    ( genScriptData, shrinkScriptData )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Cardano.Wallet.Write.Tx
    ( BinaryData
    , Datum
    , DatumHash
    , LatestLedgerEra
    , StandardBabbage
    , datumFromBytes
    , datumFromCardanoScriptData
    , datumHashFromBytes
    , datumHashToBytes
    , datumToBytes
    , datumToCardanoScriptData
    , isPlutusScript
    , scriptFromCardanoEnvelopeJSON
    )
import Cardano.Wallet.Write.Tx.Gen
    ( genData, genDatum, genDatumHash, shrinkData, shrinkDatum )
import Data.Aeson
    ( (.=) )
import Plutus.V1.Ledger.Api
    ( Data (..) )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), property, (===) )

import qualified Cardano.Api as Cardano
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

spec :: Spec
spec = do
    describe "Isomorphisms" $ do
        describe "Datum <-> Cardano.ScriptData" $ do
            it "datumFromCardanoScriptData . datumToCardanoScriptData == id" $
                property $ \x -> do
                    let f = datumFromCardanoScriptData . datumToCardanoScriptData
                    f x === x

            it "datumToCardanoScriptData . datumFromCardanoScriptData == id" $ do
                property $ \x -> do
                    let f = datumToCardanoScriptData . datumFromCardanoScriptData
                    f x === x

    describe "Roundtrips" $ do
        it "datumFromBytes . datumToBytes == Right"
            $ property $ \d -> do
                 let f = datumFromBytes . datumToBytes
                 f d === Right d
        it "datumHashFromBytes . datumHashToBytes == Just"
            $ property $ \h -> do
                 let f = datumHashFromBytes . datumHashToBytes
                 f h === Just h

        it "dataToBinaryData . binaryDataToData == id"
            $ property $ \(d :: BinaryData LatestLedgerEra) -> do
                 let f = Alonzo.dataToBinaryData . Alonzo.binaryDataToData
                 f d === d


    describe "Goldens" $ do
        describe "datumFromBytes" $ do
            let decodePlutusData hex =
                    Alonzo.getPlutusData . Alonzo.binaryDataToData
                    <$> datumFromBytes (unsafeFromHex hex)
            let shouldBeRight a b = a `shouldBe` Right b

            it "I 42" $
                (decodePlutusData "182a") `shouldBe` Right (I 42)

            it "Constr 0 []" $
                (decodePlutusData "D87980") `shouldBe` Right (Constr 0 [])

            it "Constr 1 [B \"hello\", B ..., Map ...]" $ do
                let longDatum =
                        "D87A834568656C6C6F5820859601DEB772672B933EF30D66609610\
                        \C928BCF116951A52F4B8698F34C1FC80A140A1401A001E8480"
                decodePlutusData longDatum `shouldBeRight`
                    Constr 1
                        [ B "hello"
                        , B "\133\150\SOH\222\183rg+\147>\243\rf`\150\DLE\201(\
                            \\188\241\SYN\149\SUBR\244\184i\143\&4\193\252\128"
                        , Map [(B "",Map [(B "",I 2000000)])]
                        ]

        it "scriptFromCardanoEnvelopeJSON" $ do
            case Aeson.parse scriptFromCardanoEnvelopeJSON plutusScriptV2Json of
                Aeson.Success s -> isPlutusScript s `shouldBe` True
                Aeson.Error e -> expectationFailure e

instance Arbitrary Cardano.ScriptData where
     arbitrary = genScriptData
     shrink = shrinkScriptData

instance Arbitrary (Datum StandardBabbage) where
     arbitrary = genDatum
     shrink = shrinkDatum

-- NOTE: This instance, and some of the other, could be imported from
-- "Test.Cardano.Ledger.Alonzo.Serialisation.Generators". We are however
-- interested in testing the generators we expose in
-- "Cardano.Wallet.Write.Tx.Gen".
instance Arbitrary (BinaryData StandardBabbage) where
     arbitrary = genData
     shrink = shrinkData

instance Arbitrary DatumHash where
     arbitrary = genDatumHash

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

plutusScriptV2Json :: Aeson.Value
plutusScriptV2Json = Aeson.object
    [ "type" .= Aeson.String "PlutusScriptV2"
    , "description" .= Aeson.String ""
    , "cborHex" .= Aeson.String
        "5908f85908f50100003232332232323233223232323233223232323232323232\
        \3232323232323232323232323232222323253353232330213253353302900148\
        \8100102a102b376600c660426a038666aa03a60462400246664002a04e002605\
        \066aa0584002664002a046902a19aa8159aa980d8900099aaa810004004991a8\
        \0091111111111100628009a80e199aa80e98118900091999000a813800981419\
        \aa8161000999000a811a40a866aa0566aa60362400266aaa040010012646a002\
        \444444444444016a00226a002440046666ae68cdc39aab9d5002480008cc8848\
        \cc00400c008c8c8c8c8c8c8c8c8c8c8c8c8c8cccd5cd19b8735573aa01890001\
        \1999999999999111111111110919999999999980080680600580500480400380\
        \300280200180119a80b80c1aba1500c33501701835742a01666a02e0326ae854\
        \028ccd5406dd7280d1aba150093335501b75ca0346ae854020cd405c080d5d0a\
        \803999aa80d810bad35742a00c6464646666ae68cdc39aab9d5002480008cc88\
        \48cc00400c008c8c8c8cccd5cd19b8735573aa00490001199109198008018011\
        \9a815bad35742a00460586ae84d5d1280111931901719ab9c02f02e02c135573\
        \ca00226ea8004d5d0a8011919191999ab9a3370e6aae75400920002332212330\
        \0100300233502b75a6ae854008c0b0d5d09aba2500223263202e33573805e05c\
        \05826aae7940044dd50009aba135744a004464c6405466ae700ac0a80a04d55c\
        \f280089baa00135742a00a66a02eeb8d5d0a802199aa80d80e90009aba150033\
        \335501b75c40026ae854008c07cd5d09aba2500223263202633573804e04c048\
        \26ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba2\
        \5001135744a00226ae8940044d5d1280089aab9e5001137540026ae854008c03\
        \cd5d09aba2500223263201833573803203002c202e264c6402e66ae712410350\
        \543500017135573ca00226ea80048d400488880088d40048800448c88c008dd6\
        \000990009aa811911999aab9f0012501f233501e30043574200460066ae88008\
        \0488c8c8cccd5cd19b8735573aa004900011991091980080180118051aba1500\
        \23005357426ae8940088c98c8048cd5ce00980900809aab9e500113754002464\
        \6464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c\
        \8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233\
        \500d012357426ae8940088c98c805ccd5ce00c00b80a89aab9e5001137540026\
        \ae854010ccd54021d728039aba150033232323333573466e1d40052004230253\
        \57426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573c\
        \a00846666ae68cdc3a801a400042444006464c6403266ae7006806405c058054\
        \4d55cea80089baa00135742a00466a012eb8d5d09aba25002232632013335738\
        \02802602226ae8940044d5d1280089aab9e500113754002266aa002eb9d68891\
        \19118011bab00132001355020223233335573e0044a03a466a03866442466002\
        \006004600c6aae754008c014d55cf280118021aba20030101357420022446464\
        \6666ae68cdc3a800a400046a02a600a6ae84d55cf280191999ab9a3370ea0049\
        \0011280a91931900819ab9c01101000e00d135573aa00226ea80048c8c8cccd5\
        \cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d4\
        \00920042321222230020053009357426aae7940108cccd5cd19b875003480088\
        \c848888c004014c01cd5d09aab9e500523333573466e1d401120002321222230\
        \03005375c6ae84d55cf280311931900819ab9c01101000e00d00c00b135573aa\
        \00226ea80048c8c8cccd5cd19b8735573aa00490001199109198008018011802\
        \9aba15002375a6ae84d5d1280111931900619ab9c00d00c00a135573ca00226e\
        \a80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028c\
        \d5ce00580500409baa001232323232323333573466e1d4005200c21222222200\
        \323333573466e1d4009200a21222222200423333573466e1d400d20082332212\
        \22222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a802\
        \2400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd\
        \5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426a\
        \e8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500\
        \b23333573466e1d401d2000232122222223005008300e357426aae7940308c98\
        \c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae\
        \7940084d55cf280089baa0012323232323333573466e1d400520022333222122\
        \333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab\
        \9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae7003\
        \40300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b\
        \875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a80124000\
        \46424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7\
        \540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea0\
        \0490011190911180180218031aba135573ca00846666ae68cdc3a801a4000424\
        \44004464c6401466ae7002c02802001c0184d55cea80089baa00123233335734\
        \66e1d40052002201623333573466e1d40092000201623263200633573800e00c\
        \00800626aae74dd5000a4c240029210350543100320013550112211225335001\
        \1500e22133500f3004002335530061200100400112533500121010100e112223\
        \3355003321233001225335002210031001002500425335300300113500600115\
        \0050011212230020031122001111222300330020012253350021001100a12375\
        \0002640026aa0124422444a66a00226a00644002442666a00a44004600800466\
        \6aa600e2400200a008002224400424424466002008006244a666a0042a666a00\
        \2200c4200c4200c42a666a004200c42666ae68cdd78010008040039080390a99\
        \9a801080310803909980300100090911180100211199ab9a3371e00400200800\
        \624400424400222446004002224646002002446600660040040021"
  ]