{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SavingsValidator where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude

data SavingsDatum = SavingsDatum
    { sdOwner    :: PubKeyHash
    , sdDeadline :: POSIXTime
    }

PlutusTx.makeIsDataIndexed ''SavingsDatum [('SavingsDatum, 0)]
PlutusTx.makeLift ''SavingsDatum

{-# INLINABLE mkSavingsValidator #-}
mkSavingsValidator :: SavingsDatum -> () -> ScriptContext -> Bool
mkSavingsValidator datum _ ctx =
    traceIfFalse "Owner signature missing" signedByOwner &&
    traceIfFalse "Deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner =
        txSignedBy info (sdOwner datum)

    deadlineReached :: Bool
    deadlineReached =
        contains (from $ sdDeadline datum) (txInfoValidRange info)

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkSavingsValidator ||])

validatorHash :: ValidatorHash
validatorHash = validatorHash validator

scriptAddress :: Address
scriptAddress = scriptHashAddress validatorHash
