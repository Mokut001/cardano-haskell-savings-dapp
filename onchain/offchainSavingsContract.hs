{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SavingsContract where

import Prelude (String)
import Plutus.Contract
import PlutusTx.Prelude
import Ledger
import Ledger.Value
import Ledger.Constraints as Constraints
import SavingsValidator

data LockParams = LockParams
    { lpDeadline :: POSIXTime
    , lpAmount   :: Integer
    }

lockFunds :: LockParams -> Contract () Empty ContractError ()
lockFunds params = do
    pkh <- ownPubKeyHash

    let datum = SavingsDatum
            { sdOwner = pkh
            , sdDeadline = lpDeadline params
            }

        tx =
            Constraints.mustPayToTheScript datum
                (lovelaceValueOf $ lpAmount params)

    ledgerTx <- submitTxConstraints validator tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String "Funds successfully locked"

withdrawFunds :: Contract () Empty ContractError ()
withdrawFunds = do
    utxos <- utxosAt scriptAddress
    let tx = collectFromScript utxos ()
    ledgerTx <- submitTxConstraintsSpending validator utxos tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Funds withdrawn"

type SavingsSchema =
        Endpoint "lock" LockParams
    .\/ Endpoint "withdraw" ()

endpoints :: Contract () SavingsSchema ContractError ()
endpoints =
    awaitPromise
        ( endpoint @"lock" lockFunds
          `select`
          endpoint @"withdraw" (const withdrawFunds)
        )
