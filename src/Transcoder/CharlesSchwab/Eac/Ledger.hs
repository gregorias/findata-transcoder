{-# OPTIONS_GHC -Wno-typed-holes #-}

module Transcoder.CharlesSchwab.Eac.Ledger (
  eacHistoryToLedger,
) where

import Hledger (Status (Cleared, Pending), Transaction)
import Hledger.Data.Extra (
  Comment (Comment, NoComment),
  ToAmount (toAmount),
  makeCommodityAmount,
  makePosting,
  makeTransaction,
 )
import Relude
import Transcoder.CharlesSchwab.Eac.Data (Record (..))
import Transcoder.CharlesSchwab.Eac.Data qualified as Eac
import Transcoder.Wallet (equity, financialServices, investmentAssets, liquidAssets, (<:>))

recordToTransaction :: Record -> Either Text Transaction
recordToTransaction (RecordWireTransfer wt) = wireTransferToTransaction wt
recordToTransaction (RecordSale s) = saleToTransaction s
recordToTransaction (RecordDeposit d) = depositToTransaction d

wireTransferToTransaction :: Eac.WireTransfer -> Either Text Transaction
wireTransferToTransaction
  ( Eac.WireTransfer
      { wtDescription = _wtDescription
      , wtAmount = wtAmount
      , wtSymbol = _wtSymbol
      , wtDate = wtDate
      }
    ) =
    return
      $ makeTransaction
        wtDate
        (Just Cleared)
        "Wire Transfer"
        [ makePosting
            Nothing
            (liquidAssets <:> "Charles Schwab:EAC:USD")
            (Just $ toAmount wtAmount)
            NoComment
        , makePosting (Just Pending) "Todo" Nothing NoComment
        ]

saleToTransaction :: Eac.Sale -> Either Text Transaction
saleToTransaction
  ( Eac.Sale
      { sSymbol = sSymbol
      , sSubsales = _sSubsales
      , sQuantity = sQuantity
      , sFeesAndCommissions = sFeesAndCommissions
      , sDescription = _sDescription
      , sDate = sDate
      , sAmount = sAmount
      }
    ) =
    return
      $ makeTransaction
        sDate
        (Just Cleared)
        (sSymbol <> " Sale")
        [ makePosting
            Nothing
            (investmentAssets <:> "Charles Schwab:EAC")
            (Just (makeCommodityAmount sSymbol (negate sQuantity)))
            NoComment
        , makePosting
            Nothing
            (liquidAssets <:> "Charles Schwab:EAC:USD")
            (Just $ toAmount sAmount)
            NoComment
        , makePosting
            Nothing
            financialServices
            (Just $ toAmount sFeesAndCommissions)
            NoComment
        ]

depositToTransaction :: Eac.Deposit -> Either Text Transaction
depositToTransaction
  ( Eac.Deposit
      { dQuantity = dQuantity
      , dDescription = _dDescription
      , dDepositAwardInfo = _dDepositAwardInfo
      , dSymbol = dSymbol
      , dDate = dDate
      }
    ) =
    return
      $ makeTransaction
        dDate
        (Just Cleared)
        (dSymbol <> " Deposit")
        [ makePosting
            Nothing
            (investmentAssets <:> "Charles Schwab:EAC")
            (Just (makeCommodityAmount dSymbol dQuantity))
            NoComment
        , makePosting
            (Just Pending)
            (equity <:> "Charles Schwab:Unvested GOOG")
            (Just (makeCommodityAmount dSymbol (negate dQuantity)))
            (Comment "TODO: Check state on https://client.schwab.com/app/accounts/equityawards/#/ and add a balance assertion")
        , makePosting
            Nothing
            (equity <:> "Charles Schwab:Unvested GOOG Withholding Tax")
            Nothing
            NoComment
        ]

eacHistoryToLedger :: [Record] -> Either Text [Transaction]
eacHistoryToLedger = fmap reverse . mapM recordToTransaction
