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
recordToTransaction (RecordDividend d) = dividendToTransaction d
recordToTransaction (RecordTaxWithholding d) = taxWithholdingToTransaction d

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
      , sQuantity = sQuantity
      , sFeesAndCommissions = sFeesAndCommissions
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

dividendToTransaction :: Eac.Dividend -> Either Text Transaction
dividendToTransaction (Eac.Dividend{dividendDate = dividendDate, dividendAmount = dividendAmount}) =
  return
    $ makeTransaction
      dividendDate
      (Just Cleared)
      "GOOG Dividend"
      [ makePosting
          Nothing
          (liquidAssets <:> "Charles Schwab:EAC:USD")
          (Just $ toAmount dividendAmount)
          NoComment
      , makePosting
          Nothing
          "Income:Capital Gains"
          (Just . negate $ toAmount dividendAmount)
          NoComment
      ]

taxWithholdingToTransaction :: Eac.TaxWithholding -> Either Text Transaction
taxWithholdingToTransaction (Eac.TaxWithholding{taxWithholdingDate = taxWithholdingDate, taxWithholdingAmount = taxWithholdingAmount}) =
  return
    $ makeTransaction
      taxWithholdingDate
      (Just Cleared)
      "GOOG Tax Withholding"
      [ makePosting
          Nothing
          (liquidAssets <:> "Charles Schwab:EAC:USD")
          (Just $ toAmount taxWithholdingAmount)
          NoComment
      , makePosting
          Nothing
          "State:2024:CS Withholding Tax:GOOG"
          (Just . negate $ toAmount taxWithholdingAmount)
          NoComment
      ]

eacHistoryToLedger :: [Record] -> Either Text [Transaction]
eacHistoryToLedger = fmap reverse . mapM recordToTransaction
