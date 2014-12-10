#!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleInstances  #-}



import Control.Monad.Reader hiding (when)
import Data.Decimal
import qualified Data.List as L hiding (and)
import DSL 
import FinancialArithmetic
import Models

import Graphics.EasyPlot


------------------------------------------------------------------------------
main = do
    let payments1 = []
    let u1 = Contract "1" $ when (at $ 3 Months) (Give $ One $ 100 USD)
    let u2 = Contract "2" $ american (1 Month, 3 Months) (One $ 10 NZD)
    
    
    print $ u1 .+ u2
    -- let timeline = map Model [0..11]
    -- print $ 
    
    
    {- EXAMPLE 1: ------------------------------------------------------------
    
    
    Over the timeframe of 12 months, get and compare performance figures 
    for two contracts.
    First contract: 
        Pay 100 NZD at the beginning of the third month, 
        get 105 NZD at the end of the 10th month.
    Second contract:
        Pay 100 USD at the beginning of the third month, 
        get 103 USD at the end of the 10th month.
    -}
    let c1 = Contract "C1" $
                (when (at $ 2 Months) (Give $ One $ 100 NZD)) 
                `And`
                (when (at $ 10 Months) (One $ 105 NZD))
    
    let c2 = Contract "C2" $ And
                (when (at $ 2 Months) (Give $ One $ 100 USD)) 
                (when (at $ 10 Months) (One $ 103 USD))
                
    {-
    Information provided:
        Main currency
        Highest deposit rates for each month
        Lowest loan rates for each month
        
        Defined in Models.hs
    -}
    let m = m_nzakl_2015_baseline
    
    
    -- Schedule of payments
    let s1 = contractToSchedule m c1
    let s2 = contractToSchedule m c2
    
    -- Print actions for each period of the whole term
    print $ name c1 ++ ": " ++ (show s1)
    print $ name c2 ++ ": " ++ (show s2)

    {-
    Questions asked:
        * Calculate NPV, NFV for each contract
        * Calculate equivalent deposit rate for each contract
    
    -}
    
    let npv :: Contract -> Reader (Model, Time) Contract
        npv c = do 
            (m, t) <- ask
            return $ npv' m t c

        
    print $ runReader (npv c1) (m, 1 Month)

        
