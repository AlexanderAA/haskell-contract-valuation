{-# LANGUAGE FlexibleInstances  #-}

{- Experimental DSL which allows to use relatively friendly syntax
to create financial models operating contracts, amounts, currencies.

Typechecks amounts - amounts in different currencies cannot be added 
accidentally.

Defines primitive contracts (european, american, etc.) for convenience.

Knows how to simplify contract and how to convert contract to amount
in the main currency at a given time using simplistic model of exchange rates.

-}


module DSL where
------------------------------------------------------------------------------
import Data.Decimal
import qualified Data.List as L
------------------------------------------------------------------------------


        -- Contract

------------------------------------------------------------------------------

data Contract = Contract Name Terms deriving Show

name :: Contract -> Name
name (Contract n t) = n

terms :: Contract -> Terms
terms (Contract n t) = t

type Name = String
data Terms =
    Zero
    | One  Amount
    | Give Terms
    | And  Terms Terms
    | Or   Terms Terms
    | Cond    (Obs Bool)   Terms Terms
    | Scale   (Obs Int)    Terms
    | When    (Obs Bool)   Terms
    deriving Show
------------------------------------------------------------------------------


        -- Time, Period, Random value, Observable
        
------------------------------------------------------------------------------
data PeriodName = Month | Months

type Time   = Integer
type Period = Integer

instance Num (PeriodName -> Time) where
    fromInteger t Month = t::Time
    fromInteger t Months = t

newtype Obs a = Obs (Time -> a)

getValue :: Obs a -> Time -> a
getValue (Obs x) time = x time

instance Show a => Show (Obs a) where
    show (Obs obs) = "(Obs " ++ show (obs 0) ++ ")"

konst :: a -> Obs a
konst k = Obs (\t -> k)

at :: Time -> Obs Bool
at t = Obs (\time -> (time == t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> f (o1 t) (o2 t))

date :: Obs Time
date = Obs (\t -> t::Time)



-- Compare observables
(%<), (%<=), (%==), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%>)  = lift2 (>)
(%==) = lift2 (==)
(%>=) = lift2 (>=)
(%<=) = lift2 (<=)

type Term            = [Time]
type PaymentSchedule = [Amount]
------------------------------------------------------------------------------
    

        -- Currencies and amounts
        
------------------------------------------------------------------------------
data Currency = AUD | NZD | USD deriving (Eq, Show)
data Amount = Amt Decimal Currency

instance Show Amount where
    show (Amt amt currency) = show amt ++ show currency

instance Num (Currency -> Amount) where
    fromInteger amt c = Amt (Decimal 0 amt) c

instance Num Amount where
    (-) (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = Amt (a1-a2) c1
    (+) (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = Amt (a1+a2) c1

instance Eq Amount where
    (==) (Amt a1 c1) (Amt a2 c2) = (a1==a2) && (c1==c2)
    
instance Ord Amount where
    compare (Amt a1 c1) (Amt a2 c2)
        | (c1 == c2) = compare a1 a2

instance Eq Terms where
    (==) (One a1) (One a2) = (a1 == a2)

instance Ord Terms where
    compare (One a1) (One a2) = compare a1 a2
    compare (One a1) Zero = compare a1 0
    compare Zero (One a2) = compare 0 a2
    compare Zero Zero = EQ
    
instance Eq Contract where
    (==) (Contract n1 t1) (Contract n2 t2) = (t1 == t2)

instance Ord Contract where
    compare (Contract n1 t1) (Contract n2 t2) = compare t1 t2
    

amountToDecimal (Amt a c) = a
------------------------------------------------------------------------------



        -- Operations on Terms
        
------------------------------------------------------------------------------
zero :: Terms
zero = Zero

one :: Amount -> Terms
one = One

scale :: Obs Int -> Terms -> Terms
scale = Scale

give :: Terms -> Terms
give = Give

and :: Terms -> Terms -> Terms
and = And

or :: Terms -> Terms -> Terms
or = Or

cond :: Obs Bool -> Terms -> Terms -> Terms
cond = Cond

when :: Obs Bool -> Terms -> Terms
when = When
------------------------------------------------------------------------------


        -- Operations on contracts

------------------------------------------------------------------------------
(.+) :: Contract -> Contract -> Contract
(.+) (Contract n1 t1) (Contract n2 t2) = 
    Contract (n1 ++ " _And_ " ++ n2) (t1 `DSL.and` t2)
------------------------------------------------------------------------------


        -- Specifics
        
------------------------------------------------------------------------------
-- Zero coupon bond
zcb :: Time -> Amount -> Terms
zcb t amt = when (at t) (one amt)

-- European option
european :: Time -> Currency -> Terms -> Terms
european t c u = when (at t) (u `DSL.or` Zero)

-- American option
american :: (Time, Time) -> Terms -> Terms
american (t1, t2) u = when (between t1 t2) u

-- Option
between :: Time -> Time -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))
------------------------------------------------------------------------------



        -- Modelling

------------------------------------------------------------------------------
type ExchangeRate = Double

maxT :: ExchangeRate -> Terms -> Terms -> Terms
maxT exchR Zero Zero = Zero
maxT exchR (One (Amt a1 c1)) Zero = One $ Amt (max a1 0) c1
maxT exchR Zero (One (Amt a2 c2)) = One $ Amt (max 0 a2) c2
maxT exchR (One (Amt a1 c1)) (One (Amt a2 c2)) = One $ Amt (max a1 a21) c1
    where 
        a21 = a2 *. (realToFrac exchR)

getCurrency :: Amount -> Currency
getCurrency (Amt amt currency) = currency

convertTo :: Model -> Time -> Currency ->  Terms -> Terms
convertTo m t c0 Zero = Zero
convertTo m t c0 (One (Amt amt c))
    | (c0 == c)  = One $ Amt amt c0 -- No need to convert anything
    | otherwise  = One $ Amt (amt *. currentER) c0
    where
        -- Exchange Rates
        observableER = (exchangeRate m) c c0
        currentER    = realToFrac $ getValue observableER t


data Model = Model { 
    mainCurrency :: Currency,
    depositRate :: Obs Decimal,
    loanRate :: Obs Decimal,
    exchangeRate :: Currency -> Currency -> Obs ExchangeRate }

evalContractAt :: Model -> Time -> Contract -> Contract
evalContractAt m t (Contract name terms) = Contract name (evalTermsAt m t terms)

-- Simplify contract, based on the provided time and model
evalTermsAt :: Model -> Time -> Terms -> Terms
evalTermsAt m t = eval
    where 
        eval Zero                      = Zero
        eval (One amt)                 = convertTo m t (mainCurrency m) $ One amt
        eval (Give c)                  = eval (scale (Obs (\t -> (-1))) (eval c))
        eval (Zero `And` Zero)         = Zero
        eval (Zero `And` (One amt))    = One amt
        eval ((One amt) `And` Zero)    = One amt
        eval (c1 `And` c2)             = eval ((eval c1) `DSL.and` (eval c2))
        -- eval (c1 `Or`  c2)             = maxT ((exchangeRate m) (getCurrency (eval c1)) (getCurrency (eval c2)) time) (eval c1) (eval c2) 
        eval (Cond (Obs o) c1 c2)      = if (o t) then (eval c1) else (eval c2)
        eval (When (Obs o) c)          = if (o t) then (eval c)  else Zero
        eval (Scale (Obs s) (One (Amt amt cur))) = One $ Amt (amt *. (realToFrac $ s t)) cur

-- Calculates contract value at a given point of time
toAmountAt :: Model -> Time -> Terms -> Amount
toAmountAt m t = toAmount . (evalTermsAt m t)
    where
        toAmount :: Terms -> Amount
        toAmount Zero      = Amt 0 (mainCurrency m)
        toAmount (One amt) = amt

------------------------------------------------------------------------------
