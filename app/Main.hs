module Main where

import           Lib
import           Control.Monad.Trans

main :: IO ()
main = do
    putStrLn "Woah, look at us!"
    -- x <- safeDivision 3 2        :: IO Double
    -- putStrLn "the result was: "
    -- print x
    -- y <- safeDivision 3 0
    -- print "This never happens because we threw an exception!"


class CanFail failable where
    oops :: failable a
    pick :: failable a -> failable a -> failable a
    win  :: a -> failable a

instance CanFail Maybe where
    oops                  = Nothing
    pick (Just a) _       = Just a
    pick Nothing (Just a) = Just a
    pick Nothing Nothing  = oops
    win a                 = Just a

data MaybeError a = Error String | Result a 

instance CanFail MaybeError where
    oops              = Error "oops"
    pick (Result a) _ = Result a
    pick _ (Result a) = Result a
    win a             = Result a

safeDivision :: CanFail failable => Double -> Double -> failable Double
safeDivision x y =
    if y == 0 
       then oops
       else win (x / y)

someMathFunction :: Double -> Double -> Double
someMathFunction x y =
    let result = safeDivision x y in
    case result of
        Just number -> number * 3
        Nothing     -> 0 

someMathFunction2 :: Double -> Double -> Double
someMathFunction2 x y =
    let result = safeDivision x y in
    case result of
        Result number -> number * 3
        Error str     -> 0 

--
-- try :: IO a -> IO (MaybeError a)
--
-- instance CanFail IO where
--     oops = throwException "oops"
--     pick first second = do
--         eResult <- try first
--         case eResult of
--             Result a ->
--                 return a
--             Error exception ->
--                 second
