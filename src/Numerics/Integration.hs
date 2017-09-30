{-# LANGUAGE BangPatterns #-}
module Numerics.Integration
(
    trapezoid, trapezoid', trapezoid''
)
where

    trapezoid :: (RealFloat a) => (a -> a) -> (a,a) -> a -> a
    trapezoid f (a, b) e = trapezoidIter f h t a n e
        where
            h = b - a
            t = h * (f a + f b) / 2.0
            n = 1

    trapezoidIter :: (RealFloat a) => (a -> a) -> a -> a -> a -> Int -> a -> a
    trapezoidIter f h t a n e = if abs (t2 - m) < e then t2 else trapezoidIter f h2 t2 a n2 e
        where
            m = h * middleSum f a h (n-1)
            t2 = (t + m) / 2.0
            h2 = h / 2.0
            n2 = 2 * n

    middleSum :: (RealFloat a) => (a -> a) -> a -> a -> Int -> a
    middleSum f a h 0 = f (a + 0.5 * h)
    middleSum f a h j = f (a + (fromIntegral j + 0.5) * h) + middleSum f a h (j-1)

    trapezoid' :: (RealFloat a) => (a -> a) -> (a,a) -> a -> a
    trapezoid' f (!a, !b) !e = trapezoidIter' f h t a n e
        where
            !h = b - a
            !t = h * (f a + f b) / 2.0
            !n = 1

    trapezoidIter' :: (RealFloat a) => (a -> a) -> a -> a -> a -> Int -> a -> a
    trapezoidIter' f !h !t !a !n !e = if abs (t2 - m) < e then t2 else trapezoidIter' f h2 t2 a n2 e
        where
            !m = h * middleSum' f a h (n-1)
            !t2 = (t + m) / 2.0
            !h2 = h / 2.0
            !n2 = 2 * n

    middleSum' :: (RealFloat a) => (a -> a) -> a -> a -> Int -> a
    middleSum' f !a !h 0 = f (a + 0.5 * h)
    middleSum' f !a !h !j = f (a + (fromIntegral j + 0.5) * h) + middleSum' f a h (j-1)    

    trapezoid'' :: (RealFloat a) => (a -> a) -> (a,a) -> a -> a
    trapezoid'' f (!a, !b) !e = trapezoidIter'' f h t a n e
        where
            !h = b - a
            !t = h * (f a + f b) / 2.0
            !n = 1

    trapezoidIter'' :: (RealFloat a) => (a -> a) -> a -> a -> a -> Int -> a -> a
    trapezoidIter'' f !h !t !a !n !e = if abs (t2 - m) < e then t2 else trapezoidIter'' f h2 t2 a n2 e
        where
            !m = h * middleSum'' f a h (n-1)
            !t2 = (t + m) / 2.0
            !h2 = h / 2.0
            !n2 = 2 * n

    middleSum'' :: (RealFloat a) => (a -> a) -> a -> a -> Int -> a
    middleSum'' f !a !h !n = sum values
        where
            !values = [fx | j <- [0..n], let fx = f (a + (fromIntegral j + 0.5) * h) ]