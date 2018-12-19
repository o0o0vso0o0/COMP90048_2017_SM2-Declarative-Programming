module Lab1 (subst, interleave, unroll) where

subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] = []
subst a b l = (if l!!0 == a then b else l!!0):(subst a b (drop 1 l))
--main = putStrLn (show (subst 'e' 'o' "elephant"))

interleave :: [t] -> [t] -> [t]
interleave [] b = b
interleave a b = a!!0 : (interleave b (drop 1 a))
--main = putStrLn (show (interleave  [1,2,3,4] [11,12,13,14]))
--main = putStrLn (show (interleave "" "dog"))
--main = putStrLn (show (interleave "wl" "arus" ))
--main = putStrLn (show (interleave  "tlpone" "eeh" ))

unroll :: Int -> [a] -> [a]
unroll _ [] = []
unroll num list = if num < 0 then take (-num) (cycle (reverse list)) else take num (cycle list)
--unroll num list = if num < 0 then [] else take num (cycle list)
--main = putStrLn (show (unroll 4 "ski"))