{-# OPTIONS_GHC -O2 -fno-cse #-}
primesFromTMWE primes m = dropWhile (< m) [2,3,5,7,11]
                          ++ gapsW a wh2 (compositesFrom a)
  where
    (a,wh2) = rollFrom (snapUp (max 3 m) 3 2)
    (h,p2:t) = span (< z) $ drop 4 primes           -- p < z => p*p<=a
    z = ceiling $ sqrt $ fromIntegral a + 1         -- p2>=z => p2*p2>a
    compositesFrom a = joinT (joinST [multsOf p  a    | p <- h ++ [p2]]
                               : [multsOf p (p*p) | p <- t] )

snapUp v o step = v + (mod (o-v) step)              -- full steps from o
multsOf p from = scanl (\c d->c+p*d) (p*x) wh       -- map (p*) $
  where                                             --   scanl (+) x wh
    (x,wh) = rollFrom (snapUp from p (2*p) `div` p) --   , if p < from

wheelNums = scanl (+) 0 wheel
rollFrom n = go wheelNums wheel
  where m = (n-11) `mod` 210
        go (x:xs) ws@(w:ws2) | x < m = go xs ws2
                             | True  = (n+x-m, ws)  -- (x >= m)

-- tree-merging Eratosthenes sieve, primesTME of Q.31,
--  adjusted to produce primes in a given range (inclusive)
primesR a b | b < a || b < 2 = []
            | otherwise      = takeWhile (<= b) $ primesFrom a

              primesFrom a0 = (if a0 <= 2 then [2] else []) ++
                              (gaps a $ mults $ span (< z) $ tail primesTME)
                                where
                                      a = snap (max 3 a0) 3 2
                                          z = ceiling $ sqrt $ fromIntegral a + 1       -- p<z => p*p<=a
                                                  snap v origin step = if r==0 then v else v+(step-r)
                                                                                                   where r = rem (v-origin) step   -- NB: origin <= v ; else use MOD

                                                                                                               mults (h,p':t) =                              -- p'>=z => p'*p'>a
                                                                                                                     join union ( [[x,x+s..] | p <- h,           -- heads unordered
                                                                                                                                                               let s=2*p; x=snap a (p*p) s]
                                                                                                                                                     ++ [[p'*p',p'*p'+2*p'..]] )
                                                                                                                           `union'` join union' [[p*p,p*p+2*p..] | p <- t]

                                                                                                                         join  f (xs:t)    = f xs (join f (pairs f t))
                                                                                                                                                 join  f []        = []
                                                                                                                                                                         pairs f (xs:ys:t) = f xs ys : pairs f t
                                                                                                                                                                                                 pairs f t         = t
                                                                                                                                                                                                                         union' (x:xs) ys  = x : union xs ys           -- `union` of Q.31
                                                                                                                                                                                                                                                 gaps k xs@(x:t) | k==x  = gaps (k+2) t
                                                                                                                                                                                                                                                                                     | True  = k : gaps (k+2) xs
