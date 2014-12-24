sizeS :: Int -> Int
sizeS 0 = 0
sizeS 1 = 3 -- true, false, 0
sizeS i = let sizeS' = sizeS (i - 1)
          in sizeS' +
             sizeS'*3 + -- succ, pred, iszero
             sizeS'^3 -- if 

main = print $ sizeS 3
