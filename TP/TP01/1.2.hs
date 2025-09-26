leftHalf :: [a] -> [a]
leftHalf xs = take (length xs `div` 2) xs

rightHalf :: [a] -> [a]
rightHalf xs = drop (length xs `div` 2) xs

-- Test in GHCi:
-- leftHalf [1,2,3,4,5,6,7]   -- [1,2,3]
-- rightHalf [1,2,3,4,5,6,7]  -- [4,5,6,7]