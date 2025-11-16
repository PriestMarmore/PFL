-- ====================================
-- Worksheet 1
-- ====================================

-- 1.1



-- 1.2


-- 1.3


-- 1.4


-- 1.5


-- 1.6


-- 1.7



-- ====================================
-- Worksheet 2
-- ====================================

-- 2.1
classify_conditional :: Int -> String
classify_conditional grade = if grade <= 9 then "failed" else
    if grade <= 12 then "passed" else
    if grade <= 15 then "good" else
    if grade <= 18 then "very good" else "excellent"

classify_guards :: Int -> String
classify_guards grade   | grade <= 9    = "failed"
                        | grade <= 12   = "passed"
                        | grade <= 15   = "good"
                        | grade <= 18   = "very good"
                        | otherwise     = "excellent"

-- 2.2
classifyBMI :: Float -> Float -> String
classifyBMI weight height
    | bmi < 18.5    = "underweight"
    | bmi < 25      = "normal weight"
    | bmi < 30      = "overweight"
    | otherwise     = "obese"
    where bmi = weight / (height * height)

-- 2.3

    -- a
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z
    | x >= y = if x >= z then x else z
    | y > x = if y >= z then y else z
min3 x y z
    | x <= y = if x <= z then x else z
    | y < x = if y <= z then y else z

    -- b
max32, min32 :: Ord a => a -> a -> a -> a
max32 x y z = Prelude.max x (Prelude.max y z)
min32 x y z = Prelude.min x (Prelude.min y z)

-- 2.4
xor :: Bool -> Bool -> Bool
xor False False = False
xor False secondBool = secondBool
xor firstBool False = firstBool

-- 2.5
safetailConditional :: [a] -> [a]
safetailConditional lista = if null lista then [] else tail lista

safetailGuards :: [a] -> [a]
safetailGuards lista
    | null lista        = []
    | otherwise         = tail lista

safetailPatterns :: [a] -> [a]
safetailPatterns []         = []
safetailPatterns (_:rest)   = rest

-- 2.6

    -- a
shortLength :: [a] -> Bool
shortLength lista = length lista < 3

    -- b
shortPatterns :: [a] -> Bool
shortPatterns [] = True
shortPatterns [_] = True
shortPatterns [_,_] = True
shortPatterns _ = False