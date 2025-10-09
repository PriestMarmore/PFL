import Test.QuickCheck

xor :: Bool -> Bool -> Bool
xor True True = False
xor False secondBool = secondBool
xor firstBool False = firstBool
