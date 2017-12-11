import Test.QuickCheck
import Test.QuickCheck.Function

prop :: Fun Integer Integer -> Bool
prop f = apply f 10 == apply f 20

main = quickCheck prop
