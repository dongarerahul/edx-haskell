import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Utils

pushTest :: Assertion
pushTest = [NumLit 1] ^? push (NumLit 1)

main :: IO()
main = defaultMainWithOpts
       [testcase "push" pushTest]
       mempty
