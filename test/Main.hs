import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Parser

stripPrefixTest1 = assertEqual "stripPrefixTest1" (Just "Rest") (stripPrefix "somePrefix" "somePrefixRest")

stripPrefixTest2 = assertEqual "stripPrefixTest2" Nothing (stripPrefix "somePrefix" "somePrfixRest")

splitTest1 = assertEqual "splitTest1" ["abc-edge","asdf","beet","asdf -bert"] (split ["abc-edge", "asdf- beet", "asdf -bert"])

parseTest = assertEqual "parseTest1" [("MainPlazaHalfPipe","Missile","RSaveStation1")] (parse testVal)

testVal = "Randomizer V3.1\nSeed: 525693944\nExcluded pickups: 5 19 28 \nChozo - - - Main Plaza (Half-Pipe) - - - - - - - - Missile Expansion 17 - - Warps to: Chozo Ruins | Save Station 1"

tests = [testCase "stripPrefixTest1" stripPrefixTest1
        , testCase "stripPrefixTest2" stripPrefixTest2
        , testCase "splitTest1" splitTest1
        , testCase "parseTest1" parseTest]

main :: IO ()
main = defaultMainWithOpts tests mempty