import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Parser
import Node
import Data.Map (Map)
import qualified Data.Map as Map

stripPrefixTest1 = assertEqual "stripPrefixTest1" (Just "Rest") (stripPrefix "somePrefix" "somePrefixRest")

stripPrefixTest2 = assertEqual "stripPrefixTest2" Nothing (stripPrefix "somePrefix" "somePrfixRest")

splitTest1 = assertEqual "splitTest1" ["abc-edge","asdf","beet","asdf -bert"] (split ["abc-edge", "asdf- beet", "asdf -bert"])

parseTest1 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest1" MainPlazaHalfPipe a
parseTest2 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest2" Missile b
parseTest3 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest3" RSaveStation1 c

countTrue = assertEqual "countTrue" True (containsCount 2 Missile (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1)]))

countTrue2 = assertEqual "countTrue2" True (containsCount 3 Missile (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1)]))

countFalse = assertEqual "countFalse" False (containsCount 3 PowerBomb (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1), (PowerBomb, 2)]))

countNeg = assertEqual "countNeg" False (containsCount (-1) PowerBomb (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1), (PowerBomb, 2)]))

containsAny1 = assertEqual "contiainsAny1" False (listContainsAny [IceBeam, WaveBeam] [ChargeBeam, PowerBomb ,GrappleBeam, PowerBomb, Missile, Missile, Missile])

containsAny2 = assertEqual "contiainsAny2" True (listContainsAny [GrappleBeam, WaveBeam] [ChargeBeam, PowerBomb ,GrappleBeam, PowerBomb, Missile, Missile, Missile])

testVal = "Randomizer V3.1\nSeed: 525693944\nExcluded pickups: 5 19 28 \nChozo - - - Main Plaza (Half-Pipe) - - - - - - - - Missile Expansion 17 - - Warps to: Chozo Ruins | Save Station 1\nElevators:\nChozo Ruins - Transport to Tallon Overworld North <> Tallon Overworld - Transport to Chozo Ruins West"

tests = [testCase "stripPrefixTest1" stripPrefixTest1
        , testCase "stripPrefixTest2" stripPrefixTest2
        , testCase "splitTest1" splitTest1
        , testCase "parseTest1" parseTest1
        , testCase "parseTest2" parseTest2
        , testCase "parseTest3" parseTest3
        , testCase "countTrue" countTrue
        , testCase "countTrue2" countTrue2
        , testCase "countFalse" countFalse
        , testCase "countNeg" countNeg
        , testCase "containsAny1" containsAny1
        , testCase "containsAny2" containsAny2]

main :: IO ()
main = defaultMainWithOpts tests mempty