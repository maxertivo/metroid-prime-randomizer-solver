import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Parser
import Node
import Predicates
import Util
import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

parseTest1 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest1" (getItemMapKey MainPlazaHalfPipe) a
parseTest2 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest2" Missile b
parseTest3 = let (Item a b c : rest) = parse testVal in assertEqual "parseTest3" (getRoomMapKey RSaveStation1) c

parseElevatorTest = assertEqual "parseElevatorTest" [(fromEnum RTransporttoTallonOverworldNorth, fromEnum OTransporttoChozoRuinsWest)] (parseElevators testVal)

countTrue = assertEqual "countTrue" True (containsCount 2 Missile (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1)]))

countTrue2 = assertEqual "countTrue2" True (containsCount 3 Missile (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1)]))

countFalse = assertEqual "countFalse" False (containsCount 3 PowerBomb (Map.fromList [(ChargeBeam,1), (Missile,3), (GrappleBeam,1), (PowerBomb, 2)]))

containsAny1 = assertEqual "containsAny1" False (listContainsAny [IceBeam, WaveBeam] [ChargeBeam, PowerBomb ,GrappleBeam, PowerBomb, Missile, Missile, Missile])

containsAny2 = assertEqual "containsAny2" True (listContainsAny [GrappleBeam, WaveBeam] [ChargeBeam, PowerBomb ,GrappleBeam, PowerBomb, Missile, Missile, Missile])

testVal = Data.Text.pack "Randomizer V3.1\nSeed: 525693944\nExcluded pickups: 5 19 28 \nChozo - - - Main Plaza (Half-Pipe) - - - - - - - - Missile Expansion 17 - - Warps to: Chozo Ruins | Save Station 1\nElevators:\nChozo Ruins - Transport to Tallon Overworld North <> Tallon Overworld - Transport to Chozo Ruins West"

tests = [testCase "parseTest1" parseTest1
        , testCase "parseTest2" parseTest2
        , testCase "parseTest3" parseTest3
        , testCase "parseElevatorTest" parseElevatorTest
        , testCase "countTrue" countTrue
        , testCase "countTrue2" countTrue2
        , testCase "countFalse" countFalse
        , testCase "containsAny1" containsAny1
        , testCase "containsAny2" containsAny2]

main :: IO ()
main = defaultMainWithOpts tests mempty