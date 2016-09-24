module DateTimeSpec
   (
     spec
   ) where

import           DateTime
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary DT where
  arbitrary = do
    toDT' <$> choose (2015,2017)
          <*> choose (1,12)
          <*> choose (1,28)
          <*> choose (0,23)
          <*> choose (0,59)
          <*> choose (0,59)

instance Arbitrary DayOfWeek where
  arbitrary = toEnum <$> choose (0,6)

instance Arbitrary Match where
  arbitrary = do
    x <- choose (1,7::Int)
    case x of
      5 -> Not <$> arbitrary
      6 -> Or <$> arbitrary <*> arbitrary
      7 -> And <$> arbitrary <*> arbitrary
      _ -> do
        x <- choose (0,6::Int)
        case x of
          0 -> return Always
          1 -> return Never
          2 -> dowMatch <$> listOf arbitrary
          3 -> dayMatch <$> listOf (choose (1,28::Int))
          4 -> monthMatch <$> listOf (choose (1,12::Int))
          5 -> todMatch <$> listOf todPair
          6 -> arbitrary >>= \x -> arbitrary >>= \y -> return (DTMatch (x,y))
    where todPair = do
            h1 <- choose (0,10)
            h2 <- choose (11,23)
            m1 <- choose (0,59)
            s1 <- choose (0,59)
            m2 <- choose (0,59)
            s2 <- choose (0,59)
            return ((h1::Int,m1,s1),(h2,m2,s2))

spec :: Spec
spec = do
  describe "tick/untick" $ do
    it "Works correctly" $ do
      head (drop 3600 $ iterate tick (toDT' 2015 1 1 0 0 0)) `shouldBe` toDT' 2015 1 1 1 0 0
      head (drop (2*3600) $ iterate tick (toDT' 2015 1 1 0 0 0)) `shouldBe` toDT' 2015 1 1 2 0 0
      head (drop (24*3600) $ iterate tick (toDT' 2015 1 1 0 0 0)) `shouldBe` toDT' 2015 1 2 0 0 0
      head (drop (31*24*3600) $ iterate tick (toDT' 2015 1 1 0 0 0)) `shouldBe` toDT' 2015 2 1 0 0 0
      head (drop (24*3600) $ iterate tick (toDT' 2015 12 31 0 0 0)) `shouldBe` toDT' 2016 1 1 0 0 0

    prop "untick (tick dt) == dt" $
      forAll arbitrary $ \dt -> untick (tick dt) === dt

    prop "tick (untick dt) == dt" $
      forAll arbitrary $ \dt -> tick (untick dt) === dt

  describe "Compare Match to Brute Force Match" $ do
    prop "match" $
      forAll arbitrary $ \m ->
      forAll arbitrary $ \s ->
        match m s === model_match m s

    prop "split" $
      forAll arbitrary $ \m ->
      forAll arbitrary $ \s ->
        let (e:_) = drop (10*3600) $ iterate tick s
        in split m s e === model_split m s e

  describe "Laws" $ do
    prop "match (Not m) == not (match m)" $
      forAll arbitrary $ \m ->
      forAll arbitrary $ \dt ->
        match (Not m) dt === not (match m dt)
    prop "match (m1 `And` m2) == match m1 && match m2" $
      forAll arbitrary $ \m1 ->
      forAll arbitrary $ \m2 ->
      forAll arbitrary $ \dt ->
        match (m1 `And` m2) dt === (match m1 dt && match m2 dt)
    prop "match (m1 `Or` m2) == match m1 || match m2" $
      forAll arbitrary $ \m1 ->
      forAll arbitrary $ \m2 ->
      forAll arbitrary $ \dt ->
        match (m1 `Or` m2) dt === (match m1 dt || match m2 dt)
    prop "DeMorgan's law (1)" $
      forAll arbitrary $ \m1 ->
      forAll arbitrary $ \m2 ->
      forAll arbitrary $ \dt -> match (Not $ m1 `And` m2) dt === match (Not m1 `Or` Not m2) dt
    prop "DeMorgan's law (2)" $
      forAll arbitrary $ \m1 ->
      forAll arbitrary $ \m2 ->
      forAll arbitrary $ \dt -> match (Not $ m1 `Or` m2) dt === match (Not m1 `And` Not m2) dt

  describe "Individual Match Specs" $ do
    prop "Always" $ forAll arbitrary (match Always)
    prop "Never" $ forAll arbitrary (not . match Never)
    it "DowMatch" $ do
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 4 7 10 0 `shouldBe` False
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 6 7 10 0 `shouldBe` False
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 8 7 10 0 `shouldBe` False
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 9 7 10 0 `shouldBe` False
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 10 7 10 0 `shouldBe` False
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 5 7 10 0 `shouldBe` True
      dowMatch [Tuesday,Thursday] `match` toDT' 2016 1 7 7 10 0 `shouldBe` True
    it "TodMatch" $ do
      todMatch [((3,10,0),(10,0,0)),((13,10,0),(15,0,0))] `match` toDT' 2016 1 4 2 10 0 `shouldBe` False
      todMatch [((3,10,0),(10,0,0)),((13,10,0),(15,0,0))] `match` toDT' 2016 1 4 12 10 0 `shouldBe` False
      todMatch [((3,10,0),(10,0,0)),((13,10,0),(15,0,0))] `match` toDT' 2016 1 4 5 10 0 `shouldBe` True
      todMatch [((3,10,0),(10,0,0)),((13,10,0),(15,0,0))] `match` toDT' 2016 1 4 14 10 0 `shouldBe` True

  describe "split" $ do
    let s = toDT' 2015 1 1 1 0 0
        e = toDT' 2015 1 1 5 0 0
        run m = split m s e
    it "returns one non-matching range" $ do
      run (dowMatch [Monday]) `shouldBe` [(False,4*3600+1,(s,e))]

    it "returns one matching range" $ do
      run (dowMatch [Thursday]) `shouldBe` [(True,4*3600+1,(s,e))]

    it "returns one matching + one none matching range" $ do
      run (todMatch [((0,0,0),(2,30,0))]) `shouldBe`
        [(True,3600+1800+1,(s,toDT' 2015 1 1 2 30 0)),
         (False,2*3600+1800,(toDT' 2015 1 1 2 30 1,e))]

    prop "the total duration of all ranges is equal to the input range" $
      forAll arbitrary $ \m ->
      forAll arbitrary $ \dt1 ->
      forAll arbitrary $ \dt2 -> (dt1 <= dt2) ==>
        let [(_,actual,_)] = split Always dt1 dt2
        in sum (map (\(_,n,_) -> n) (split m dt1 dt2)) == actual

toDT' a b c d e f = let Right x = toDT a b c d e f in x

