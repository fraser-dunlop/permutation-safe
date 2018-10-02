{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (nub,(\\))
import Data.Permutation
import Data.Permutation.Internal

main :: IO ()
main = hspec $ do
  describe "Data.Permutation.fromRelation" $ do
    it "[(1,2),(2,2),(2,3)] returns permutation construction error" $ do
      fromRelation [(1,2),(2,2),(2,3)] `shouldBe` relationConstructionError 

    it "[(1,2),(2,3)] returns permutation construction error" $ do
      fromRelation [(1,2),(2,3)] `shouldBe` relationConstructionError 

    it "[(1,2),(2,1)] returns permutation" $ do
      fromRelation [(1,2),(2,1)] `shouldBe` permutationValidLiteral [(1,2),(2,1)]

    it "[(1,2)] returns permutation construction error" $ do
      fromRelation [(1,2)] `shouldBe` relationConstructionError 

    it "[] returns permutation" $ do
      fromRelation [] `shouldBe` permutationValidLiteral ([] :: [(Int,Int)]) 

    it "toCycles (Permutation [(1,2),(2,3),(3,1)]) `shouldBe` [[1,2,3]]" $ do
      toCycles (Permutation [(1,2),(2,3),(3,1)]) `shouldBe` [[1,2,3]]

    it "toCycles (Permutation [(1,2),(2,3),(3,1),(4,5),(5,6),(6,4)])`shouldBe` [[1,2,3],[4,5,6]]" $ do
      toCycles (Permutation [(1,2),(2,3),(3,1),(4,5),(5,6),(6,4)])
            `shouldBe` [[1,2,3],[4,5,6]]

    it "toCycles (Permutation [(4,5),(5,6),(6,4),(1,2),(2,3),(3,1)]) `shouldBe` [[4,5,6],[1,2,3]]" $ do
      toCycles (Permutation [(4,5),(5,6),(6,4),(1,2),(2,3),(3,1)])
            `shouldBe` [[4,5,6],[1,2,3]]

    it "toCyclesCanonical (Permutation [(4,5),(5,6),(6,4),(1,2),(2,3),(3,1)]) `shouldBe` [[1,2,3],[4,5,6]]" $ do
      toCyclesCanonical (Permutation [(4,5),(5,6),(6,4),(1,2),(2,3),(3,1)])
            `shouldBe` [[1,2,3],[4,5,6]]

    it "fromRelation valid: zipping an offset of a unique list of ints" $
      property $ \d (xs :: [Int]) ->
        (case fromRelation (zip (nub xs)
                                (drop d (cycle (nub xs)))) of
          Right _ -> True
          Left _  -> False) == True

    it "fromRelation not valid: injecting a duplicate value" $
      property $ \d (xs :: [Int]) ->
        (if length xs > 0
            then (case fromRelation (zip ((head xs):(nub xs))
                                    (drop d (cycle ((head xs):(nub xs))))) of
                    Right _ -> True
                    Left _  -> False)
            else False) == False

    it "fromRelation not valid: injecting a duplicate value" $
      property $ \d (xs :: [Int]) ->
        (if length xs > 0
            then (case fromRelation (zip ((head xs):(nub xs))
                                    (drop d (cycle (nub xs)))) of
                    Right _ -> True
                    Left _  -> False)
            else False) == False


    it "fromTwoLineForm valid: offsetting a unique list of ints" $
      property $ \d (xs :: [Int]) ->
        (if length xs > 0
            then case fromTwoLineForm (nub xs
                                      , take (length (nub xs)) (drop d (cycle (nub xs)))) of
                   Right _ -> True
                   Left _  -> False
            else True)

    it "fromTwoLineForm invalid: lists of different length" $
      property $ \d (xs :: [Int]) ->
        (if length xs > 0
            then case fromTwoLineForm (nub xs
                                      , take (1 + length (nub xs)) (drop d (cycle (nub xs)))) of
                   Left (PermutationError "Data.Permutation.fromTwoLineForm: The top and bottom lines have different length") -> True
                   _ -> False
            else True)

    it "fromTwoLineForm invalid: injecting a duplicate value" $
      property $ \d (xs :: [Int]) ->
        (if length xs > 0
            then case fromTwoLineForm ((head xs):(nub xs)
                                      , take (1 + length (nub xs)) (drop d (cycle (nub xs)))) of
                   Left (PermutationError "Data.Permutation.fromRelation: The relation is not bijective") -> True
                   _ -> False
            else True)

    it "multitest: transitivity" $
      property $ \d (xs :: [Int]) ->
        if length xs > 0
           then let min' = minimum xs
                    max' = maximum xs
                    rel' = (zip (nub xs) (drop d (cycle (nub xs))))
                 in case (do
                      perm    <- fromRelation rel'
                      twoline <- toTwoLineForm min' max' perm
                      perm'   <- fromTwoLineForm twoline
                      perm''  <- fromCycles $ toCycles perm'
                      perm''' <- fromCycles $ toCyclesCanonical perm''
                      rel     <- toRelation min' max' perm''' 
                      return rel
                         ) of
                        Right rel ->
                          rel \\ (rel' ++ (zip [min'..max'] [min'..max'])) == []
                        _ -> False
            else True

    it "multitest: transitivity" $
      property $ \d (xs :: [Int]) ->
        if length xs > 0
           then let min' = minimum xs
                    max' = maximum xs
                    rel' = (zip (nub xs) (drop d (cycle (nub xs))))
                 in case (do
                      perm    <- fromRelation rel'
                      twoline <- toTwoLineForm min' max' perm
                      perm'   <- fromTwoLineForm twoline
                      perm''  <- fromCycles $ toCycles perm'
                      perm''' <- fromCycles $ toCyclesCanonical perm''
                      rel     <- toRelation min' max' perm''' 
                      return [perm, perm', perm'', perm'''] 
                         ) of
                        Right perms -> (length (nub perms)) == 1
                        _ -> False
            else True

relationConstructionError :: Either PermutationError (Permutation a)
relationConstructionError = Left $ PermutationError
                "Data.Permutation.fromRelation: The relation is not bijective"

permutationValidLiteral :: [(a,a)] -> Either PermutationError (Permutation a)
permutationValidLiteral p = Right $ Permutation p



