
module RowTraversal  where

import           Data.Array.Repa      as R
import           Data.Array.Repa.Eval as Ev


mat = fromListUnboxed (Z :. (3 :: Int) :. (3 :: Int)) [1,2,1,0,-4,1,0,0,-2] :: Array U DIM2 Double
bs = fromListUnboxed (Z :. (3:: Int)) [5,2,4] :: Array U DIM1 Double

arrayLength :: Array D DIM1 Double -> Int
arrayLength arr = n
                where (Z :. n) = extent arr

nRow :: Array U DIM2 Double -> Int -> Array U DIM1 Double
nRow arr n = computeS $ R.slice arr (Any :. n :. All) -- xs :: Array U DIM1 Double

delayNRow :: Array U DIM2 Double -> Int -> Array D DIM1 Double
delayNRow arr n =  R.slice arr (Any :. n :. All) 

--nCol ::(Ev.Load r sh e, Ev.Target r e) => Array r DIM2 e -> Int -> Array r DIM1 e--
--nCol ::(Load r0 sh e, Shape sh',  Target r0 e) =>
--            Array r0 sh e -> DIM1 -> Array r0 sh' e
nCol :: Array U DIM2 Double -> Int -> Array U DIM1 Double
nCol arr n = computeS $ R.slice arr (R.Any :. n)

mReverseRow :: Array U DIM2 Double -> Array D DIM2 Double 
mReverseRow mat = backpermute dim (\ (Z :. i :. cols) -> (Z :. n - i - 1 :. cols )) mat
            where dim@(Z :. n :. cols) = extent mat

vReverse :: Array D DIM1 Double -> Array D DIM1 Double
vReverse v = backpermute (Z :. n) (\ (Z :. i) ->  Z :.(n - i - 1)) v
            where n = arrayLength v


traverse2Backwards :: Array D DIM1 Double -> Array D DIM1 Double ->
                        (Double -> Double -> (Int, Int) -> Double) -> Array D DIM1 Double
traverse2Backwards v1 v2 f = traverse2 b1 b2 const
                              (\l1 l2 (Z :. i) -> f  (l1 (Z :. back n1 i )) (l2 (Z :. back n2 i))
                                    (back n1 i, back n2 i) )
                              where b1 = vReverse v1
                                    b2 = vReverse v2
                                    back n i = n - i - 1
                                    n1 = arrayLength v1
                                    n2 = arrayLength v2

{-/traverse3Backwards v1 v2 v3 f = traverse3 b1 b2 b3 (\ (Z :. i) _ _  ->  (Z :. i))
                                    (\l1 l2 l3 (Z :. i) -> f  (l1 (Z :. back n1 i )) (l2 (Z :. back n1 i)) (l3 (Z :. back n1 i))
                                          (back n1 i, back n1 i) )
                              where b1 = vReverse v1
                                    b2 = vReverse v2
                                    b3 = vReverse v3
                                    back n i = n - i - 1
                                    n1 = arrayLength v1
-}

traverseMatrixByRow matrix f = R.traverse matrix ( \(Z :. i :. _) -> (Z :. i))
									(\_ (Z :. currRow) -> f (nRow matrix currRow) currRow)


--traverses Matrix from bottom to up
traverseMatrixByRowBackwards matrix f = R.traverse matrix (\ (Z :. i :. _) -> (Z :. i)) g
                                            where (Z :. rows :. _) = extent matrix
                                                  g _ (Z :. i) = f (nRow matrix $ backRow i) i
                                                  backRow i = rows - i - 1
--traverseMat2 combines rows with columns
traverseMat2RC ::
                   Array R.U DIM2  Double -> Array R.U DIM2 Double ->
                   (Array R.U DIM1 Double -> Array R.U DIM1 Double -> (Int, Int) ->  Double) ->
                   Array R.D DIM2 Double
traverseMat2RC mat1 mat2 f = R.traverse2 mat1 mat2
                              (\ (Z :. i1 :. _) ( Z :. _ :. j2) -> (Z :. i1 :. j2)) oper
                              where oper _ _ (R.Z :. i :. j) = f (nRow mat1 i) (nCol mat2 j) (i, j)

traverseAugmentForm :: Array U DIM2 Double -> Array U DIM2 Double ->
                        Array U DIM2 Double ->
                        (Array U DIM1 Double -> Array U DIM1 Double ->
                              Array U DIM1 Double -> (Int, Int) ->  Double)
                        -> Array D DIM2 Double
traverseAugmentForm mat1 xs bs f = R.traverse3 mat1 xs bs
                                    (\ (Z :. i1 :. _) ( Z :. _ :. j2) _ -> (Z :. i1 :. j2))
                                    oper
                                    where oper _ _ _ (Z :. i :. j) = f (nRow mat1 i) (nCol xs j) (nCol bs j) (i, j)

foldMatrixByRow :: (Array D DIM1 Double -> Array D DIM1 Double -> Int -> Array D DIM1 Double)
                        -> Array D DIM1 Double
                        -> Array U DIM2 Double
                        -> Array D DIM1 Double
foldMatrixByRow f intial mat = foldMatrixByRowUtil f intial (mat, 0)
                                  where foldMatrixByRowUtil f initial (mat, i)
                                          | i == n = initial
                                          | otherwise = foldMatrixByRowUtil f result (mat, i + 1)
                                          where result = (f matRow initial i)  :: Array D DIM1 Double
                                                matRow = (delayNRow mat i):: Array D DIM1 Double
                                                (Z :. n :. _) = extent mat
