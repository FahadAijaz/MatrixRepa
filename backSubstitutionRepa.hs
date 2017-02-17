import           Data.Array.Repa as R
import           RowTraversal


swap1And3 f x1 x2 x3 = f x3 x2 x1

{-\backSubstitution matrix bs = traverseMat2RC matrix rowOp
                                where rowOp row rowIndex = R.traverse3 row bs const g
                                      g = (\f1 f2 (Z :. currCol) -> if currCol == rowIndex then
                                                else f1 (Z :. currCol) * f2 (Z :. currCol))-}

--Does not get updated with the new x
{-\solutionIteration matrix xs bs = traverseMatrixByRow matrix (rowOp xs bs)
                                    where rowOp xs bs matRow i = ((bs R.! i) - lhs) / (matRow R.! i)
                                                where lhs = sumAllS $ traverse3Backwards matRow xs bs (mult i)
                                                      mult i m x b (j, k)
                                                            | i <= j = 0
                                                            | otherwise =  x * m-}



backsubstitution :: Array U DIM2 Double -> Array U DIM1 Double -> Array D DIM1 Double															
backsubstitution origMat origBs = foldMatrixByRow f initial mat
                              where  
                                    f matRow sols i = (fromFunction (extent bs) intArr ):: Array D DIM1 Double
                                          where 
                                                intArr (Z :. j) | i == j = rowComp | j < i = 0.0 | otherwise = sols R.! (Z :. j) 
                                                intc = ((bs ! (Z :. i)) - (rowComp)) / (matRow ! (Z :.i))
                                                rowComp = sumAllS $ traverse2Backwards matRow sols g 	 
                                                g m s (i', _) = if i < i' then m * s else 0.0
											
                                    initial = ( fromFunction (extent bs) (\ _ -> 0) ):: Array D DIM1 Double
                                    mat =computeS $ mReverseRow origMat
                                    bs = vReverse $ delay origBs
                                    
									
{-\backSubstituition matrix bs =  matrix bs rowOp
                              where rowOp mRow bCol (i1 ,j2) =
                                     traverse2Backwards mRow bCol g
                                     where g a b (j1, i2) |
                                                j1 < i1 = 0
                                                j1 > i1 -}



--forwardSubstitution matrix bs =

main :: IO ()
main = do
      return ()
