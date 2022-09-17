import Syntax.Abs
import Semantics
import Control.Monad
import Data.Set

emptyEnv :: Env
emptyEnv = Env []

data TestCase = TestCase
  { program :: Exp
  , testEnv :: Env
  , denotationRes :: Integer
  , bigStepRes :: Exp
  , smallStepRes :: Set (Env, Exp)
  } deriving (Show)

tests :: [TestCase]
tests =
  [ TestCase (EInt 5)                   emptyEnv
    5  (EInt 5)  empty
  , TestCase (EPlus (EInt 5) (EInt 4))  emptyEnv
    9  (EInt 9)  (fromList [(emptyEnv, EInt 9)])
  , TestCase (ETimes (EInt 5) (EInt 4)) emptyEnv
    20 (EInt 20) (fromList [(emptyEnv, EInt 20)])
  , TestCase (ETimes (EPlus (EInt 6) (EInt 4)) (EPlus (EInt 6) (EInt 1))) emptyEnv
    70 (EInt 70) (fromList [ (emptyEnv, (ETimes (EInt 10) (EPlus (EInt 6) (EInt 1))))
                           , (emptyEnv, (ETimes (EPlus (EInt 6) (EInt 4)) (EInt 7)))
                           ])
  , TestCase (EPlus (ETimes (EInt 5) (EVar (Ident "x"))) (EVar (Ident "y"))) e1
    21
    (EInt 21)
    (fromList [ (e1, EPlus (ETimes (EInt 5) (EInt 4)) (EVar (Ident "y")))
              , (e1, EPlus (ETimes (EInt 5) (EVar (Ident "x"))) (EInt 1))
              ])
  ]
  where
    e1 :: Env
    e1 = Env [Assign (Ident "x") 4, Assign (Ident "y") 1]

test :: (Eq a, Show a) => a -> a -> TestCase -> IO ()
test result expected tc =
  if expected == result
  then return ()
  else error $
       "Failed test: " ++ show tc ++
       " Got " ++ show result ++
       " instead of " ++ show expected

runTest :: TestCase -> IO ()
runTest tc =
  test (denotation (testEnv tc, program tc)) (denotationRes tc) tc >>
  test (bigStep (testEnv tc, program tc))    (bigStepRes tc)    tc >>
  test (smallStep (testEnv tc, program tc))  (smallStepRes tc)  tc

main :: IO ()
main = putStrLn "Running test..." >> forM_ tests runTest
