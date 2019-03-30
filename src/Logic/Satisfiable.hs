module Logic.Satisfiable 
( single
, both
, oneOf
, neg
, satisfiable
, CompoundProp
) where

import qualified Data.Map as M
import Data.List
import Control.Monad

-- Ex3. determine whether a compound proposition is satisfiable

-- type alias and data definition
type Symbol = String

data CompoundProp = Prop Symbol 
  | And CompoundProp CompoundProp 
  | Or CompoundProp CompoundProp 
  | Not CompoundProp deriving (Show, Read)

type LogicOp = Bool -> Bool -> Bool
type LogicF = [Bool] -> Bool
type Func = (LogicF, [Symbol])


-- core of this module, it parse a proposition structure to a `Func`
parseToFunc :: CompoundProp -> Func
parseToFunc (Prop sym) = (head, [sym])
parseToFunc (And cp1 cp2) = parseHelper cp1 cp2 (&&)
parseToFunc (Or cp1 cp2) = parseHelper cp1 cp2 (||)
parseToFunc (Not cp) = (not . f, sym) where (f, sym) = parseToFunc cp

parseHelper :: CompoundProp -> CompoundProp -> LogicOp -> Func
parseHelper cp1 cp2 op = 
  let func1@(_, sym1) = parseToFunc cp1
      func2@(_, sym2) = parseToFunc cp2
      sym = union sym1 sym2
  in (fGenerator func1 func2 sym op, sym)


-- logic function generators, which transform `Func` to real function
fGenerator :: Func -> Func -> [Symbol] -> LogicOp -> ([Bool] -> Bool)
fGenerator func1@(f1, sym1) func2@(f2, sym2) sym op bools = 
  let argMap  = M.fromList $ zip sym bools
      params1 = getArguments sym1 argMap
      params2 = getArguments sym2 argMap
  in f1 params1 `op` f2 params2


-- given some given arguments, get the arguments we need  based on symbol table
getArguments :: [Symbol] -> M.Map Symbol Bool -> [Bool]
getArguments sym argMap = foldr action [] sym
  where  action cur acc = if M.member cur argMap then (argMap M.! cur) : acc else acc


-- apply `Func` to some arguments
data ApplyError = TooLessArgs

applyFunc :: Func -> M.Map Symbol Bool -> Either ApplyError Bool
applyFunc (f, xs) m 
  | sort xs == sort (M.keys m) = Left TooLessArgs
  | otherwise = Right (f arguments) 
      where arguments =  map (m M.!) . filter (`M.member` m) $ xs
  

-- we have done the parsing, now we need to validate
validate :: Func -> Bool  
validate (f, xs) = 
  let allBools = replicateM (length xs) [True, False]
      action cur acc = f cur || acc
  in foldr action False allBools


-- user interface
single = Prop
both = And
oneOf = Or
neg = Not

satisfiable :: CompoundProp -> Bool
satisfiable = validate . parseToFunc


-- some example
p = single "p"
q = single "q"
r = single "r"

first = both (both (oneOf p (neg q)) (oneOf q (neg r))) (oneOf r (neg p))
second = both (oneOf p (oneOf q r)) (oneOf (neg p) (oneOf (neg q) (neg r)))
third = both first second