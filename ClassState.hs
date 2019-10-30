module ClassState
where

import qualified Data.Map as M

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

type TipVar = String
type SimbVar = String
type TipParam = String
type TipRet = String
type SimbFunc = String

type Vars = M.Map SimbVar TipVar
type Funcs = M.Map (SimbFunc, [TipParam]) TipRet

data ClassState = ClassState Vars Funcs deriving (Show, Eq)


initEmptyClass :: ClassState
initEmptyClass = ClassState M.empty M.empty


insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState var func) instr l
	| instr == Var = ClassState (M.insert (head l) (head t) var) func
	| otherwise = ClassState var (M.insert (head l, tail t) (head t) func)
		where t = tail l


getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState var func) instr
	| instr == Var = map (\(x, y) -> [x,y]) $ M.toList var
	| otherwise = map (\((x, y), z) -> ([x,z] ++ y)) $ M.toList func


-- getter pentru variabile
getV (ClassState var func) = var


-- getter pentru functii
getF (ClassState var func) = func
