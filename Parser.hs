module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import qualified Data.Map as M

-- Definire Program
type ClassName = String
type ParentName = String
type ExprParam = String

type Clasa = M.Map ClassName (ParentName, ClassState)

data Program = Program Clasa deriving (Show, Eq)

initEmptyProgram :: Program 
initEmptyProgram = Program $ M.singleton "Global" ("", initEmptyClass)


getVars :: Program -> [[String]]
getVars (Program pr) = getValues (snd (pr M.! "Global") ) Var


getClasses :: Program -> [String]
getClasses (Program pr) = map (\(x,(y, z)) -> x) $ M.toList pr


getParentClass :: String -> Program -> String
getParentClass cl (Program pr)
	| cl == "Global" = "Global"
	| otherwise = fst (pr M.! cl)


getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass cl (Program pr)
	| M.member cl pr = getValues (snd (pr M.! cl) ) Func
	| otherwise = []


data Instruction = ClassDeclare ClassName ParentName |
	VarDeclare SimbVar TipVar |
	InferDeclare SimbVar [ExprParam] |
	FuncDeclare TipRet ClassName SimbFunc [TipParam] deriving (Show, Eq)


-- Parseaza sirul s folosind delimitatorii din c
words' :: String -> String -> [String]
words' c s = wordsAux c s [] [] where
	wordsAux c [] crt acc = if null crt then acc else acc ++ [crt]
	wordsAux c (x:xs) crt acc
	    | x `elem` c = if crt == [] then wordsAux c xs [] acc
	    	else wordsAux c xs [] (acc ++ [crt])
	    | otherwise = wordsAux c xs (crt ++ [x]) acc


-- Parseaza aproape la fel, dar pastreaza caracterele "." si ")"
words'' :: String -> String -> [String]
words'' c s = wordsAux c s [] [] where
	wordsAux c [] crt acc = if null crt then acc else acc ++ [crt]
	wordsAux c (x:xs) crt acc
	    | x `elem` c = if x `elem` ".)" then if crt == [] then
	    	wordsAux c xs [] (acc ++ [[x]]) else
    		wordsAux c xs [] (acc ++ [crt] ++ [[x]]) else if crt == [] then
			wordsAux c xs [] acc else wordsAux c xs [] (acc ++ [crt])
	    | otherwise = wordsAux c xs (crt ++ [x]) acc


-- Parseaza sirul s in functie de tipul comenzii
split' :: String -> [[String]]
split' s = map choose (words' "\n" s)
	where choose x
		| take 5 x == "infer" = words'' " .=:,()\t" x
		| otherwise = words' " .=:,()\t" x


-- Creeaza o instrunctiune pe baza cuvantului cheie din comanda data
parseAux :: [String] -> Instruction
parseAux s
	| h == "class" = if length s == 2 then ClassDeclare ht "Global"
		else ClassDeclare ht l
	| h == "newvar" = VarDeclare ht l
	| h == "infer" = InferDeclare ht tt
	| otherwise = FuncDeclare h ht htt ttt where
		h = head s
		t = tail s
		l = last s
		ht = head t
		tt = tail t
		htt = head tt
		ttt = tail tt


-- Verifica daca parametrii functiei sunt corecti
verif :: [String] -> Clasa -> Bool
verif params pr = length (filter (\x -> x == False) $
	map (\x -> M.member x pr) params) > 0


-- Verifica daca exista o functie cu aceeasi semnatura
exists :: ClassName -> (SimbFunc, [TipParam]) -> Clasa -> Bool
exists cn key pr
	| null cn = False
	| otherwise = M.member key (getF (snd (pr M.! cn)))


-- Determina tipul returnat al unei functii
getTipRet :: ClassName -> (SimbFunc, [TipParam]) -> Clasa -> Maybe String
getTipRet cn key pr
	| null cn = Nothing
	| otherwise = if M.notMember key (getF (snd (pr M.! cn))) then
		getTipRet (fst (pr M.! cn)) key pr else Just ((getF (snd (pr M.! cn)))
		M.! key)


-- Insereaza variabila "sb" de tip "tip"
insVar :: SimbVar -> TipVar -> Program -> Program
insVar sb tip (Program pr) = Program (M.insert "Global" ("", insertIntoClass
			(snd (pr M.! "Global")) Var [sb, tip]) pr)


-- Determina indicele ")" corespunzatoare functiei
ind :: [String] -> Int
ind str = valAux (drop 3 str) 0 0 where
	valAux str count index
		| head str == "." = valAux (tail str) (count + 1) (index + 1)
		| head str == ")" = if count == 0 then index + 4 else
			valAux (tail str) (count - 1) (index + 1)
		| otherwise = valAux (tail str) count (index + 1)


-- Creeaza o lista de expresii dintr-o lista de string-uri
makeExpr :: [TipParam] -> [Expr]
makeExpr str
	| length str == 0 = []
	| length str == 1 = [Va h]
	| ht /= "." = (Va h) : (makeExpr t)
	| otherwise = (FCall h htt (makeExpr left)) : (makeExpr right) where
		h = head str
		t = tail str
		ht = head t
		htt = head $ tail t
		left = drop 3 $ take ((ind str) - 1) str
		right = drop (ind str) str


parse :: String -> [Instruction]
parse str = map parseAux (split' str)


interpret :: Instruction -> Program -> Program
interpret instr p @ (Program pr) = case instr of
	ClassDeclare cn pn -> if M.member cn pr then p else
		Program (M.insert cn (parent, initEmptyClass) pr) where
			parent = if M.member pn pr then pn else "Global"

	VarDeclare sb tip -> if (M.notMember tip pr) || (M.member sb (getV (snd
		(pr M.! "Global")))) then p else insVar sb tip p

	FuncDeclare tr cn sb pm -> if M.notMember tr pr || M.notMember cn pr ||
		verif pm pr then p else if exists cn (sb, pm) pr then p else
		Program (M.insert cn (fst (pr M.! cn),
		insertIntoClass (snd(pr M.! cn)) Func ([sb, tr] ++ pm)) pr)

	InferDeclare var pm -> if result == Nothing then p else
		insVar var (fromJust result) p where
			result = infer (FCall (head pm) (head (tail (tail pm))) rest) p
				where rest = makeExpr $ init (drop 3 pm)


infer :: Expr -> Program -> Maybe String
infer (Va var) (Program pr)
	| M.notMember var (getV (snd (pr M.! "Global"))) = Nothing
	| otherwise = Just $ (getV (snd (pr M.! "Global"))) M.! var

infer (FCall v f l) (Program pr)
	| M.notMember v (getV (snd (pr M.! "Global"))) = Nothing
	| params == Nothing = Nothing
	| otherwise = getTipRet ((getV (snd (pr M.! "Global"))) M.! v) 
		(f, fromJust params) pr
		where params
			| length (filter (\x -> x == Nothing) list) > 0 = Nothing
			| otherwise = Just $ map fromJust list
				where list = map (\x -> infer x (Program pr)) l

