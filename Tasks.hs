
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Read

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}

-- Task 1

-- Functie care imi transforma un Float in String
toStr :: Float -> String
toStr x = (printf "%.2f") x

-- Functie care imi transforma un String in Float
rFloat :: String -> Float
rFloat = read

-- Functie care imi calculeaza suma pasilor la o persoana impart la 8 pentru a afla media 
-- si imi intoarce numele  persoanei impreuna cu nr de pasi medii
concateStr :: [String] -> [String]
concateStr x = head x : toStr ((foldr (+) 0 (map rFloat(tail x))) / 8) : [] 

-- Aflu nr de pasi la toti
compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : (map concateStr (tail m))


-- Task 2

-- Primesc o persoana ma uit daca nr de pasi total sunt mai mult de 1000 daca da atunci adaug
-- intr-o lista auxiliara numele lui, daca nu pun o lista goala
sumStep :: [String] -> [String]
sumStep x  
     |foldr (+) 0 (map rFloat(tail x)) > 1000 = head x : []
     |otherwise = []

-- Calculez cate persoane am in lista care v-a fi prelucrata cu functia de mai sus, daca e lista
-- goala atunci nu adaug la acumulator.
countLists :: Table -> Int -> Int
countLists [] acc = acc
countLists (x:xs) acc 
                | x == [] = countLists xs acc
                | otherwise = countLists xs (acc+1)

-- Calculez suma unui rand si intorc o lista de tip float
allStep :: [String] -> [Float]
allStep x = (foldr (+) 0 (map rFloat(tail x))) : []

-- Calculez suma tuturor persoanelor
allCount :: [[Float]] -> Float -> Float
allCount [] acc = acc
allCount (x:xs) acc = allCount xs (acc + head x)

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = countLists (map sumStep (tail m)) 0

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m)::Float)
                                   / fromIntegral (length (tail m))::Float

-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (allCount (map allStep (tail m)) 0) / fromIntegral (length (tail m))::Float

-- Task 3

-- Functie care imi transpune un Table
mTranspose :: Table -> Table
mTranspose ([]:_) = []
mTranspose x = (map head x) : transpose (map tail x)

-- Calculeaza suma unui rand si imi intoarece un Float
allStep1 :: [String] -> Float
allStep1 x = (foldr (+) 0 (map rFloat(tail x)))

-- Calculez suma pe fiecare ora 
sCon :: Table -> [Float] -> [Float]
sCon [] acc = acc
sCon (x:xs) acc = (allStep1 x ) : (sCon xs acc)

-- Calculez media pentru fiecare zi si o bag in acumulator
fAux :: [Float] -> Float -> [Float] -> [Float]
fAux [] l acc = acc
fAux (x:xs) l acc = (x / l) : (fAux xs l acc)

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : 
                        [(map (toStr) (take 8 (fAux (sCon (tail (mTranspose m)) [0])
                         (fromIntegral (length (tail m))::Float) [0])))]

-- Task 4

-- Primesc un String si compar fiecare element in dependenta de ce valoare am adun la 
-- acumulator
forEach :: [String] -> [String] -> Int -> Int -> Int -> [String]
forEach [] fl acc1 acc2 acc3 = show acc1 : show acc2 : show acc3 : fl
forEach (x:xs) fl acc1 acc2 acc3 
                              |(rFloat x) < 49.99 = forEach xs fl (acc1+1) acc2 acc3
                              |(rFloat x) < 99.99 = forEach xs fl acc1 (acc2+1) acc3
                              |otherwise = forEach xs fl acc1 acc2 (acc3+1)

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] :
                      ("VeryActiveMinutes" : forEach (tail (mTranspose m !! 3)) [] 0 0 0) :
                      ("FairlyActiveMinutes" : forEach (tail (mTranspose m !! 4)) [] 0 0 0) :
                      [("LightlyActiveMinutes" : forEach (tail (mTranspose m !! 5)) [] 0 0 0)]

-- Task 5

-- Functia de comparare care compara TotalSteps daca sunt egali compar dupa nume
cmp a b 
      |rFloat (head (tail a)) <  rFloat (head (tail b)) = LT
      |(rFloat (head (tail a)) == rFloat (head (tail b))) && (head a) < (head b) = LT
      |(rFloat (head (tail a)) == rFloat (head (tail b))) && (head a) > (head b) = GT
      |otherwise = GT

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (sortBy cmp (tail (map (take 2) m)))

-- Task 6
--Compar dupa diferenta daca sunt egale dupa nume
cmp1 a b 
      |rFloat (head (reverse a)) <  rFloat (head (reverse b)) = LT
      |(rFloat (head (reverse a)) == rFloat (head (reverse b))) && (head a) < (head b) = LT
      |(rFloat (head (reverse a)) == rFloat (head (reverse b))) && (head a) > (head b) = GT
      |otherwise = GT

-- Suma a 4 elemente din string, inpart la 4 pentru a afla media
sumF :: [String] -> Float
sumF x = (foldr (+) 0 (map rFloat (x))) / 4

-- Creez un String dupa cum a fost aratat in cerinta
concatAll :: [String] -> [String]
concatAll x = (head x) : toStr(sumF(take 4 (tail x))) : toStr(sumF(drop 5 (x))) :
               toStr(abs((sumF(take 4 (tail x)) - sumF(drop 5 (x))))) : []

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] :
                           sortBy cmp1 (map concatAll (tail m))

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f (tail m))

-- Calculez suma tuturor minutelor
get_sleep_total :: Row -> Row
get_sleep_total r = head r : [printf "%.2f" (allStep1  r)]


{-
    TASK SET 2
-}

-- Task 1

-- Caut pe ce pozitie se afla coloana de care am nevoie
findInd :: ColumnName -> [String] -> Int -> Int
findInd colum [] acc = acc
findInd colum (x:xs) acc 
                         | not (colum == x) = findInd colum xs (acc+1)
                         | otherwise = acc

-- Daca este un string apelez cmpS daca este un double cmpInd
tsort :: ColumnName -> Table -> Table
tsort column table = if (isNr(head(tail table) !! (findInd column (head table) 0))) == False 
                        then (head table) : (sortBy cmpS (tail table)) else (head table) :
                         (sortBy cmpInd (tail table))
                    where
                        cmpInd a b
                                  | rFloat (a !! (findInd column (head table) 0)) < rFloat
                                   (b !! (findInd column (head table) 0)) = LT
                                  | rFloat (a !! (findInd column (head table) 0)) > rFloat
                                   (b !! (findInd column (head table) 0)) = GT
                                  | otherwise = if (head a) < (head b) then LT else if (head a) == (head b) then EQ else GT
                        cmpS a b
                                | (a !! (findInd column (head table) 0)) < (b !! (findInd column
                                 (head table) 0)) = LT
                                | (a !! (findInd column (head table) 0)) > (b !! (findInd column
                                 (head table) 0)) = GT
                                | otherwise = if (head a) < (head b) then LT else if (head a) == (head b) then EQ else GT

-- Intoarce true daca e un double                                  
isNr :: String -> Bool
isNr str 
       | (readMaybe str :: Maybe Double) == Nothing = False
       | otherwise = True

-- Task 2

-- Concatenez t1 cu t2 
vunion :: Table -> Table -> Table
vunion t1 t2 
            | (head t1) == (head t2) = t1 ++ (tail t2)
            | otherwise = t1


-- Task 3

-- Adaug in tabelul cu lungimea mai mica stringuri goale, si concatenez fiecare row
hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) (t1 ++ (take (sizeTable t1 t2) (repeat (take (length(head t1))
                             (repeat ""))))) 
                            (t2 ++ (take (sizeTable t1 t2) (repeat (take (length(head t2))
                             (repeat ""))))) 

-- Aflu diferenta dintre lungimi
sizeTable :: Table -> Table -> Int 
sizeTable t1 t2 
                | length t1 > length t2 = length t1 - length t2
                | length t1 < length t2 = length t2 - length t1
                | otherwise = 0


-- Task 4 
-- Daca sunt egale coloanele atunci extrag pe bucati stringul 2 fara coloana care deja este.
mergeStrings :: Int -> Int -> [String] -> Table -> [String]
mergeStrings nr nr2 s1 [] = []
mergeStrings nr nr2 s1 (x:xs) 
                            | ((s1 !! nr) == (x !! nr2)) = (s1 ++ (((take (nr2) x) ++ (drop (nr2+1) x))))
                            | otherwise = mergeStrings nr nr2 s1 xs 

-- Apelez mergeStrings pentru fiecare string din table
tjoinaux :: ColumnName -> Table -> Table -> Table -> Table
tjoinaux key_column taux [] t2 = []
tjoinaux key_column taux (x:xs) t2 
                                | (mergeStrings (findInd key_column (head taux) 0) 
                                (findInd key_column (head t2) 0) x t2) == [] = tjoinaux key_column taux xs t2
                                | otherwise = (mergeStrings (findInd key_column (head taux) 0) 
                                (findInd key_column (head t2) 0) x t2) : (tjoinaux key_column taux xs t2)

-- Controlez daca coloana exista in t1 daca da o schimb
replace :: Table -> [String] -> Table -> Table
replace [] s1 acc = reverse acc
replace (x:xs) s1 acc 
                | (head x) == (head s1) = replace xs s1 (s1 : acc)
                | otherwise = replace xs s1 (x : acc)

-- Apelez functia de mai sus pe fiecare string din Table2.
replaceall :: Table -> Table -> Table
replaceall t1 t2 = mTranspose (head (drop ((length (map (\x -> replace (mTranspose t1) x [])
                     (mTranspose t2)))-1) (map (\x -> replace (mTranspose t1) x []) (mTranspose t2))))

-- Apelez functia ajutatoare care imi raspunde de tot
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = mTranspose(take ((findInd ((head(tjoinaux key_column t1 (replaceall t1 t2) t2)) !! 1)
                         (drop 2 (head(tjoinaux key_column t1 (replaceall t1 t2) t2))) 0)+2) (mTranspose(tjoinaux key_column t1 (replaceall t1 t2) t2)))


-- Task 5 

-- Apelez functia cartesianAux pe tail t1 pentru a scapa de head.
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names :
                                (cartesianAux new_row_function (tail t1) t2)

-- Amplic functia data pe [String] primit ca parametru si fiecare row din tabelul dat.
cartFunc :: [String] -> (Row -> Row -> Row) -> Table -> Table
cartFunc a new_row_function [] = []
cartFunc a new_row_function t2 = map (\x -> new_row_function a x) (tail t2)

-- Aplic functia carFunc pe fiecare row din tabelul 1.
cartesianAux :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesianAux new_row_function [] t2 = []
cartesianAux new_row_function t1 t2 = cartFunc (head t1) new_row_function t2
                                ++ cartesianAux new_row_function (tail t1) t2 
-- Task 6

-- Filtrez tabelul lasand doar coloanele cu numele dat in [ColumnName] 
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = mTranspose (filter (\x -> elem (head x)
                                         columns_to_extract) (mTranspose t)) 

-- Task 7 

-- Filtrez tabelul dupa functia data, cu ajutorul coloanei date ca parametru
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filter (\x -> condition (x !!
                                     (findInd key_column (head t) 0))) (tail t))

-- Task 8 TO_DO

{-
    TASK SET 3
-}

-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

parse_res (Table table) = table

instance Eval Query where
    eval (FromTable table) = Table table
    eval (AsList colname query) = List ((mTranspose (tail (parse_res(eval query)))) !! (findInd colname (head (parse_res(eval query))) 0))
    eval (Sort colname query) = Table (tsort colname (parse_res(eval query)))
    eval (ValueMap op query) = Table (vmap op (parse_res(eval query)))
    eval (RowMap op colnames query) = Table (rmap op colnames (parse_res(eval query)))
    eval (VUnion query query1) = Table (vunion (parse_res(eval query)) (parse_res(eval query1))) 
    eval (HUnion query query1) = Table (hunion (parse_res(eval query)) (parse_res(eval query1)))
    eval (TableJoin colname query query1) = Table (tjoin colname (parse_res(eval query)) (parse_res(eval query1)))
    eval (Cartesian op colnames query query1) = Table (cartesian op colnames (parse_res(eval query)) (parse_res(eval query1)))
    eval (Projection colnames query) = Table (projection colnames (parse_res(eval query)))
    eval (Filter condition query) = Table ((head (parse_res(eval query))) : (filter (feval (head (parse_res(eval query))) condition ) (tail (parse_res(eval query)))))
    eval (Graph edgeOp query) = Table (nub (["From","To","Value"] : (forAllRows edgeOp (tail(parse_res(eval query))))))
-- -- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval columns (Eq colname ref) r = ref == rFloat(r !! (findInd colname (columns) 0))
    feval columns (Lt colname ref) r = ref > rFloat(r !! (findInd colname (columns) 0))
    feval columns (Gt colname ref) r = ref < rFloat(r !! (findInd colname (columns) 0))
    feval columns (In colname list) r = elem (rFloat(r !! (findInd colname (columns) 0))) list
    feval columns (FNot cond) r = not (feval columns cond r)
    feval columns (FieldEq s1 s2) r = (rFloat(r !! (findInd s1 (columns) 0))) == (rFloat(r !! (findInd s2 (columns) 0)))

instance FEval String where
    feval columns (Eq colname ref) r = ref == (r !! (findInd colname (columns) 0))
    feval columns (Lt colname ref) r = ref > (r !! (findInd colname (columns) 0))
    feval columns (Gt colname ref) r = ref < (r !! (findInd colname (columns) 0))
    feval columns (In colname list) r = elem (r !! (findInd colname (columns) 0)) list
    feval columns (FNot cond) r = not (feval columns cond r)
    feval columns (FieldEq s1 s2) r = (r !! (findInd s1 (columns) 0)) == (r !! (findInd s2 (columns) 0))

-- -- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- Parsez din maybe in element
fromMaybe (Just el) = el

-- Daca functia edge imi intoarce ceva atunci sortez head-urile stringurilor si adaug in acc
graphSolver :: EdgeOp -> Table -> Row -> Table -> Table
graphSolver op [] r acc =  reverse acc
graphSolver op (x:xs) r acc
                            | op x r == Nothing = graphSolver op xs r acc
                            | otherwise = graphSolver op xs r (((sort[(head x),(head r)]) ++ [(fromMaybe(op x r))]) : acc)

-- Apelez functia de mai sus pe fiecare row din tabel
forAllRows :: EdgeOp -> Table -> Table
forAllRows op [] = []
forAllRows op t1 = (graphSolver op (tail t1) (head t1) []) ++ (forAllRows op (tail t1)) 

-- 3.5

-- Ma uit cate coloane au aceiasi valoare intre doua randuri
eq_columns :: Row -> Row -> Int -> Int
eq_columns [] r2 acc = acc
eq_columns r1 [] acc = acc
eq_columns (y:r1) (x:r2) acc = if(x == y) then eq_columns r1 r2 (acc+1) else eq_columns r1 r2 acc

-- Ma uit daca am mai mult de 5 coloane la fel, daca da atunci transform int in string
edge_new :: Row -> Row -> Maybe String
edge_new r1 r2 
            | (eq_columns (tail r1) (tail r2) 0) >= 5 = Just (show (eq_columns (tail r1) (tail r2) 0))
            | otherwise = Nothing

-- Apelez functia Graph care imi raspunde de crearea grafului, elimin cu filtru tot ce e gol, si sortez
similarities_query :: Query
similarities_query = Sort "Value" (Filter (FNot (Eq "From" "")) (Filter (FNot (Eq "To" "")) (Graph edge_new (FromTable eight_hours))))

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = undefined

