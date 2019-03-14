import System.IO
import System.Environment
import System.Exit

import Data.List
import qualified Data.Set as Set



gramar = ["A,B,S","a,b","S","A->AB","B->b","S->A","S->a"]

gramar2 = ["B,S","a,b","S","B->b","S->a"]


gramarNonTerms = ['A','B','S']
gramarTerms = ['a','b']
gramarRules = [('A',"AB"),('B',"b"),('S',"A"),('S',"a")]

gramarNonTerms2 = ['A','B','S', 'D']
gramarRules2 = [('A',"AB"),('B',"b"),('S',"A"),('S',"a"), ('A',"D"), ('D',"A")]




main :: IO()
main = do
    argv <- getArgs
    let file = getFileAndValidate argv
    input <- getInput file
    let asijo = process (head argv) input
    putStr asijo
    putStr (grammarAlgorithm1 $ parseByLines input)


process :: String -> String -> String
process arg input
            | "-i" == arg = "Load -i"
            | "-1" == arg = "Algoritmus 1"
            | "-2" == arg = "Algoritmus 2"
            | otherwise = error "Error: Invalid parameter in process!"


parseByLines :: String -> [String]
parseByLines input = lines (input)


getFileAndValidate :: [String] -> String
getFileAndValidate args 
                    | isArgsValid args = getFile args
                    | otherwise = error "Error: Invalid program arguments!"


isArgsValid :: [String] -> Bool 
isArgsValid ["-i"] = True 
isArgsValid ["-i", file] = True 
isArgsValid ["-1"] = True 
isArgsValid ["-1", file] = True 
isArgsValid ["-2"] = True 
isArgsValid ["-2", file] = True 
isArgsValid others = False 


getFile :: [String] -> String
getFile [] = ""
getFile (x:file)
        | not (elem x ["-i", "-1", "-2"]) = x
        | otherwise = getFile file


getInput :: String -> IO String
getInput "" = getContents
getInput file = readFile (file)


-- vi is Vi set from TIN arlgorithm 4.2
grammarAlgorithm2 :: [String] -> String
grammarAlgorithm2 grammar = do
                    let nonterminals = makeSetFromList (getArrayNonterminals ( head grammar ))
                    let terminals = makeSetFromList (getArrayTerminals ( head $ drop 1 grammar ))
                    let startNonterminal = makeSetFromList ( getArrayNonterminals ( head $ drop 2 grammar ))
                    let rules =  parseRules (drop 3 grammar)
                    let vi = makeSetVi startNonterminal rules
                    charArrToStringWithCommas (Set.toList (Set.intersection vi nonterminals)) ++ charArrToStringWithCommas (Set.toList (Set.intersection vi terminals)) ++ charArrToStringWithCommas (Set.toList startNonterminal) ++ listOfStringsToString ( rulesToArrayOfStrings (filterRulesBySymbols (Set.toList vi) rules))



makeSetFromList :: (Ord a) => [a] -> Set.Set a
makeSetFromList [] = Set.empty
makeSetFromList (x:xs) = Set.insert x (makeSetFromList xs)


makeSetVi :: Set.Set Char -> [(Char, String)] -> Set.Set Char
makeSetVi viOld rules = do
                        let vi = Set.union (iterateByViElemsToMakeVi (Set.toList viOld) rules) viOld
                        if viOld == vi
                            then vi
                            else makeSetVi vi rules


iterateByViElemsToMakeVi :: [Char] -> [(Char, String)]  -> Set.Set Char
iterateByViElemsToMakeVi [] rules = Set.empty
iterateByViElemsToMakeVi (viElem:rest) rules = Set.union (iterateByRulesToMakeVi viElem rules) (iterateByViElemsToMakeVi rest rules)


iterateByRulesToMakeVi :: Char -> [(Char, String)]  -> Set.Set Char
iterateByRulesToMakeVi viElem [] = Set.empty
iterateByRulesToMakeVi viElem (rule:rest) = Set.union (addElemsToVi Set.empty viElem rule) (iterateByRulesToMakeVi viElem rest)


addElemsToVi :: Set.Set Char -> Char -> (Char, String) -> Set.Set Char
addElemsToVi vi viElem (left, right) = do
                                if viElem == left
                                   then Set.union vi (insertStringToSetByChars right)
                                   else vi


insertStringToSetByChars :: String -> Set.Set Char
insertStringToSetByChars [] = Set.empty
insertStringToSetByChars (x:xs) = Set.insert x (insertStringToSetByChars xs)


-- nt is Nt set from TIN arlgorithm 4.1
grammarAlgorithm1 :: [String] -> String
grammarAlgorithm1 grammar = do
                            let terminals = getArrayTerminals ( head $ drop 1 grammar )
                            let startNonterminal = getArrayNonterminals ( head $ drop 2 grammar )
                            let rules =  parseRules (drop 3 grammar)
                            let nt = makeSetNt terminals rules [] 
                            charArrToStringWithCommas nt ++ charArrToStringWithCommas terminals ++ charArrToStringWithCommas startNonterminal ++ listOfStringsToString ( rulesToArrayOfStrings (filterRulesBySymbols (nt ++ terminals) rules) )

filterRulesBySymbols :: [Char] -> [(Char, String)] -> [(Char, String)]
filterRulesBySymbols keys rules = [ (left,right) | (left,right) <- rules, elem left keys, checkElems right keys]

listOfStringsToString :: [String] -> String
listOfStringsToString [] = ""
listOfStringsToString (x:xs) = x ++ listOfStringsToString xs

rulesToArrayOfStrings :: [(Char, String)] -> [String]
rulesToArrayOfStrings rules = [ [left] ++ "->" ++ right ++ "\n" | (left, right) <- rules]

charArrToStringWithCommas :: [Char] -> String
charArrToStringWithCommas [] = []
charArrToStringWithCommas [x] = [x] ++ "\n"
charArrToStringWithCommas (x:xs) = [x] ++ "," ++  charArrToStringWithCommas xs


makeSetNt :: [Char]->[(Char, String)]->[Char]->[Char]
makeSetNt terminals rules ntOld = do
                            let nt = [ nonterminal | (nonterminal, right) <- rules, checkElems right (terminals ++ ntOld)]
                            if ntOld == nt
                                then nt
                                else makeSetNt terminals rules nt

-- Check if elems of set are in other set
checkElems :: [Char] -> [Char] -> Bool
checkElems [] x = True
checkElems (x:xs) yList = (elem x yList) && ( checkElems xs yList)

getArrayNonterminals :: String -> [Char]
getArrayNonterminals nonterminals = [ nonterminal | nonterminal <- nonterminals , elem nonterminal ['A'..'Z'] ]

getArrayTerminals :: String -> [Char]
getArrayTerminals terminals = [ terminal | terminal <- terminals , elem terminal ['a'..'z'] ]

parseRules :: [String] -> [(Char, String)]
parseRules rules = [parseRule rule | rule <- rules]

parseRule :: String -> (Char,String)
parseRule rule = (parseLeft rule, parseRight rule)

parseLeft :: String -> Char
parseLeft rule = head rule

parseRight :: String -> String
parseRight rule = drop 3 rule