import System.IO
import System.Environment
import System.Exit

import Data.List
import qualified Data.Set as Set


main :: IO()
main = do
    argv <- getArgs
    let file = getFileAndValidate argv
    input <- getInput file
    let output = process (head argv) (parseByLines input)
    putStr $ grammarToString output



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


getFileAndValidate :: [String] -> String
getFileAndValidate args 
                    | isArgsValid args = getFile args
                    | otherwise = error "Error: Invalid program arguments!"


type Nonterminal = Char
type Terminal = Char
type Rule = (Nonterminal,String)


data Grammar = Grammar{
    grammarNonterminals::Set.Set Nonterminal,
    grammarTerminals::Set.Set Terminal,
    grammarOrigin::Nonterminal,
    grammarRules::Set.Set Rule
}


instance Show Grammar where
    show (Grammar grammarNonterminals grammarTerminals grammarOrigin grammarRules) = 
        "Nonterminals:\n" ++ show grammarNonterminals ++ "\n" ++
        "Terminals:\n" ++ show grammarTerminals ++ "\n" ++
        "Origin:\n" ++ show grammarOrigin ++ "\n" ++
        "Rules:\n" ++ show grammarRules ++ "\n"


process :: String -> [String] -> Grammar
process arg input
            | "-i" == arg = parseGrammar input
            | "-1" == arg = grammarAlgorithm1 $ parseGrammar input
            | "-2" == arg = grammarAlgorithm2 $ grammarAlgorithm1 $ parseGrammar input
            | otherwise = error "Error: Invalid parameter in process!"


parseGrammar :: [String] -> Grammar
parseGrammar (inNonteminals:inTerminals:inOrigin:inRules) = Grammar{
            grammarNonterminals = nonterminals,
            grammarTerminals = terminals,
            grammarOrigin = origin,
            grammarRules = rules
        }
        where
            nonterminals = makeSetFromList (getArrayNonterminals ( inNonteminals ))
            terminals = makeSetFromList (getArrayTerminals ( inTerminals ))
            origin = getOriginNonterminal ( inOrigin )
            rules =  makeSetFromList $ parseRules (inRules)

parseByLines :: String -> [String]
parseByLines input = lines (input)


grammarToStringByElems :: Set.Set Nonterminal -> Set.Set Terminal -> Nonterminal -> Set.Set Rule -> String
grammarToStringByElems inNonteminals inTerminals inOrigin inRules = nonterminals ++ terminals ++ origin ++ rules 
            where
                nonterminals = charArrToStringWithCommas (Set.toList $ inNonteminals)
                terminals = charArrToStringWithCommas (Set.toList $ inTerminals)
                origin = inOrigin:"\n"
                rules = listOfStringsToString $ rulesToArrayOfStrings inRules


grammarToString :: Grammar -> String
grammarToString grammar = grammarToStringByElems nonterminals terminals origin rules
        where
            nonterminals = grammarNonterminals grammar
            terminals = grammarTerminals grammar
            origin = grammarOrigin grammar
            rules =  grammarRules grammar


-- nt is Nt set from TIN arlgorithm 4.1
grammarAlgorithm1 :: Grammar -> Grammar
grammarAlgorithm1 grammar = Grammar{
            grammarNonterminals = nt,
            grammarTerminals = terminals,
            grammarOrigin = origin,
            grammarRules = filtredRules
        }
        where
            terminals = grammarTerminals grammar
            origin = grammarOrigin grammar
            rules =  grammarRules grammar
            nt = makeSetNt terminals rules Set.empty
            filtredRules = filterRulesBySymbols (Set.union nt terminals) rules


filterRulesBySymbols :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Rule
filterRulesBySymbols keys rules = makeSetFromList $ [ (left,right) | (left,right) <- Set.toList rules, elem left (Set.toList keys), checkElems right (Set.toList keys)]


listOfStringsToString :: [String] -> String
listOfStringsToString [] = ""
listOfStringsToString (x:xs) = x ++ listOfStringsToString xs

rulesToArrayOfStrings :: Set.Set Rule -> [String]
rulesToArrayOfStrings rules = [ [left] ++ "->" ++ right ++ "\n" | (left, right) <- Set.toList rules]

charArrToStringWithCommas :: [Char] -> String
charArrToStringWithCommas [] = []
charArrToStringWithCommas [x] = [x] ++ "\n"
charArrToStringWithCommas (x:xs) = [x] ++ "," ++  charArrToStringWithCommas xs


makeSetNt :: Set.Set Terminal->Set.Set Rule->Set.Set Nonterminal->Set.Set Nonterminal
makeSetNt terminals rules ntOld = do
                            let nt = [ nonterminal | (nonterminal, right) <- Set.toList rules, checkElems right (Set.toList (Set.union terminals ntOld))]
                            if ntOld == makeSetFromList nt
                                then makeSetFromList nt
                                else makeSetNt terminals rules (makeSetFromList nt)

-- Check if elems of set are in other set
checkElems :: [Char] -> [Char] -> Bool
checkElems [] x = True
checkElems (x:xs) yList = (elem x yList) && ( checkElems xs yList)

getArrayNonterminals :: String -> [Char]
getArrayNonterminals nonterminals = [ nonterminal | nonterminal <- nonterminals , elem nonterminal ['A'..'Z'] ]

getArrayTerminals :: String -> [Char]
getArrayTerminals terminals = [ terminal | terminal <- terminals , elem terminal ['a'..'z'] ]

getOriginNonterminal :: String -> Nonterminal
getOriginNonterminal "" = error "Invalid Origin" 
getOriginNonterminal origin =  head origin

parseRules :: [String] -> [Rule]
parseRules rules = [parseRule rule | rule <- rules]

parseRule :: String -> (Char,String)
parseRule rule = (parseLeft rule, parseRight rule)

parseLeft :: String -> Char
parseLeft rule = head rule

parseRight :: String -> String
parseRight rule = drop 3 rule


makeSetFromList :: (Ord a) => [a] -> Set.Set a
makeSetFromList [] = Set.empty
makeSetFromList (x:xs) = Set.insert x (makeSetFromList xs)



--vi is Vi set from TIN arlgorithm 4.2
grammarAlgorithm2 :: Grammar -> Grammar
grammarAlgorithm2 grammar = Grammar{
            grammarNonterminals = Set.intersection vi nonterminals,
            grammarTerminals = Set.intersection vi terminals,
            grammarOrigin = origin,
            grammarRules = filtredRules
        }
        where
            nonterminals = grammarNonterminals grammar
            terminals = grammarTerminals grammar
            origin = grammarOrigin grammar
            rules =  grammarRules grammar
            vi = makeSetVi (makeSetFromList [origin]) rules
            filtredRules = filterRulesBySymbols vi rules


makeSetVi :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Char
makeSetVi viOld rules = do
                        let vi = Set.union (iterateByViElemsToMakeVi (Set.toList viOld) (Set.toList rules)) viOld
                        if viOld == vi
                            then vi
                            else makeSetVi vi rules


iterateByViElemsToMakeVi :: [Char] -> [Rule]  -> Set.Set Char
iterateByViElemsToMakeVi [] rules = Set.empty
iterateByViElemsToMakeVi (viElem:rest) rules = Set.union (iterateByRulesToMakeVi viElem rules) (iterateByViElemsToMakeVi rest rules)


iterateByRulesToMakeVi :: Char -> [Rule]  -> Set.Set Char
iterateByRulesToMakeVi viElem [] = Set.empty
iterateByRulesToMakeVi viElem (rule:rest) = Set.union (addElemsToVi Set.empty viElem rule) (iterateByRulesToMakeVi viElem rest)


addElemsToVi :: Set.Set Char -> Char -> Rule -> Set.Set Char
addElemsToVi vi viElem (left, right) = do
                                if viElem == left
                                   then Set.union vi (insertStringToSetByChars right)
                                   else vi


insertStringToSetByChars :: String -> Set.Set Char
insertStringToSetByChars [] = Set.empty
insertStringToSetByChars (x:xs) = Set.insert x (insertStringToSetByChars xs)


