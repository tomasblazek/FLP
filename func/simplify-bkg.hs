import System.IO
import System.Environment
import System.Exit

import Data.Char
import Data.List.Split
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
            nonterminals = getArrayNonterminals inNonteminals 
            terminals = getArrayTerminals inTerminals
            origin = getOriginNonterminal inOrigin nonterminals
            rules = parseRules inRules nonterminals terminals

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

getArrayNonterminals :: String -> Set.Set Nonterminal
getArrayNonterminals input
                | validateNonterminals nonterminals && (Set.size (Set.fromList nonterminalsString)) == length nonterminalsString = Set.fromList nonterminalsString
                | otherwise = error "Error: Invalid nonterminals!" 
                where
                    nonterminals = splitOn "," input
                    nonterminalsString = listOfStringsToString nonterminals


isChar :: String -> Bool
isChar (char:[]) = True
isChar str = False 

toChar :: String -> Char
toChar (char:[]) = char

validateNonterminals :: [String] -> Bool
validateNonterminals [] = True
validateNonterminals (nonterminal:rest) 
                    | isChar nonterminal && elem (toChar nonterminal) ['A'..'Z'] = validateNonterminals rest
                    | otherwise = False


getArrayTerminals :: String -> Set.Set Terminal
getArrayTerminals input
                | validateTerminals terminals && (Set.size (Set.fromList terminalsString)) == length terminalsString = Set.fromList terminalsString
                | otherwise = error "Error: Invalid terminals!" 
                where
                    terminals = splitOn "," input
                    terminalsString = listOfStringsToString terminals


validateTerminals :: [String] -> Bool
validateTerminals [] = True
validateTerminals (terminal:rest) 
                    | isChar terminal && elem (toChar terminal) ['a'..'z'] = validateTerminals rest
                    | otherwise = False


getOriginNonterminal :: String -> Set.Set Nonterminal-> Nonterminal
getOriginNonterminal "" _ = error "Error: Missing Origin" 
getOriginNonterminal (origin:[]) nonterminals
                            | Set.member origin nonterminals = origin
                            | otherwise = error "Error: Given origin is not in defined nonterminal set!" 
getOriginNonterminal other _ = error "Error: Invalid Origin" 


parseRules :: [String] -> Set.Set Nonterminal -> Set.Set Terminal -> Set.Set Rule
parseRules rules nonterminals terminals 
                | Set.size ruleSet == length ruleList = ruleSet
                | otherwise = error "Error: Duplicity in rules!"
                where
                    ruleList = [parseRule rule nonterminals terminals | rule <- rules]
                    ruleSet = Set.fromList ruleList


parseRule :: String -> Set.Set Nonterminal -> Set.Set Terminal -> Rule
parseRule rule nonterminals terminals = (parseRuleLeft rule nonterminals, parseRuleRight rule nonterminals terminals)

parseRuleLeft :: String -> Set.Set Nonterminal -> Char
parseRuleLeft rule nonterminals
            | isChar left && Set.member leftChar nonterminals = leftChar
            | otherwise = error ("Error: Invalid left side of rule " ++ show rule)
            where 
                left = head (splitOn "->" rule)
                leftChar = toChar left


parseRuleRight :: String -> Set.Set Nonterminal -> Set.Set Terminal -> String
parseRuleRight rule nonterminals terminals 
            | length ruleSplit == 2 && (checkElems right ((Set.toList nonterminals) ++ (Set.toList terminals)) || right == "#") = right
            | otherwise = error ("Error: Invalid right side of rule " ++ show rule)
            where 
                ruleSplit = splitOn "->" rule
                right = ruleSplit !! 1


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
            nonterminals = grammarNonterminals grammar
            origin = grammarOrigin grammar
            rules =  grammarRules grammar
            nt = makeSetNtAndValidateLanguageEmptiness origin nonterminals terminals rules
            filtredRules = filterRulesBySymbols (Set.union nt terminals) rules


filterRulesBySymbols :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Rule
filterRulesBySymbols keys rules = Set.fromList $ [ (left,right) | (left,right) <- Set.toList rules, Set.member left keys, checkElems right (Set.toList keys) || right == "#"]


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
makeSetNt terminals rules ntOld 
                    | ntOld == nt = nt
                    | otherwise = makeSetNt terminals rules nt
                    where
                        nt = Set.fromList [ left | (left, right) <- Set.toList rules, checkElems right (Set.toList (Set.union terminals ntOld)) || right == "#"]


makeSetNtAndValidateLanguageEmptiness :: Nonterminal -> Set.Set Nonterminal -> Set.Set Terminal -> Set.Set Rule -> Set.Set Nonterminal
makeSetNtAndValidateLanguageEmptiness origin nonterminals terminals rules
                    | isLanguageNotEmpty origin nt = nt
                    | otherwise = error "Grammar generates empty language!"
                    where 
                        nt = makeSetNt terminals rules Set.empty 


isLanguageNotEmpty :: Nonterminal -> Set.Set Nonterminal -> Bool
isLanguageNotEmpty origin nonterminals
                            | Set.member origin nonterminals = True
                            | otherwise = False 

-- Check if elems of set are in other set
checkElems :: [Char] -> [Char] -> Bool
checkElems [] x = True
checkElems (x:xs) yList = (elem x yList) && ( checkElems xs yList)



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
            vi = makeSetVi (Set.fromList [origin]) rules
            filtredRules = filterRulesBySymbols vi rules


makeSetVi :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Char
makeSetVi viOld rules 
                | viOld == vi = vi 
                | otherwise = makeSetVi vi rules
                where 
                    vi = Set.union (iterateByViElemsToMakeVi (Set.toList viOld) (Set.toList rules)) viOld

iterateByViElemsToMakeVi :: [Char] -> [Rule]  -> Set.Set Char
iterateByViElemsToMakeVi [] rules = Set.empty
iterateByViElemsToMakeVi (viElem:rest) rules = Set.union (iterateByRulesToMakeVi viElem rules) (iterateByViElemsToMakeVi rest rules)


iterateByRulesToMakeVi :: Char -> [Rule]  -> Set.Set Char
iterateByRulesToMakeVi viElem [] = Set.empty
iterateByRulesToMakeVi viElem (rule:rest) = Set.union (addElemsToVi Set.empty viElem rule) (iterateByRulesToMakeVi viElem rest)


addElemsToVi :: Set.Set Char -> Char -> Rule -> Set.Set Char
addElemsToVi vi viElem (left, right)
                            | viElem == left = Set.union vi (insertStringToSetByChars right)
                            | otherwise = vi


insertStringToSetByChars :: String -> Set.Set Char
insertStringToSetByChars [] = Set.empty
insertStringToSetByChars (x:xs) = Set.insert x (insertStringToSetByChars xs)


