-- Název: simplify-bkg
-- Autor: Tomáš Blažek (xblaze31) 

import System.IO
import System.Environment
import System.Exit

import Data.Char
import Data.List.Split
import qualified Data.Set as Set


-- Main function
main :: IO()
main = do
    argv <- getArgs
    let file = getFileAndValidate argv
    input <- getInput file
    let output = process (head argv) (parseByLinesAndValidate input)
    putStr $ grammarToString output


-- Tests if program arguments are valid
isArgsValid :: [String] -> Bool 
isArgsValid ["-i"] = True 
isArgsValid ["-i", file] = True 
isArgsValid ["-1"] = True 
isArgsValid ["-1", file] = True 
isArgsValid ["-2"] = True 
isArgsValid ["-2", file] = True 
isArgsValid others = False 

-- Get name of file
getFile :: [String] -> String
getFile [] = ""
getFile (x:file)
        | x `notElem` ["-i", "-1", "-2"] = x
        | otherwise = getFile file

-- Get input from file or stdin
getInput :: String -> IO String
getInput "" = getContents
getInput file = readFile file

-- Get file name and validate program arguments
getFileAndValidate :: [String] -> String
getFileAndValidate args 
                    | isArgsValid args = getFile args
                    | otherwise = error "Error: Invalid program arguments!"

-- Types for grammar parsing 
type Nonterminal = Char
type Terminal = Char
type Rule = (Nonterminal,String)

-- Grammar data record
data Grammar = Grammar{
    grammarNonterminals::Set.Set Nonterminal,
    grammarTerminals::Set.Set Terminal,
    grammarOrigin::Nonterminal,
    grammarRules::Set.Set Rule
}

-- Specify Show instance of grammar to get greater debug output
instance Show Grammar where
    show (Grammar grammarNonterminals grammarTerminals grammarOrigin grammarRules) = 
        "Nonterminals:\n" ++ show grammarNonterminals ++ "\n" ++
        "Terminals:\n" ++ show grammarTerminals ++ "\n" ++
        "Origin:\n" ++ show grammarOrigin ++ "\n" ++
        "Rules:\n" ++ show grammarRules ++ "\n"



-- Process data by given arguments
process :: String -> [String] -> Grammar
process arg input
            | "-i" == arg = parseGrammar input
            | "-1" == arg = grammarAlgorithm1 $ parseGrammar input
            | "-2" == arg = grammarAlgorithm2 $ grammarAlgorithm1 $ parseGrammar input
            | otherwise = error "Error: Invalid parameter in process!"

-- Parse and load grammar to inside logic
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


-- Make from string list of lines (strings) and test if input is atleast 4 lines long
parseByLinesAndValidate :: String -> [String]
parseByLinesAndValidate input 
            | length inputLines >= 4 = inputLines
            | otherwise = error "Error: Invalid input format!"
            where
                inputLines = lines input


-- Get grammar string to print output format
grammarToStringByElems :: Set.Set Nonterminal -> Set.Set Terminal -> Nonterminal -> Set.Set Rule
                            -> String
grammarToStringByElems inNonteminals inTerminals inOrigin inRules = 
            nonterminals ++ terminals ++ origin ++ rules 
            where
                nonterminals = charArrToStringWithCommas (Set.toList inNonteminals)
                terminals = charArrToStringWithCommas (Set.toList inTerminals)
                origin = inOrigin:"\n"
                rules = listOfStringsToString $ rulesToArrayOfStrings inRules

-- Get grammar string to print output format
grammarToString :: Grammar -> String
grammarToString grammar = grammarToStringByElems nonterminals terminals origin rules
    where
        nonterminals = grammarNonterminals grammar
        terminals = grammarTerminals grammar
        origin = grammarOrigin grammar
        rules =  grammarRules grammar

-- Get set of nontelminals from string and validate
getArrayNonterminals :: String -> Set.Set Nonterminal
getArrayNonterminals input
    | validateNonterminals nonterminals
    && Set.size (Set.fromList nonterminalsString) == length nonterminalsString 
    = Set.fromList nonterminalsString
    | otherwise = error "Error: Invalid nonterminals!" 
    where
        nonterminals = splitOn "," input
        nonterminalsString = listOfStringsToString nonterminals

-- Return true if String is Char symbol
isChar :: String -> Bool
isChar [char] = True
isChar str = False 

-- Make Char from string or first Char is taken
toChar :: String -> Char
toChar [char] = char

-- Validate nonterminals to A-Z interval
validateNonterminals :: [String] -> Bool
validateNonterminals [] = True
validateNonterminals (nonterminal:rest) 
        | isChar nonterminal && elem (toChar nonterminal) ['A'..'Z'] = validateNonterminals rest
        | otherwise = False

-- Get set of terminals from string and validate
getArrayTerminals :: String -> Set.Set Terminal
getArrayTerminals input
    | (validateTerminals terminals || terminals == [""]) 
    && Set.size (Set.fromList terminalsString) == length terminalsString 
    = Set.fromList terminalsString
    | otherwise = error "Error: Invalid terminals!" 
    where
        terminals = splitOn "," input
        terminalsString = listOfStringsToString terminals

-- Validate nonterminals to a-z interval
validateTerminals :: [String] -> Bool
validateTerminals [] = True
validateTerminals (terminal:rest) 
                    | isChar terminal && elem (toChar terminal) ['a'..'z'] = validateTerminals rest
                    | otherwise = False

-- Get origin nonterminal from string and Validate
getOriginNonterminal :: String -> Set.Set Nonterminal-> Nonterminal
getOriginNonterminal "" _ = error "Error: Missing Origin" 
getOriginNonterminal [origin] nonterminals
                    | Set.member origin nonterminals = origin
                    | otherwise = error "Error: Given origin is not in defined nonterminal set!"
getOriginNonterminal other _ = error "Error: Invalid Origin" 


-- Get set of rules from list of strings and validate
parseRules :: [String] -> Set.Set Nonterminal -> Set.Set Terminal -> Set.Set Rule
parseRules rules nonterminals terminals 
    | Set.size ruleSet == length ruleList = ruleSet
    | otherwise = error "Error: Duplicity in rules!"
    where
        ruleList = [parseRule rule nonterminals terminals | rule <- rules]
        ruleSet = Set.fromList ruleList

-- Parse rule and create Rule type
parseRule :: String -> Set.Set Nonterminal -> Set.Set Terminal -> Rule
parseRule rule nonterminals terminals = 
    (parseRuleLeft rule nonterminals, parseRuleRight rule nonterminals terminals)

-- Parse left side of rule and validate
parseRuleLeft :: String -> Set.Set Nonterminal -> Char
parseRuleLeft rule nonterminals
    | isChar left && Set.member leftChar nonterminals = leftChar
    | otherwise = error ("Error: Invalid left side of rule " ++ show rule)
    where 
        left = head (splitOn "->" rule)
        leftChar = toChar left

-- Parse right side of rule and validate
parseRuleRight :: String -> Set.Set Nonterminal -> Set.Set Terminal -> String
parseRuleRight rule nonterminals terminals 
    | length ruleSplit == 2 
    && (checkElems right (Set.toList nonterminals ++ Set.toList terminals) || right == "#")
    = right
    | otherwise = error ("Error: Invalid right side of rule " ++ show rule)
    where 
        ruleSplit = splitOn "->" rule
        right = ruleSplit !! 1


-- Implements TIN arlgorithm 4.1. Result grammar grammar after aplication TIN arlgorithm 4.1
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
        -- nt is Nt set from TIN arlgorithm 4.1
        nt = makeSetNtAndValidateLanguageEmptiness origin nonterminals terminals rules
        filtredRules = filterRulesBySymbols (Set.union nt terminals) rules

-- Filter rules by nonterminals in key set
filterRulesBySymbols :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Rule
filterRulesBySymbols keys rules = Set.fromList [ (left,right) | (left,right) <- Set.toList rules,
                        Set.member left keys, checkElems right (Set.toList keys) || right == "#"]

-- Concatenate list of strings to one string
listOfStringsToString :: [String] -> String
listOfStringsToString [] = ""
listOfStringsToString (x:xs) = x ++ listOfStringsToString xs

-- Get list of string from set of Rules
rulesToArrayOfStrings :: Set.Set Rule -> [String]
rulesToArrayOfStrings rules = [[left] ++ "->" ++ right ++ "\n" | (left, right) <- Set.toList rules]

-- Get string from list of chars delimited with commas with newline
charArrToStringWithCommas :: String -> String
charArrToStringWithCommas [] = "\n"
charArrToStringWithCommas [x] = x:"\n"
charArrToStringWithCommas (x:xs) = [x] ++ "," ++  charArrToStringWithCommas xs

-- Make Nt set from TIN arlgorithm 4.1
makeSetNt :: Set.Set Terminal->Set.Set Rule->Set.Set Nonterminal->Set.Set Nonterminal
makeSetNt terminals rules ntOld 
    | ntOld == nt = nt
    | otherwise = makeSetNt terminals rules nt
    where
        nt = Set.fromList [left | (left, right) <- Set.toList rules,
         checkElems right (Set.toList (Set.union terminals ntOld)) || right == "#"]

-- Make Nt set and validate nonemptyness of generated language. If generated language is empty 
-- error is generated.
makeSetNtAndValidateLanguageEmptiness :: Nonterminal -> Set.Set Nonterminal -> Set.Set Terminal 
                                        -> Set.Set Rule -> Set.Set Nonterminal
makeSetNtAndValidateLanguageEmptiness origin nonterminals terminals rules
                    | isLanguageNotEmpty origin nt = nt
                    | otherwise = error "Grammar generates empty language!"
                    where 
                        nt = makeSetNt terminals rules Set.empty 

-- Test if generated language is empty
isLanguageNotEmpty :: Nonterminal -> Set.Set Nonterminal -> Bool
isLanguageNotEmpty origin nonterminals
                            | Set.member origin nonterminals = True
                            | otherwise = False 

-- Check if elems of list are in other list
checkElems :: String -> String -> Bool
checkElems [] x = True
checkElems (x:xs) yList = elem x yList && checkElems xs yList



-- Implements TIN arlgorithm 4.2. Result grammar grammar after aplication TIN arlgorithm 4.2
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
            vi = makeSetVi (Set.fromList [origin]) rules --vi is Vi set from TIN arlgorithm 4.2
            filtredRules = filterRulesBySymbols vi rules

-- Make Vi set from TIN arlgorithm 4.2
makeSetVi :: Set.Set Nonterminal -> Set.Set Rule -> Set.Set Char
makeSetVi viOld rules 
        | viOld == vi = vi 
        | otherwise = makeSetVi vi rules
        where 
            vi = Set.union (iterateByViElemsToMakeVi (Set.toList viOld) (Set.toList rules)) viOld

-- Iterate by Vi elements to add symbols to Vi set
iterateByViElemsToMakeVi :: String -> [Rule]  -> Set.Set Char
iterateByViElemsToMakeVi [] rules = Set.empty
iterateByViElemsToMakeVi (viElem:rest) rules =
    Set.union (iterateByRulesToMakeVi viElem rules) (iterateByViElemsToMakeVi rest rules)

-- Iterate by given rules with Vi Set
iterateByRulesToMakeVi :: Char -> [Rule]  -> Set.Set Char
iterateByRulesToMakeVi viElem [] = Set.empty
iterateByRulesToMakeVi viElem (rule:rest) = 
    Set.union (addElemsToVi Set.empty viElem rule) (iterateByRulesToMakeVi viElem rest)

-- Add symbols to Vi set
addElemsToVi :: Set.Set Char -> Char -> Rule -> Set.Set Char
addElemsToVi vi viElem (left, right)
                            | viElem == left = Set.union vi (insertStringToSetByChars right)
                            | otherwise = vi

-- Insert string splited by chars to Set of Chars
insertStringToSetByChars :: String -> Set.Set Char
insertStringToSetByChars [] = Set.empty
insertStringToSetByChars (x:xs) = Set.insert x (insertStringToSetByChars xs)


