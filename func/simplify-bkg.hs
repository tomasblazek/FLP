import System.IO
import System.Environment
import System.Exit
import Control.Monad
--import Data.List.Split

import Debug.Trace

main :: IO()
main = do
    argv <- getArgs
    when (not (isArgsValid argv)) $ error "Invalid Arguments!"
    let file = (getFile (argv))
    input <- getInput (file)
    mapM_ print (parseByLines $ grammarAlgorithm1 $ parseByLines input)


parseByLines :: String -> [String]
parseByLines input = lines (input)


isArgsValid :: [String] -> Bool
isArgsValid ["-i"] = True
isArgsValid ["-i", file] = True
isArgsValid ["-1"] = True
isArgsValid ["-1", file] = True
isArgsValid ["-2"] = True
isArgsValid ["-2", file] = True
isArgsValid others = False


getFile :: [String] -> String
getFile [ _ , file] = file
getFile others = ""


getInput :: String -> IO String
getInput "" = getContents
getInput file = readFile (file)



-- main :: IO()
-- main = do
--   --  args <- parseArgs (getArgs);
--     getArgs >>= argParse >>= putStr . tac

-- tac = unlines . reverse . lines

-- argParse :: [String] -> Either String Con
-- argParse [ ] = error "Invalid arguments!"
-- argParse [ x ]  = argError >> exitFail
-- argParse ["-h"] = usage >> exitSucc


-- usage = putStrLn "Usage: tac [-vh] [file ..]"
-- argError = putStrLn "Invalid arguments!"

-- exitSucc = exitWith ExitSuccess
-- exitFail = exitWith (ExitFailure 1)


--parseArgs :: [String] -> String
--parseArgs [] = error "Nothing to be done"


-- faktorial :: Integer -> Integer
-- faktorial 0 = 1
-- faktorial n = product[1..n]



-- main = do
--     putStrLn "Hello, what's your name?"  
--     x <- getLine  
--     putStrLn ("Hey " ++ x ++ ", you rock!")
--     args




gramar = ["A,B,S","a,b","S","A->AB","B->b","S->A","S->a"]

gramarNonTerms = ['A','B','S']
gramarTerms = ['a','b']
gramarRules = [('A',"AB"),('B',"b"),('S',"A"),('S',"a")]

gramarNonTerms2 = ['A','B','S', 'D']
gramarRules2 = [('A',"AB"),('B',"b"),('S',"A"),('S',"a"), ('A',"D"), ('D',"A")]


grammarAlgorithm1 :: [String] -> String
grammarAlgorithm1 grammar = do
                            let nonterminals = getArrayNonterminals ( head grammar )
                            let terminals = getArrayTerminals ( head $ drop 1 grammar )
                            let startNonterminal = getArrayNonterminals ( head $ drop 2 grammar )
                            let rules =  parseRules (drop 3 grammar)
                            let nt = makeSetNt nonterminals terminals rules [] 
                            charArrToStringWithCommas nt ++ charArrToStringWithCommas terminals ++ charArrToStringWithCommas startNonterminal ++ arrayOfStringsToString ( rulesToArrayOfStrings rules )

arrayOfStringsToString :: [String] -> String
arrayOfStringsToString [] = ""
arrayOfStringsToString (x:xs) = x ++ arrayOfStringsToString xs

rulesToArrayOfStrings :: [(Char, String)] -> [String]
rulesToArrayOfStrings rules = [ [left] ++ "->" ++ right ++ "\n" | (left, right) <- rules]

charArrToStringWithCommas :: [Char] -> String
charArrToStringWithCommas [] = []
charArrToStringWithCommas [x] = [x] ++ "\n"
charArrToStringWithCommas (x:xs) = [x] ++ "," ++  charArrToStringWithCommas xs


makeSetNt :: [Char]->[Char]->[(Char, String)]->[Char]->[Char]
makeSetNt nonterminals terminals rules nt = do
                            let ntNew = [ nonterminal | (nonterminal, right) <- rules, checkElems right (terminals ++ nt)]
                            if nt == ntNew
                                then ntNew
                                else makeSetNt nonterminals terminals rules ntNew

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