{-
 - 
 - Lukasz Sliwa
 -
 - $ ghc --make runi
 - $ ./runi test1.i
 -
 -}

module Main where

import qualified Text.ParserCombinators.Parsec.Token as PTok
import qualified Text.ParserCombinators.Parsec.Language as PLan
import qualified Text.ParserCombinators.Parsec.Char as PChar
import qualified Text.ParserCombinators.Parsec.Combinator as PComb
import qualified Text.ParserCombinators.Parsec.Expr as PExpr
import qualified Text.ParserCombinators.Parsec.Prim as PPrim
import qualified Text.ParserCombinators.Parsec.Error as PErr
import List(deleteBy)
import Maybe(fromMaybe)
import System.Environment
import System.IO.Unsafe
import System.Cmd

type Identifier = String
type Contents = Integer
type Boolean = Bool
data AExpr = Const Contents | Ident Identifier |
             AExpr AExpr AOp AExpr deriving Show
data AOp = Oplus | Ominus | Otimes | Odiv | Omod deriving Show
data BExpr = BTrue | BFalse | BNot BExpr | BExpr BExpr BOp BExpr |
             RExpr AExpr ROp AExpr deriving Show
data BOp = BAnd | BOr deriving Show
data ROp = RLt | RLe | RGt | RGe | REq | RNe deriving Show
data Command = Skip | Abort | Identifier := AExpr | Command :. Command |
               Read Identifier | Write Identifier |
	       If BExpr Command | IfElse BExpr Command Command |
               While BExpr Command deriving Show
separator :: String
separator = ";"

parse :: String -> Either PErr.ParseError Command
parse = PPrim.parse (do PTok.whiteSpace lexer; c <- program; PComb.eof; return c) "" where
   langDef = PLan.LanguageDef {
      PLan.commentStart = "(*",
      PLan.commentEnd = "*)",
      PLan.commentLine = "//",
      PLan.nestedComments = True,
      PLan.identStart = PChar.letter,
      PLan.identLetter = PChar.alphaNum PPrim.<|> PChar.char '_',
      PLan.opStart = PLan.opLetter langDef,
      PLan.opLetter = PChar.oneOf (concat $ PLan.reservedOpNames langDef),
      PLan.reservedOpNames = ["+", "-", "*", "div", "mod", "=", "<>",
                              "<", "<=", ">", ">=", "and", "or", "not"],
      PLan.reservedNames = ["skip", "abort", "if", "then", "else", "fi",
                            "while", "do", "done", "read", "write"],
      PLan.caseSensitive = False }
   lexer = PTok.makeTokenParser langDef
   program = do
      commands <- PComb.many1 (do
      			   PTok.whiteSpace lexer
			   p <- command
			   PTok.whiteSpace lexer
			   PTok.symbol lexer separator
			   return p)
      return$ case commands of
         [c] -> c
         cs -> (foldr1 (:.) cs)
   command = PComb.choice [
      PTok.reserved lexer "skip" >> return Skip,
      PTok.reserved lexer "abort" >> return Abort,
      do
         id <- PTok.identifier lexer
         PTok.symbol lexer ":="
         ae <- aExpr
         return$ id := ae,
      do
         PTok.reserved lexer "if"
         be <- bExpr
         PTok.reserved lexer "then"
         th <- program
         PComb.choice [
            PTok.reserved lexer "fi" >> return (If be th),
            do
               PTok.reserved lexer "else"
               el <- program
               PTok.reserved lexer "fi"
               return$ IfElse be th el],
      do
         PTok.reserved lexer "while"
         be <- bExpr
         PTok.reserved lexer "do"
         bo <- program
         PTok.reserved lexer "done"
         return$ While be bo,
      do
	 PTok.reserved lexer "read"
         id <- PTok.identifier lexer
	 return$ Read id,
      do
	 PTok.reserved lexer "write"
	 id <- PTok.identifier lexer
	 return$ Write id]
   aExpr = PExpr.buildExpressionParser
      [[op "*" PExpr.AssocLeft, op "div" PExpr.AssocLeft,
      op "mod" PExpr.AssocLeft], [op "+" PExpr.AssocLeft,
      op "-" PExpr.AssocLeft ]] primAExpr where
      op name assoc = PExpr.Infix (do
         PTok.reservedOp lexer name
         return (\ x y -> AExpr x (case name of
            "*" -> Otimes
            "div" -> Odiv
            "mod" -> Omod
            "+" -> Oplus
            "-" -> Ominus) y)) assoc
      primAExpr = PComb.choice [
         PTok.integer lexer >>= return . Const,
         PTok.identifier lexer >>= return . Ident,
         PTok.parens lexer aExpr]
   bExpr = PExpr.buildExpressionParser
      [[prefix "not"], [op "and" PExpr.AssocRight],
         [op "or" PExpr.AssocRight]] primBExpr where
      op name assoc = PExpr.Infix (do
         PTok.reservedOp lexer name
         return (\ x y -> BExpr x (case name of
            "and" -> BAnd
            "or" -> BOr) y)) assoc
      prefix name = PExpr.Prefix $ do
         PTok.reservedOp lexer name
         return BNot
      primBExpr = PComb.choice [
         PTok.reserved lexer "true" >> return BTrue,
         PTok.reserved lexer "false" >> return BFalse,
         do
            a1 <- aExpr
            relop <- PComb.choice [
               PChar.string "=",
               PPrim.try (PChar.string "<>"),
               PPrim.try (PChar.string "<="),
               PChar.string "<",
               PPrim.try (PChar.string ">="),
               PChar.string ">"]
            let op = case relop of
                  "=" -> REq
                  "<>" -> RNe
                  "<=" -> RLe
                  "<" -> RLt
                  ">=" -> RGe
                  ">" -> RGt
            a2 <- aExpr
            return$ RExpr a1 op a2,
         PTok.parens lexer bExpr]

class Memory m where
   view :: m -> Identifier -> Contents
   update :: m -> (Identifier,Contents) -> m

aVal :: Memory m => AExpr -> m -> Contents
bVal :: Memory m => BExpr -> m -> Boolean
cVal :: Memory m => Command -> m -> IO m

aVal (Const n) _ = n
aVal (Ident x) m = m `view` x
aVal (AExpr e1 op e2) m = aVal e1 m `op'` aVal e2 m where
   op' = case op of
            Oplus -> (+)
            Ominus -> (-)
            Otimes -> (*)
            Odiv -> div
            Omod -> mod

bVal BTrue _ = True
bVal BFalse _ = False
bVal (BNot b) m = not $ bVal b m
bVal (BExpr b1 op b2) m = bVal b1 m `op'` bVal b2 m where
   op' = case op of
            BAnd -> (&&)
            BOr -> (||)
bVal (RExpr e1 op e2) m = aVal e1 m `op'` aVal e2 m where
   op' = case op of
            RLt -> (<)
            RLe -> (<=)
            RGt -> (>)
            RGe -> (>=)
            REq -> (==)
            RNe -> (/=)
 
cVal Skip m = return m
cVal (x := e) m =  return $ m `update` (x, aVal e m)
cVal (c1 :. c2) m = do
                      p <- cVal c1 m
		      q <- cVal c2 p
		      return q
cVal (If b c) m
   | bVal b m = cVal c m
   | otherwise = return m
cVal (IfElse b c1 c2) m
   | bVal b m = cVal c1 m
   | otherwise = cVal c2 m
cVal (While b c) m
   | bVal b m = do
                  p <- cVal c m
                  cVal (While b c) p
   | otherwise = return m
cVal (Read x) m = do 
                    value <- getLine
		    let v = read value::Integer
		    cVal (x := Const v) m
cVal (Write x) m = do 
                    putStrLn $ show (m `view` x) 
		    return m


newtype AssocListMem = ALM [(Identifier, Contents)] deriving Show

instance Memory AssocListMem where
   view (ALM xs) x = fromMaybe 0 (lookup x xs)
   update (ALM xs) p@(x,_) =
      ALM $ p : deleteBy (\ a b -> fst a == fst b) p xs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
	            then repl ++ (replace (drop (length find) s) find repl)
		            else [head s] ++ (replace (tail s) find repl)

specNames :: [String]
specNames = [ "done", "fi" ]

replaceSpecialNames :: [String] -> [Char] -> [Char]
replaceSpecialNames [] t = t
replaceSpecialNames (s:sp) t = replaceSpecialNames sp (replace t s (s ++ separator))

interprete :: Command -> IO AssocListMem
interprete cmd = flip cVal (ALM []) cmd

checkFileName :: String -> String
checkFileName name = 
	if length name > 2 
		then check (reverse name) 
		else name ++ ".i" where
			check (i:dot:_) = 
				if i == 'i' && dot == '.' 
					then name
					else name ++ ".i"
main :: IO ()
main = do
	(file:_) <- getArgs
	prog <- readFile (checkFileName file)
	case parse (replaceSpecialNames specNames prog) of
		Left err -> print err
                Right ast -> do interprete ast ; return ()
