{-|
Module      : VeriFuzz.Verilog.Parser
Description : Minimal Verilog parser to reconstruct the AST.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Minimal Verilog parser to reconstruct the AST. This parser does not support the
whole Verilog syntax, as the AST does not support it either.
-}

module VeriFuzz.Verilog.Parser
    ( -- * Parser
      parseVerilog
    , parseModDecl
    )
where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Bifunctor              (bimap)
import           Data.Bits
import           Data.Functor                (($>))
import           Data.Functor.Identity       (Identity)
import           Data.List                   (isInfixOf, isPrefixOf)
import qualified Data.Text                   as T
import           Text.Parsec                 hiding (satisfy)
import           Text.Parsec.Expr
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.Internal
import           VeriFuzz.Verilog.Lex
import           VeriFuzz.Verilog.Preprocess
import           VeriFuzz.Verilog.Token


type Parser = Parsec [Token] ()

type ParseOperator = Operator [Token] () Identity

data Decimal = Decimal Int Integer

instance Num Decimal where
    (Decimal sa na) + (Decimal sb nb) = Decimal (max sa sb) (na + nb)
    (Decimal sa na) - (Decimal sb nb) = Decimal (max sa sb) (na - nb)
    (Decimal sa na) * (Decimal sb nb) = Decimal (max sa sb) (na * nb)
    negate (Decimal s n) = Decimal s $ negate n
    abs (Decimal s n) = Decimal s $ abs n
    signum (Decimal s n) = Decimal s $ signum n
    fromInteger = Decimal 32 . fromInteger

-- | This parser succeeds whenever the given predicate returns true when called
-- with parsed `Token`. Same as 'Text.Parsec.Char.satisfy'.
satisfy :: (Token -> Bool) -> Parser TokenName
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Token -> Maybe TokenName
    tokeq t@(Token t' _ _) = if f t then Just t' else Nothing

satisfy' :: (Token -> Maybe a) -> Parser a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
nextPos pos _ (Token _ _ (Position _ l c) : _) =
    setSourceColumn (setSourceLine pos l) c
nextPos pos _ [] = pos

-- | Parses given `TokenName`.
tok :: TokenName -> Parser TokenName
tok t = satisfy (\(Token t' _ _) -> t' == t) <?> show t

-- | Parse without returning the `TokenName`.
tok' :: TokenName -> Parser ()
tok' p = void $ tok p

parens :: Parser a -> Parser a
parens = between (tok SymParenL) (tok SymParenR)

braces :: Parser a -> Parser a
braces = between (tok SymBraceL) (tok SymBraceR)

sBinOp :: BinaryOperator -> Expr -> Expr -> Expr
sBinOp = sOp BinOp where sOp f b a = f a b

parseExpr' :: Parser Expr
parseExpr' = buildExpressionParser parseTable parseTerm <?> "expr"

decToExpr :: Decimal -> Expr
decToExpr (Decimal s n) = Number s n

-- | Parse a Number depending on if it is in a hex or decimal form. Octal and
-- binary are not supported yet.
parseNum :: Parser Expr
parseNum = decToExpr <$> number

parseVar :: Parser Expr
parseVar = Id <$> identifier

systemFunc :: String -> Parser String
systemFunc s = satisfy' matchId
  where
    matchId (Token IdSystem s' _) = if s == s' then Just s else Nothing
    matchId _                     = Nothing

parseFunction :: Parser Function
parseFunction =
    systemFunc "$unsigned"
        $>  UnsignedFunc
        <|> systemFunc "$signed"
        $>  SignedFunc

parseFun :: Parser Expr
parseFun = do
    f    <- parseFunction
    expr <- parens parseExpr
    return $ Func f expr

parseTerm :: Parser Expr
parseTerm =
    parens parseExpr
        <|> (Concat <$> braces (commaSep parseExpr))
        <|> parseFun
        <|> parseNum
        <|> parseVar
        <?> "simple expr"

-- | Parses the ternary conditional operator. It will behave in a right
-- associative way.
parseCond :: Expr -> Parser Expr
parseCond e = do
    tok' SymQuestion
    expr <- parseExpr
    tok' SymColon
    Cond e expr <$> parseExpr

parseExpr :: Parser Expr
parseExpr = do
    e <- parseExpr'
    option e . try $ parseCond e

-- | Table of binary and unary operators that encode the right precedence for
-- each.
parseTable :: [[ParseOperator Expr]]
parseTable =
    [ [prefix SymBang (UnOp UnLNot), prefix SymTildy (UnOp UnNot)]
    , [ prefix SymAmp      (UnOp UnAnd)
      , prefix SymBar      (UnOp UnOr)
      , prefix SymTildyAmp (UnOp UnNand)
      , prefix SymTildyBar (UnOp UnNor)
      , prefix SymHat      (UnOp UnXor)
      , prefix SymTildyHat (UnOp UnNxor)
      , prefix SymHatTildy (UnOp UnNxorInv)
      ]
    , [prefix SymPlus (UnOp UnPlus), prefix SymDash (UnOp UnMinus)]
    , [binary SymAsterAster (sBinOp BinPower) AssocRight]
    , [ binary SymAster   (sBinOp BinTimes) AssocLeft
      , binary SymSlash   (sBinOp BinDiv)   AssocLeft
      , binary SymPercent (sBinOp BinMod)   AssocLeft
      ]
    , [ binary SymPlus (sBinOp BinPlus) AssocLeft
      , binary SymDash (sBinOp BinPlus) AssocLeft
      ]
    , [ binary SymLtLt (sBinOp BinLSL) AssocLeft
      , binary SymGtGt (sBinOp BinLSR) AssocLeft
      ]
    , [ binary SymLtLtLt (sBinOp BinASL) AssocLeft
      , binary SymGtGtGt (sBinOp BinASR) AssocLeft
      ]
    , [ binary SymLt   (sBinOp BinLT)  AssocNone
      , binary SymGt   (sBinOp BinGT)  AssocNone
      , binary SymLtEq (sBinOp BinLEq) AssocNone
      , binary SymGtEq (sBinOp BinLEq) AssocNone
      ]
    , [ binary SymEqEq   (sBinOp BinEq)  AssocNone
      , binary SymBangEq (sBinOp BinNEq) AssocNone
      ]
    , [ binary SymEqEqEq   (sBinOp BinEq)  AssocNone
      , binary SymBangEqEq (sBinOp BinNEq) AssocNone
      ]
    , [binary SymAmp (sBinOp BinAnd) AssocLeft]
    , [ binary SymHat      (sBinOp BinXor)     AssocLeft
      , binary SymHatTildy (sBinOp BinXNor)    AssocLeft
      , binary SymTildyHat (sBinOp BinXNorInv) AssocLeft
      ]
    , [binary SymBar (sBinOp BinOr) AssocLeft]
    , [binary SymAmpAmp (sBinOp BinLAnd) AssocLeft]
    , [binary SymBarBar (sBinOp BinLOr) AssocLeft]
    ]

binary :: TokenName -> (a -> a -> a) -> Assoc -> ParseOperator a
binary name fun = Infix ((tok name <?> "binary") >> return fun)

prefix :: TokenName -> (a -> a) -> ParseOperator a
prefix name fun = Prefix ((tok name <?> "prefix") >> return fun)

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy $ tok SymComma

parseContAssign :: Parser ContAssign
parseContAssign = do
    var  <- tok KWAssign *> identifier
    expr <- tok SymEq *> parseExpr
    tok' SymSemi
    return $ ContAssign var expr

numLit :: Parser String
numLit = satisfy' matchId
  where
    matchId (Token LitNumber s _) = Just s
    matchId _                     = Nothing

number :: Parser Decimal
number = number' <$> numLit
  where
    number' :: String -> Decimal
    number' a | all (`elem` ['0' .. '9']) a = fromInteger $ read a
              | head a == '\''              = fromInteger $ f a
              | "'" `isInfixOf` a           = Decimal (read w) (f b)
              | otherwise = error $ "Invalid number format: " ++ a
      where
        w = takeWhile (/= '\'') a
        b = dropWhile (/= '\'') a
        f a'
            | "'d" `isPrefixOf` a' = read $ drop 2 a'
            | "'h" `isPrefixOf` a' = read $ "0x" ++ drop 2 a'
            | "'b" `isPrefixOf` a' = foldl
                (\n b' -> shiftL n 1 .|. (if b' == '1' then 1 else 0))
                0
                (drop 2 a')
            | otherwise = error $ "Invalid number format: " ++ a'

toInteger' :: Decimal -> Integer
toInteger' (Decimal _ n) = n

-- | Parse a range and return the total size. As it is inclusive, 1 has to be
-- added to the difference.
parseRange :: Parser Int
parseRange = do
    rangeH <- tok SymBrackL *> number
    rangeL <- tok SymColon *> number
    tok' SymBrackR
    return . fromInteger . toInteger' $ rangeH - rangeL + 1

strId :: Parser String
strId = satisfy' matchId
  where
    matchId (Token IdSimple  s _) = Just s
    matchId (Token IdEscaped s _) = Just s
    matchId _                     = Nothing

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> strId

parseNetDecl :: Maybe PortDir -> Parser ModItem
parseNetDecl pd = do
    t     <- option Wire type_
    sign  <- option False (tok KWSigned $> True)
    range <- option 1 parseRange
    name  <- identifier
    tok' SymSemi
    return $ Decl pd (Port t sign 0 range name) Nothing
    where type_ = tok KWWire $> Wire <|> tok KWReg $> Reg

parsePortDir :: Parser PortDir
parsePortDir =
    tok KWOutput
        $>  PortOut
        <|> tok KWInput
        $>  PortIn
        <|> tok KWInout
        $>  PortInOut

parseDecl :: Parser ModItem
parseDecl = (Just <$> parsePortDir >>= parseNetDecl) <|> parseNetDecl Nothing

parseModItem :: Parser ModItem
parseModItem = (ModCA <$> parseContAssign) <|> parseDecl

parseModList :: Parser [Identifier]
parseModList = list <|> return [] where list = parens $ commaSep identifier

filterDecl :: PortDir -> ModItem -> Bool
filterDecl p (Decl (Just p') _ _) = p == p'
filterDecl _ _                    = False

modPorts :: PortDir -> [ModItem] -> [Port]
modPorts p mis = filter (filterDecl p) mis ^.. traverse . declPort

parseModDecl :: Parser ModDecl
parseModDecl = do
    name <- tok KWModule *> identifier
    _    <- fmap defaultPort <$> parseModList
    tok' SymSemi
    modItem <- option [] . try $ many1 parseModItem
    tok' KWEndmodule
    return $ ModDecl name
                     (modPorts PortOut modItem)
                     (modPorts PortIn modItem)
                     modItem
                     []

-- | Parses a 'String' into 'Verilog' by skipping any beginning whitespace
-- and then parsing multiple Verilog source.
parseVerilogSrc :: Parser Verilog
parseVerilogSrc = Verilog <$> many parseModDecl

-- | Parse a 'String' containing verilog code. The parser currently only supports
-- the subset of Verilog that is being generated randomly.
parseVerilog
    :: String -- ^ Name of parsed object.
    -> String -- ^ Content to be parsed.
    -> Either String Verilog -- ^ Returns 'String' with error
                                         -- message if parse fails.
parseVerilog s =
    bimap show id . parse parseVerilogSrc s . alexScanTokens . preprocess [] s