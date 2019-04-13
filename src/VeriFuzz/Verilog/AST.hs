{-|
Module      : VeriFuzz.Verilog.AST
Description : Definition of the Verilog AST types.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Poratbility : POSIX

Defines the types to build a Verilog AST.
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module VeriFuzz.Verilog.AST
    ( -- * Top level types
      Verilog(..)
    , getVerilog
    -- * Primitives
    -- ** Identifier
    , Identifier(..)
    , getIdentifier
    -- ** Control
    , Delay(..)
    , getDelay
    , Event(..)
    -- ** Operators
    , BinaryOperator(..)
    , UnaryOperator(..)
    -- ** Task
    , Task(..)
    , taskName
    , taskExpr
    -- ** Left hand side value
    , LVal(..)
    , regId
    , regExprId
    , regExpr
    , regSizeId
    , regSizeMSB
    , regSizeLSB
    , regConc
    -- ** Ports
    , PortDir(..)
    , PortType(..)
    , Port(..)
    , portType
    , portSigned
    , portSizeLower
    , portSize
    , portName
    -- * Expression
    , Expr(..)
    , exprSize
    , exprVal
    , exprId
    , exprConcat
    , exprUnOp
    , exprPrim
    , exprLhs
    , exprBinOp
    , exprRhs
    , exprCond
    , exprTrue
    , exprFalse
    , exprFunc
    , exprBody
    , exprStr
    , ConstExpr(..)
    , constSize
    , constNum
    , constParamId
    , constConcat
    , constUnOp
    , constPrim
    , constLhs
    , constBinOp
    , constRhs
    , constCond
    , constTrue
    , constFalse
    , constStr
    , Function(..)
    -- * Assignment
    , Assign(..)
    , assignReg
    , assignDelay
    , assignExpr
    , ContAssign(..)
    , contAssignNetLVal
    , contAssignExpr
    -- ** Parameters
    , Parameter(..)
    , paramIdent
    , paramValue
    , LocalParam(..)
    , localParamIdent
    , localParamValue
    -- * Statment
    , Statement(..)
    , statDelay
    , statDStat
    , statEvent
    , statEStat
    , statements
    , stmntBA
    , stmntNBA
    , stmntTask
    , stmntSysTask
    , stmntCondExpr
    , stmntCondTrue
    , stmntCondFalse
    , forAssign
    , forExpr
    , forIncr
    , forStmnt
    -- * Module
    , ModDecl(..)
    , modId
    , modOutPorts
    , modInPorts
    , modItems
    , modParams
    , ModItem(..)
    , modContAssign
    , modInstId
    , modInstName
    , modInstConns
    , paramDecl
    , localParamDecl
    , traverseModItem
    , declDir
    , declPort
    , declVal
    , ModConn(..)
    , modConn
    , modConnName
    , modExpr
    -- * Useful Lenses and Traversals
    , getModule
    , getSourceId
    )
where

import           Control.Lens
import           Data.Data
import           Data.Data.Lens
import           Data.List.NonEmpty (NonEmpty)
import           Data.String        (IsString, fromString)
import           Data.Text          (Text)
import           Data.Traversable   (sequenceA)

-- | Identifier in Verilog. This is just a string of characters that can either
-- be lowercase and uppercase for now. This might change in the future though,
-- as Verilog supports many more characters in Identifiers.
newtype Identifier = Identifier { _getIdentifier :: Text }
                   deriving (Eq, Show, Ord, Data, IsString, Semigroup, Monoid)

-- | Verilog syntax for adding a delay, which is represented as @#num@.
newtype Delay = Delay { _getDelay :: Int }
                deriving (Eq, Show, Ord, Data, Num)

-- | Verilog syntax for an event, such as @\@x@, which is used for always blocks
data Event = EId {-# UNPACK #-} !Identifier
           | EExpr !Expr
           | EAll
           | EPosEdge {-# UNPACK #-} !Identifier
           | ENegEdge {-# UNPACK #-} !Identifier
           deriving (Eq, Show, Ord, Data)

-- | Binary operators that are currently supported in the verilog generation.
data BinaryOperator = BinPlus    -- ^ @+@
                    | BinMinus   -- ^ @-@
                    | BinTimes   -- ^ @*@
                    | BinDiv     -- ^ @/@
                    | BinMod     -- ^ @%@
                    | BinEq      -- ^ @==@
                    | BinNEq     -- ^ @!=@
                    | BinCEq     -- ^ @===@
                    | BinCNEq    -- ^ @!==@
                    | BinLAnd    -- ^ @&&@
                    | BinLOr     -- ^ @||@
                    | BinLT      -- ^ @<@
                    | BinLEq     -- ^ @<=@
                    | BinGT      -- ^ @>@
                    | BinGEq     -- ^ @>=@
                    | BinAnd     -- ^ @&@
                    | BinOr      -- ^ @|@
                    | BinXor     -- ^ @^@
                    | BinXNor    -- ^ @^~@
                    | BinXNorInv -- ^ @~^@
                    | BinPower   -- ^ @**@
                    | BinLSL     -- ^ @<<@
                    | BinLSR     -- ^ @>>@
                    | BinASL     -- ^ @<<<@
                    | BinASR     -- ^ @>>>@
                    deriving (Eq, Show, Ord, Data)

-- | Unary operators that are currently supported by the generator.
data UnaryOperator = UnPlus    -- ^ @+@
                   | UnMinus   -- ^ @-@
                   | UnLNot    -- ^ @!@
                   | UnNot     -- ^ @~@
                   | UnAnd     -- ^ @&@
                   | UnNand    -- ^ @~&@
                   | UnOr      -- ^ @|@
                   | UnNor     -- ^ @~|@
                   | UnXor     -- ^ @^@
                   | UnNxor    -- ^ @~^@
                   | UnNxorInv -- ^ @^~@
                   deriving (Eq, Show, Ord, Data)

data Function = SignedFunc
              | UnsignedFunc
              deriving (Eq, Show, Ord, Data)

-- | Verilog expression, which can either be a primary expression, unary
-- expression, binary operator expression or a conditional expression.
data Expr = Number { _exprSize :: {-# UNPACK #-} !Int
                   , _exprVal  :: Integer
                   }
          | Id { _exprId :: {-# UNPACK #-} !Identifier }
          | Concat { _exprConcat :: [Expr] }
          | UnOp { _exprUnOp :: !UnaryOperator
                 , _exprPrim :: Expr
                 }
          | BinOp { _exprLhs   :: Expr
                  , _exprBinOp :: !BinaryOperator
                  , _exprRhs   :: Expr
                  }
          | Cond { _exprCond  :: Expr
                 , _exprTrue  :: Expr
                 , _exprFalse :: Expr
                 }
          | Func { _exprFunc :: !Function
                 , _exprBody :: Expr
                 }
          | Str { _exprStr :: {-# UNPACK #-} !Text }
          deriving (Eq, Show, Ord, Data)

instance Num Expr where
  a + b = BinOp a BinPlus b
  a - b = BinOp a BinMinus b
  a * b = BinOp a BinTimes b
  negate = UnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = Number 32 . fromInteger

instance Semigroup Expr where
  (Concat a) <> (Concat b) = Concat $ a <> b
  (Concat a) <> b = Concat $ a <> [b]
  a <> (Concat b) = Concat $ a : b
  a <> b = Concat [a, b]

instance Monoid Expr where
  mempty = Concat []

instance IsString Expr where
  fromString = Str . fromString

instance Plated Expr where
  plate = uniplate

-- | Constant expression, which are known before simulation at compile time.
data ConstExpr = ConstNum { _constSize :: Int
                          , _constNum  :: Integer
                          }
               | ParamId { _constParamId :: {-# UNPACK #-} !Identifier }
               | ConstConcat { _constConcat :: [ConstExpr] }
               | ConstUnOp { _constUnOp :: !UnaryOperator
                           , _constPrim :: ConstExpr
                           }
               | ConstBinOp { _constLhs   :: ConstExpr
                            , _constBinOp :: !BinaryOperator
                            , _constRhs   :: ConstExpr
                            }
               | ConstCond { _constCond  :: ConstExpr
                           , _constTrue  :: ConstExpr
                           , _constFalse :: ConstExpr
                           }
               | ConstStr { _constStr :: {-# UNPACK #-} !Text }
               deriving (Eq, Show, Ord, Data)

instance Num ConstExpr where
  a + b = ConstBinOp a BinPlus b
  a - b = ConstBinOp a BinMinus b
  a * b = ConstBinOp a BinTimes b
  negate = ConstUnOp UnMinus
  abs = undefined
  signum = undefined
  fromInteger = ConstNum 32 . fromInteger

instance Semigroup ConstExpr where
  (ConstConcat a) <> (ConstConcat b) = ConstConcat $ a <> b
  (ConstConcat a) <> b = ConstConcat $ a <> [b]
  a <> (ConstConcat b) = ConstConcat $ a : b
  a <> b = ConstConcat [a, b]

instance Monoid ConstExpr where
  mempty = ConstConcat []

instance IsString ConstExpr where
  fromString = ConstStr . fromString

instance Plated ConstExpr where
  plate = uniplate

data Task = Task { _taskName :: {-# UNPACK #-} !Identifier
                 , _taskExpr :: [Expr]
                 } deriving (Eq, Show, Ord, Data)

-- | Type that represents the left hand side of an assignment, which can be a
-- concatenation such as in:
--
-- @
-- {a, b, c} = 32'h94238;
-- @
data LVal = RegId { _regId :: {-# UNPACK #-} !Identifier }
          | RegExpr { _regExprId :: {-# UNPACK #-} !Identifier
                    , _regExpr   :: !Expr
                    }
          | RegSize { _regSizeId  :: {-# UNPACK #-} !Identifier
                    , _regSizeMSB :: !ConstExpr
                    , _regSizeLSB :: !ConstExpr
                    }
          | RegConcat { _regConc :: [Expr] }
          deriving (Eq, Show, Ord, Data)

instance IsString LVal where
  fromString = RegId . fromString

-- | Different port direction that are supported in Verilog.
data PortDir = PortIn    -- ^ Input direction for port (@input@).
             | PortOut   -- ^ Output direction for port (@output@).
             | PortInOut -- ^ Inout direction for port (@inout@).
             deriving (Eq, Show, Ord, Data)

-- | Currently, only @wire@ and @reg@ are supported, as the other net types are
-- not that common and not a priority.
data PortType = Wire
              | Reg
              deriving (Eq, Show, Ord, Data)

-- | Port declaration. It contains information about the type of the port, the
-- size, and the port name. It used to also contain information about if it was
-- an input or output port. However, this is not always necessary and was more
-- cumbersome than useful, as a lot of ports can be declared without input and
-- output port.
--
-- This is now implemented inside 'ModDecl' itself, which uses a list of output
-- and input ports.
data Port = Port { _portType      :: !PortType
                 , _portSigned    :: !Bool
                 , _portSizeLower :: {-# UNPACK #-} !Int
                 , _portSize      :: {-# UNPACK #-} !Int
                 , _portName      :: {-# UNPACK #-} !Identifier
                 } deriving (Eq, Show, Ord, Data)

-- | This is currently a type because direct module declaration should also be
-- added:
--
-- @
-- mod a(.y(y1), .x1(x11), .x2(x22));
-- @
data ModConn = ModConn { _modConn :: !Expr }
             | ModConnNamed { _modConnName :: {-# UNPACK #-} !Identifier
                            , _modExpr     :: !Expr
                            }
             deriving (Eq, Show, Ord, Data)

data Assign = Assign { _assignReg   :: !LVal
                     , _assignDelay :: !(Maybe Delay)
                     , _assignExpr  :: !Expr
                     } deriving (Eq, Show, Ord, Data)

data ContAssign = ContAssign { _contAssignNetLVal :: {-# UNPACK #-} !Identifier
                             , _contAssignExpr    :: !Expr
                             } deriving (Eq, Show, Ord, Data)

-- | Statements in Verilog.
data Statement = TimeCtrl { _statDelay :: {-# UNPACK #-} !Delay
                          , _statDStat :: Maybe Statement
                          }                                -- ^ Time control (@#NUM@)
           | EventCtrl { _statEvent :: !Event
                       , _statEStat :: Maybe Statement
                       }
           | SeqBlock { _statements   :: [Statement] }     -- ^ Sequential block (@begin ... end@)
           | BlockAssign { _stmntBA      :: !Assign }     -- ^ blocking assignment (@=@)
           | NonBlockAssign { _stmntNBA     :: !Assign }     -- ^ Non blocking assignment (@<=@)
           | TaskEnable { _stmntTask    :: !Task }
           | SysTaskEnable { _stmntSysTask :: !Task }
           | CondStmnt { _stmntCondExpr  :: Expr
                       , _stmntCondTrue  :: Maybe Statement
                       , _stmntCondFalse :: Maybe Statement
                       }
           | ForLoop { _forAssign :: !Assign
                     , _forExpr   :: Expr
                     , _forIncr   :: !Assign
                     , _forStmnt  :: Statement
                     } -- ^ Loop bounds shall be statically computable for a for loop.
           deriving (Eq, Show, Ord, Data)

instance Semigroup Statement where
  (SeqBlock a) <> (SeqBlock b) = SeqBlock $ a <> b
  (SeqBlock a) <> b = SeqBlock $ a <> [b]
  a <> (SeqBlock b) = SeqBlock $ a : b
  a <> b = SeqBlock [a, b]

instance Monoid Statement where
  mempty = SeqBlock []

-- | Parameter that can be assigned in blocks or modules using @parameter@.
data Parameter = Parameter { _paramIdent :: {-# UNPACK #-} !Identifier
                           , _paramValue :: ConstExpr
                           }
               deriving (Eq, Show, Ord, Data)

-- | Local parameter that can be assigned anywhere using @localparam@. It cannot
-- be changed by initialising the module.
data LocalParam = LocalParam { _localParamIdent :: {-# UNPACK #-} !Identifier
                             , _localParamValue :: ConstExpr
                             }
                deriving (Eq, Show, Ord, Data)

-- | Module item which is the body of the module expression.
data ModItem = ModCA { _modContAssign :: !ContAssign }
             | ModInst { _modInstId    :: {-# UNPACK #-} !Identifier
                       , _modInstName  :: {-# UNPACK #-} !Identifier
                       , _modInstConns :: [ModConn]
                       }
             | Initial !Statement
             | Always !Statement
             | Decl { _declDir  :: !(Maybe PortDir)
                    , _declPort :: !Port
                    , _declVal  :: Maybe ConstExpr
                    }
             | ParamDecl { _paramDecl :: NonEmpty Parameter }
             | LocalParamDecl { _localParamDecl :: NonEmpty LocalParam }
             deriving (Eq, Show, Ord, Data)

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModDecl = ModDecl { _modId       :: {-# UNPACK #-} !Identifier
                       , _modOutPorts :: [Port]
                       , _modInPorts  :: [Port]
                       , _modItems    :: [ModItem]
                       , _modParams   :: [Parameter]
                       }
             deriving (Eq, Show, Ord, Data)

traverseModConn :: (Applicative f) => (Expr -> f Expr) -> ModConn -> f ModConn
traverseModConn f (ModConn e       ) = ModConn <$> f e
traverseModConn f (ModConnNamed a e) = ModConnNamed a <$> f e

traverseModItem :: (Applicative f) => (Expr -> f Expr) -> ModItem -> f ModItem
traverseModItem f (ModCA (ContAssign a e)) = ModCA . ContAssign a <$> f e
traverseModItem f (ModInst a b e) =
    ModInst a b <$> sequenceA (traverseModConn f <$> e)
traverseModItem _ e = pure e

-- | The complete sourcetext for the Verilog module.
newtype Verilog = Verilog { _getVerilog :: [ModDecl] }
                   deriving (Eq, Show, Ord, Data, Semigroup, Monoid)

makeLenses ''Identifier
makeLenses ''Delay
makeLenses ''Expr
makeLenses ''ConstExpr
makeLenses ''Task
makeLenses ''LVal
makeLenses ''PortType
makeLenses ''Port
makeLenses ''ModConn
makeLenses ''Assign
makeLenses ''ContAssign
makeLenses ''Statement
makeLenses ''ModItem
makeLenses ''Parameter
makeLenses ''LocalParam
makeLenses ''ModDecl
makeLenses ''Verilog

getModule :: Traversal' Verilog ModDecl
getModule = getVerilog . traverse
{-# INLINE getModule #-}

getSourceId :: Traversal' Verilog Text
getSourceId = getModule . modId . getIdentifier
{-# INLINE getSourceId #-}