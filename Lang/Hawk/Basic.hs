module Lang.Hawk.Basic where

data ArithOp = Set | Add | Sub | Mul | Div | Mod | Pow
               deriving (Eq, Show)

data CmpOp = CmpEQ | CmpNE | CmpGT | CmpGE | CmpLT | CmpLE
             deriving (Eq, Show)

data LogOp = AND | OR
             deriving (Eq, Show)

data BVar = ARGC | ARGV
          | FILENAME | FNR | FS
          | NF | NR
          | OFMT | OFS | ORS
          | RLENGTH | RS | RSTART
          | SUBSEP
            deriving (Eq, Ord, Show)

data BFunc = Atan2 | Cos | Exp | Int | Log | Sin | Sqrt
           | Srand | Rand 
           | Index | Length | Substr | FMatch | FSub | GSub | Split
           | Printf | SPrintf
             deriving (Eq, Show)

data Notation = Pre | Post
                deriving (Eq, Show)

data FileMod = ModAppend | ModRewrite
               deriving (Eq, Show)
