-------------------------------------------------------------------------------
-- Pretty printer for the Imp language
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module Lang.Imp.Pretty where
import Lang.Imp.Ast
import Data.List (intercalate)

todo :: a
todo = error "TODO"

-- | Pretty print an expression
ppExpr :: Expr a -> String
ppExpr e = case e of
    AVar name -> name
    AConst n -> show n
    APlus a b -> expr "+" a b
    AMinus a b -> expr "-" a b
    AMul a b -> expr "*" a b
    ADiv a b -> expr "/" a b
    AMod a b -> expr "%" a b
    BNot b -> "not" ++ ppExpr b
    BAnd a b -> expr "&&" a b
    BOr a b -> expr "||" a b
    REq a b -> expr "=" a b
    RLt a b -> expr "<" a b
    where
        expr op a b = "(" ++ ppExpr a ++ " " ++ op ++ " " ++ ppExpr b ++ ")" 

-- | Pretty print a command
ppCmd :: Cmd -> String
ppCmd = ppCmdAt 0

ppCmdAt :: Int -> Cmd -> String
ppCmdAt n cmd =
    let indent = replicate (2 * n) ' '
        nestedCmd = ppCmdAt (n + 1)
    in case cmd of 
        CSkip               -> indent ++ "skip"
        CAssign name expr   -> indent ++ name ++ " := " ++ ppExpr expr
        CSeq cmds           -> intercalate ";\n" (map (ppCmdAt n) cmds)
        CIfThEl cond th el  -> indent ++ "if " ++ ppExpr cond ++ " then\n"
                                ++ nestedCmd th ++ "\n"
                                ++ indent ++ "else\n"
                                ++ nestedCmd el ++ "\n"
                                ++ indent ++ "endif"
        CWhile cond body    -> indent ++ "while " ++ ppExpr cond ++ " do\n"
                                ++ nestedCmd body ++ "\n"
                                ++ indent ++ "endwhile"