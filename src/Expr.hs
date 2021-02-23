--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Introduction to Sequential Composition                            --
--------------------------------------------------------------------------------

module Expr where

--------------------------------------------------------------------------------
-- Expression language

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add l r) = eval l + eval r

--------------------------------------------------------------------------------
