--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 20: Sequential composition                                         --
--------------------------------------------------------------------------------

module ExprDiv where

--------------------------------------------------------------------------------
-- Expression language with division

data Expr = Val Int | Add Expr Expr | Div Expr Expr

-- unsafe: could crash at runtime
eval :: Expr -> Int
eval (Val n)   = n
eval (Add l r) = eval l + eval r
eval (Div l r) = eval l `div` eval r

safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  f = Nothing
bind (Just x) f = f x

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Add l r) = eval' l `bind` \x ->
                  eval' r `bind` \y ->
                  Just (x+y)
eval' (Div l r) = eval' l `bind` \x ->
                  eval' r `bind` \y ->
                  x `safediv` y

--------------------------------------------------------------------------------
