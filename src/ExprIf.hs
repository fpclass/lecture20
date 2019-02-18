--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 20: Sequential composition                                         --
--------------------------------------------------------------------------------

module ExprIf where

--------------------------------------------------------------------------------
-- Expression language with division and conditionals

data Expr = Val Int | Add Expr Expr | Div Expr Expr | If Expr Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  f = Nothing
bind (Just x) f = f x

-- wrong semantics: using applicatives, both branches of an if expression
-- get evaluated
eval :: Expr -> Maybe Int
eval (Val n)    = Just n
eval (Add l r)  = eval l `bind` \x ->
                  eval r `bind` \y ->
                  Just (x+y)
eval (Div l r)  = eval l `bind` \x ->
                  eval r `bind` \y ->
                  x `safediv` y
eval (If c t f) = ifA <$> eval c <*> eval t <*> eval f
    where ifA b x y = if b /= 0 then x else y

-- correct semantics: using bind, only one branch of an if expression
-- gets evaluated
eval' :: Expr -> Maybe Int
eval' (Val n)    = Just n
eval' (Add l r)  = eval' l `bind` \x ->
                   eval' r `bind` \y ->
                   Just (x+y)
eval' (Div l r)  = eval' l `bind` \x ->
                   eval' r `bind` \y ->
                   x `safediv` y
eval' (If c t f) = eval' c `bind` \b ->
    if b /= 0 then eval t else eval f

--------------------------------------------------------------------------------
