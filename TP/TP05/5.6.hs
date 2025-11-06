type Name = Char -- ’x’, ’y’, ’z’, etc.
type Env = [(Name, Bool)]
-- associations of names to values

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop    -- New Addition
  deriving (Eq, Show)

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
    = case lookup x env of
        Just b  -> b
        Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q)
    = eval env p && eval env q
eval env (Imply p q)
    = not (eval env p) || eval env q
eval env (Or p q)                   -- New Addition
    = eval env p || eval env q
