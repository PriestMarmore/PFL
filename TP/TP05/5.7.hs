type Name = Char
type Env = [(Name, Bool)]

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop

vars :: Prop -> [Name]
vars (Const b)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q
vars (Or p q)     = vars p ++ vars q