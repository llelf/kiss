module A where
import Data.Int
type I=Int32;type J=Int64;type O=Integer;type S=String;type C=Char;type Sy=S;type V=S;type B=Bool

data E=A L|Ls[E]|{-|Dic[E][E]-}Fun Fun|Var V|Ap E[E]|Ass V E|Cond[E]|Seq[E]|Nil deriving(Eq,Show)
{- ==> E α=A..|L[α]|D[α][α] -}

data L=N N|C C|Sy Sy deriving(Eq,Show);   data Fun=Op Op|Lam[V]E|Adv'd Adv E deriving(Eq,Show)
data N=J J|O O       deriving(Eq,Show);   data Adv=Fold|Scan|Each            deriving(Eq,Enum,Show,Bounded)
 
data Op=(:~)|(:!)|(:@)|(:#)|(:$)|(:%)|(:^)|(:&)|(:*)|(:-)|(:=)|(:--)|(:+)|(:..)|(:.)|(:<)|(:>)|(:?)|(:/)|(:\)|(:|)
 deriving(Eq,Enum,Read,Show,Bounded)
