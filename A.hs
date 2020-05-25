module A where
import Data.Int
type I=Int32;type J=Int64;type O=Integer;type S=String;type C=Char;type Sy=S;type V=S

data E=A L|Ls[E]|Var V|Ap E[E]|Ass V E|Fun Fun|Seq[E]|Nil{-|Dic[(E,E)]-}deriving(Eq,Show)
{- ==> E α=A..|L[α]|D[(α,α)] -}

data L=N N|C C|Sy Sy deriving(Eq,Show);   data Fun=Op Op|Lam[V]E|Adv'd Adv E deriving(Eq,Show)
data N=J J           deriving(Eq,Show);   data Adv=Fold|Scan|Each|Mod        deriving(Eq,Enum,Show,Bounded)
 
data Op=(:~)|(:!)|(:@)|(:#)|(:$)|(:%)|(:^)|(:&)|(:*)|(:-)|(:=)|(:--)|(:+)|(:..)|(:.)|(:<)|(:>)|(:?)|(:/)|(:\)|(:|)
 deriving(Eq,Enum,Show,Bounded)
