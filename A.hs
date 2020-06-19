{-#language DeriveGeneric#-}
module A where
import Data.Int;import GHC.Generics(Generic)
type I=Int32;type J=Int64;type O=Integer;type F=Double;type S=String;type C=Char;type Sy=S;type V=S;type B=Bool

data E=A L|Ls[E]|{-|Dic[E][E]-}Fun Fun|Var V|Ap E[E]|Ass E E|Cond[E]|Seq[E]|Nil|Com[E] deriving(Eq,Ord,Show,Generic)
{- ==> E α=A..|L[α]|D[α][α] -}

data L=N N|C C|Sy Sy deriving(Eq,Ord,Show);   data Fun=Op Op|Lam[V]E|Adv'd Adv E deriving(Eq,Ord,Show)
data N=J J|O O|F F   deriving(Eq,Ord,Show);   data Adv=Fold|Scan|Each            deriving(Eq,Ord,Enum,Show,Bounded)
 
data Op=(:~)|(:!)|(:@)|(:#)|(:$)|(:%)|(:^)|(:&)|(:*)|(:-)|(:=)|(:--)|(:+)|(:..)|(:.)|(:<)|(:>)|(:?)|(:|)
 deriving(Eq,Ord,Enum,Read,Show,Bounded)

data Ty=Ta TA|Tl TA|TL|TF|NT deriving(Eq,Ord,Show); data TA=Tj|To deriving(Eq,Ord,Show)
