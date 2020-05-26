{-#language PatternSynonyms#-}module A where
import Data.Int
type I=Int32;type J=Int64;type O=Integer;type S=String;type C=Char;type Sy=S;type V=S

data E=A L|Ls'(Maybe E)[E]|Var V|Ap E[E]|Ass V E|Fun Fun|Seq[E]|Nil{-|Dic[(E,E)]-}deriving(Eq,Show)
{- ==> E α=A..|L[α]|D[(α,α)] -}

pattern Ls x<-Ls' _ x where Ls(x:y)=Ls'(pure x)(x:y); Ls[]=Ls' Nothing[]

data L=N N|C C|Sy Sy deriving(Eq,Show);   data Fun=Op Op|Lam[V]E|Adv'd Adv E deriving(Eq,Show)
data N=J J|O O       deriving(Eq,Show);   data Adv=Fold|Scan|Each|Mod        deriving(Eq,Enum,Show,Bounded)
 
data Op=(:~)|(:!)|(:@)|(:#)|(:$)|(:%)|(:^)|(:&)|(:*)|(:-)|(:=)|(:--)|(:+)|(:..)|(:.)|(:<)|(:>)|(:?)|(:/)|(:\)|(:|)
 deriving(Eq,Enum,Show,Bounded)
