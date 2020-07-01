{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module T where
import A; import K; import P; import Data.Function;import Control.Applicative
import Shower;        import Control.Monad;import Test.QuickCheck hiding(Fun)

trip(e::E)=ps(pp e)👌Just e where(👌)=(==)`on`fmap post

post::E->E; post=rwE lst1∘rwE comn∘rwE bdic∘rwE lst1

lst1(Ap(Fun(Op(:..)))[x])=π$Ls[x]; lst1 _=ε;  bdic(Ap(Fun(Op(:!)))[Ls k,Ls v])|len k==len v=π$Dic k v; bdic _=ε
comn(Com(Ap f[x,Nil])g)  =π$Ap f[x,g];                     comn(Com f@Fun{} g)=π$Ap f[g];                comn _=ε
coms(Ap f[x,Ap g[y,Nil]])=π$Com(Ap f[x,Nil])(Ap g[y,Nil]); coms(Ap f[Ap g[y,Nil]])=π$Com f(Ap g[y,Nil]); coms _=ε
apn (Ap(Ap f[x,Nil])[y]) =π$Ap f[x,y]; apn _=ε

{-comn(Com f(Ap g a))|Nil∉a=π$Ap f[Ap g a]-}


qc=quickCheckWith(stdArgs{maxSuccess=2000,maxSize=500,maxShrinks=5000})
qc' x y z=quickCheckWith(stdArgs{maxSuccess=x,maxSize=y,maxShrinks=z})
ε=empty;(∉)=notElem
