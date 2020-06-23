{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module T where
import A; import K; import P; import Data.Function;import Control.Applicative
import Shower;        import Control.Monad;import Test.QuickCheck hiding(Fun)

trip(e::E)=rwE bd e==e ==> ps(pp e)👌Just e where(👌)=(==)`on`fmap post

post::E->E; post=rwE bdic∘rwE lst1∘rwE fnilf
lst1(Ap(Fun(Op(:..)))[x])=π$Ls[x]; lst1 _=ε;   bdic(Ap(Fun(Op(:!)))[Ls k,Ls v])|len k==len v=π$Dic k v; bdic _=ε
fnilf(Ap f[])=π f;fnilf _=ε

qc=quickCheckWith(stdArgs{maxSuccess=2000,maxSize=500,maxShrinks=5000})
qc' x y z=quickCheckWith(stdArgs{maxSuccess=x,maxSize=y,maxShrinks=z})
ε=empty
