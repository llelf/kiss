{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module T where
import A; import K; import P; import Data.Function;import Control.Applicative
import Shower;        import Control.Monad;import Test.QuickCheck hiding(Fun)

trip(e::E)=ps(pp e)👌Just e where(👌)=(==)`on`fmap post

post::E->E; post=rwE lst1∘rwE pmap

lst1(Ap(Fun(Op(:..)))[x])=π$Ls[x]; lst1 _=empty;
pmap(Ap f@(Fun(Op _))[Nil])=π f; pmap _=empty;

qc=quickCheckWith(stdArgs{maxSuccess=2000,maxSize=500,maxShrinks=5000})
qc' s=quickCheckWith(stdArgs{maxSuccess=2000,maxSize=500,maxShrinks=s})
