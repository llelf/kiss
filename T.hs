{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module T where
import A; import K; import P; import Data.Function; import Test.QuickCheck hiding(Fun)

trip(e::E)=ps(pp e)👌Just e where(👌)=(==)`on`fmap post

post::E->E; post=list1 where list1=rwE f where f(Ap(Fun(Op(:..)))[x])=Ls[x]; f x=x

qc=quickCheckWith(stdArgs{maxSuccess=2000,maxSize=500,maxShrinks=5000})
