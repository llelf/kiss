{-#language FlexibleInstances,FunctionalDependencies,NoMonomorphismRestriction,TypeApplications#-}
module P where
import Prelude hiding(map,seq); import qualified Data.Text as T; import qualified Data.Text.Encoding as T
import System.IO.Unsafe; import Control.Applicative; import Data.Foldable
import A; import AST.Unmarshal; import AST.Element; import qualified AST; import TS.K

(∘)=(.);(<∘>)=fmap∘fmap;(?)=(<|>);infixl 0?;trv=traverse;π=pure;nyi=error∘("nyi:"<>)

ps ::S->Maybe E;           ps =either(pure Nothing)k∘ps'
ps'::S->Either S(AST.K()); ps'=unsafePerformIO∘parseByteString @AST.K @() tree_sitter_k∘T.encodeUtf8∘T.pack

dyap f x=Ap f[x]; moap f x y=Ap f[x,y]

dap(AST.Dap _ a b v)=moap<$>kv v<*>kn a<*>ke b
map(AST.Map _   a f)=dyap<$>kt f<*>ke a
cap(AST.Cap _ _)    =nyi"cap"

kn (AST.Kn   _ x)=  ap=<<prj x ? parn=<<prj x ? list=<<prj x ?   n=<<prj x ? lam=<<prj x ? nyi"kn"
ke (AST.Ke   _ x)=  kt=<<prj x ?  map=<<prj x ?  dap=<<prj x ? cap=<<prj x ?      nyi"ke"
n  (AST.N    _ x)=int1=<<prj x ? intv=<<prj x ?  var=<<prj x ?       nyi"n"
kt (AST.Kt   _ x)=  kn=<<prj x ?   kv=<<prj x ?       nyi"kt"
kv (AST.Kv   _ x)=   v=<<prj x ?  avd=<<prj x
k  (AST.K    _ x)=  ke=<<prj x

v   (AST.V     _ x)=π∘Fun∘Op∘pop∘T.head$x where pop::A.C->Op;pop '_'=(:--);pop ','=(:..);pop c=read("(:"<>[c]<>")")
avd (AST.Avd _ a f)=Fun<∘>Adv'd<$>(π∘pad∘a'$a)<*>kt f where pad::A.C->Adv;pad '/'=Fold;pad '\\'=Scan;pad '\''=Each
lam (AST.Lam _ b v)=Fun<∘>Lam<$>args v<*>Seq<$>seq b
int1(AST.Int1  _ x)=π∘A∘N∘O∘read∘T.unpack$x
intv(AST.Intv  _ x)=Ls<∘>trv int1∘toList$x
list(AST.List  _ x)=Ls<$>seq x

ap  (AST.Ap _ a f)=Ap<$>ke f<*>seq a
parn(AST.Parn _ x)=ke=<<prj x

seq Nothing=π[]; seq(Just(AST.Seq _ x))=trv ke∘toList$x
args Nothing=π[]; args(Just(AST.Args _ x))=trv var'∘toList$x

var'(AST.Var _ x)=π∘T.unpack$x; var=Var<∘>var'; a'(AST.A _ x)=T.head x


