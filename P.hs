{-# language FunctionalDependencies,NoMonomorphismRestriction,PartialTypeSignatures,TypeApplications,ViewPatterns #-}
module P (ps,ps',plE,rwE,univ,vars) where
import Prelude hiding(exp,map,seq); import Data.Functor.Identity;import Data.Functor.Const;import Control.Applicative
import qualified Data.Text as T;import qualified Data.Text.Encoding as T;import System.IO.Unsafe;import Data.Foldable
import A;import qualified AST;import TS.K;import AST.Unmarshal;import AST.Element;    import qualified Data.List as L

(∘)=(.);(<∘>)=fmap∘fmap;(??)=flip;(?)=(<|>);infixl 0?;trv=traverse;π=pure;nyi=error∘("nyi:"<>)

ps ::S->Maybe E;           ps =either(pure Nothing)k∘ps'
ps'::S->Either S(AST.K()); ps'=unsafePerformIO∘parseByteString @AST.K @() tree_sitter_k∘T.encodeUtf8∘T.pack

dyap f x=Ap f[x]; moap f x y=Ap f[x,y]; comp x y=Com x y

dap(AST.Dap _ a b v)=moap<$>kv v<*>kn a<*>ke b
map(AST.Map _   a f)=dyap<$>kt f<*>ke a
cap(AST.Cap _(Just a)b _)=comp<$>cap a<*>kv b; cap(AST.Cap _ _ b(Just a))=comp<$>kt a<*>kv b

ke (AST.Ke   _ x)=  kt=<<prj x ?  map=<<prj x ?  dap=<<prj x ? cap=<<prj x ? ass=<<prj x ? exp=<<prj x ? nyi"ke"
kn (AST.Kn   _ x)=  ap=<<prj x ? parn=<<prj x ? list=<<prj x ?   n=<<prj x ? lam=<<prj x ? nyi"kn"
n  (AST.N    _ x)=int1=<<prj x ? intv=<<prj x ?  var=<<prj x ?       nyi"n"
kt (AST.Kt   _ x)=  kn=<<prj x ?   kv=<<prj x ?       nyi"kt"
kv (AST.Kv   _ x)=   v=<<prj x ?  avd=<<prj x
k  (AST.K    _ x)=  ke=<<prj x

v   (AST.V     _ x)=π∘Fun∘Op∘pop∘T.head$x where pop::A.C->Op;pop '_'=(:--);pop ','=(:..);pop c=read("(:"<>[c]<>")")
avd (AST.Avd _ a f)=Fun<∘>Adv'd<$>(π∘pad∘a'$a)<*>kt f where pad::A.C->Adv;pad '/'=Fold;pad '\\'=Scan;pad '\''=Each
lam (AST.Lam _ b v)=Fun<∘>Lam<$>args v<*>Seq<$>seq b
exp (AST.Exp   _ x)=Fun∘e2lam<$>ke x
int1(AST.Int1  _ x)=π∘pint∘T.unpack$x; pint=A∘N∘O∘read::S->E
intv(AST.Intv  _ x)=π∘Ls$pint<∘>words∘T.unpack$x
list(AST.List  _ x)=Ls<$>seq x

ass (AST.Ass _ e Nothing v)=Ass<$>kn v<*>ke e; ass _=nyi"cmplx.ass"

ap  (AST.Ap _ a f)=Ap<$>ke f<*>seq a
parn(AST.Parn _ x)=ke=<<prj x

seq Nothing=π[]; seq(Just(AST.Seq _ x))=trv ke∘toList$x
args Nothing=π[]; args(Just(AST.Args _ x))=trv var'∘toList$x

var'(AST.Var _ x)=π∘T.unpack$x; var=Var<∘>var'; a'(AST.A _ x)=T.head x


rwE::(E->E)->_;rwE f=z where{z=f∘over plE z};univ a=a:a^.plE∘(∘univ);vars a=[x |Var x<-v a]where v(Fun Lam{})=[];v a=a:a^.plE∘(∘v)
plE::_=>(E->p E)->_; plE f=z where{g=trv f;z(Ls x)=Ls<$>g x;z(Ass v e)=Ass<$>f v<*>f e;z(Fun(Adv'd a e))=Fun∘Adv'd a<$>f e;
 z(Fun(Lam v e))=Fun∘Lam v<$>f e;z(Ap x y)=Ap<$>f x<*>g y;z(Cond x)=Cond<$>g x;z(Seq x)=Seq<$>g x;z(Com x y)=Com<$>f x<*>f y;z x=π x}

over l f=runIdentity∘l(Identity∘f);view l=getConst∘l Const;(^.)=flip view;infixl 8^.
e2lam e=Lam??e$case L.intersect(π<$>"xyz")∘vars$e of[]->[];(maximum->(y:_))->π<$>['x'..y]

