{-# language FunctionalDependencies,NoMonomorphismRestriction,PartialTypeSignatures,PatternSynonyms,TypeApplications,
             TypeOperators,TypeFamilies,ViewPatterns #-}
module P (ps,ps',plE,rwE,univ,vars) where
import Prelude hiding(exp,map,seq);import Data.Functor.Identity;import Data.Functor.Const;import qualified Data.List as L
import qualified Data.Text as T;import qualified Data.Text.Encoding as T;import Data.Foldable; import Control.Applicative
import A; import qualified AST; import TreeSitter.K; import AST.Unmarshal; import AST.Element;    import System.IO.Unsafe

(∘)=(.);(<∘>)=fmap∘fmap;(∈)=elem;(⊗)=(<>);(??)=flip;(?)=(<|>);infixl 0?;trv=traverse;seqA=sequenceA;nyi=error∘("nyi:"⊗)
pattern T=True;pattern Nt=Nothing;pattern Jt x=Just x;type(?)=Maybe;type(+)=Either;σ=T.unpack;rev=reverse;π=pure;ρ=read

ps ::S->(?)E;      ps""=π Nil; ps s=either(π Nt)k∘ps'$s
ps'::S->S+AST.K(); ps' =unsafePerformIO∘parseByteString @AST.K @() tree_sitter_k∘T.encodeUtf8∘T.pack

moap f x=Ap f[x]; dyap f x y=Ap f[x,y]; nyap f=Ap f[]; comp x y=Com x y; fopm=Fun$Op(:-); kt x=kn=<<prj x ? kv=<<prj x

dam (AST.Dam _ a b _)=dyap<$>π fopm<*>kn a<*>ke b
dap (AST.Dap _ a b v)=dyap<$> kv v <*>kn a<*>ke b
map (AST.Map _   a f)=moap<$> kt f <*>ke a

pdam(AST.Pdam _    Nt a _)=dyap<$>π fopm<*>kn a<*>π Nil {-dyap<$>π fopm<*>kn a<*>maybe(π Nil)kpe z-}
pdam(AST.Pdam _(Jt z) a _)=Com<$>(dyap<$>π fopm<*>kn a<*>π Nil)<*>kpe z
pdap(AST.Pdap _    Nt a v)=dyap<$>kv v<*>kn a<*>π Nil
pdap(AST.Pdap _(Jt z) a v)=Com<$>(dyap<$>kv v<*>kn a<*>π Nil)<*>kpe z

pmap(AST.Pmap _ Nt Nt  (Jt b))=kv b {-nyap<$>kv b-}
pmap(AST.Pmap _(Jt z)(Jt f)Nt)=Com<$>kt f<*>zz {-moap<$>kt f<*>zz-} where zz=kpe=<<prj z

ke (AST.E      x)=  kn=<<prj x ?  kv=<<prj x ? map=<<prj x ? dap=<<prj x ? dam=<<prj x ? ass=<<prj x ? exp=<<prj x ? nyi"ke"
lit(AST.Lit  _ x)=int1=<<prj x ?intv=<<prj x ?bit1=<<prj x ?bitv=<<prj x ?flt1=<<prj x ? var=<<prj x ?sym1=<<prj x ? nyi"n"
kn (AST.N      x)=  ap=<<prj x ?parn=<<prj x ?list=<<prj x ?dict=<<prj x ? lit=<<prj x ? lam=<<prj x ?   nyi"ne"
kpe(AST.Pe     x)=pmap=<<prj x ?pdap=<<prj x ?pass=<<prj x ?pdam=<<prj x ?    nyi"pe"
kv (AST.V      x)=   v=<<prj x ? avd=<<prj x ?  io=<<prj x
kk (AST.Kk   _ x)=  ke=<<prj x ? kpe=<<prj x

k  (AST.K _ ks _)=(fx<$>)$Seq<∘>trv kk∘toList$ks where fx(Seq[x])=x;fx x=x

v   (AST.Op    _ x)=π∘Fun∘Op∘pop∘T.head$x where pop::A.C->Op;pop '_'=(:--);pop ','=(:..);pop c=ρ("(:"⊗[c]⊗")")
avd (AST.Avd _ a f)=Fun<∘>Adv'd<$>(π∘pad∘a'$a)<*>kt f where pad::A.C->Adv;pad '/'=Fold;pad '\\'=Scan;pad '\''=Each
lam (AST.Lam _ b v)=Fun<∘>Lam<$>args v<*>Seq<$>(seq=<<b);   exp (AST.Exp   _ x)=Fun∘e2lam<$>ke x
int1(AST.Int1  _ x)=π∘pint∘σ$x;                             intv(AST.Intv  _ x)=π∘Ls$pint<∘>words∘σ$x
bit1(AST.Bit1  _ x)=π∘pbit∘nosuf∘σ$x;                       bitv(AST.Bitv  _ x)=π∘Ls$pbit∘π<∘>nosuf∘σ$x
flt1(AST.Flt1  _ x)=π∘pflt∘σ$x;
sym1(AST.Sym1  _ x)=π∘A∘Sy∘tail∘σ$x;                        io  (AST.Io    _ x)=π∘Fun∘Io∘ρ∘π∘head∘σ$x

pbit,pint,pflt::S->E; pbit=A∘N∘B∘toEnum∘ρ;pint=A∘N∘O∘ρ;pflt=A∘N∘F∘f where f x|('.':_)<-rev x=ρ(x⊗"0");f('-':x)=ρ("-0"⊗x);f x=ρ('0':x)

dict(AST.Dict   _  x _)=kv2d<∘>trv kv$x where kv(AST.Kv _ k v)=(,)<$>(A∘Sy<∘>var'$k)<*>maybe(π Nil)kk v
list(AST.List      _ x)|Jt x<-x=Ls<$>seq x|T=π$Ls[]
ass (AST.Ass  _ e Nt v)|Jt e<-e=Ass<$>kn v<*>ke e|T=Ass<$>kn v<*>π Nil; ass _=nyi"cmplx.ass"
pass(AST.Pass _ e Nt v)=Ass<$>kn v<*>kpe e; pass _=nyi"cmplx.pass"

ap  (AST.Ap _ a f)|Jt a<-a=Ap<$>ke f<*>seq a|T=Ap<$>ke f<*>π[Nil]
parn(AST.Parn _ x)=kk=<<prj x
seq (AST.Seq _  x)=fx<∘>seq'∘toList$x where fx=(f=<<)∘L.group∘(⊗[Nil])∘(Nil:) where f(Nil:n)=n; f x=x
seq'=trv f where f::(AST.SEMI:+:AST.Kk)_->(?)E; f x=Nil<$prj @AST.SEMI x ? kk=<<prj @AST.Kk x

args Nt=π[]; args(Jt(AST.Args _ x))=trv var'∘toList$x;                               nosuf=rev∘dropWhile(∈"ijfbo")∘rev
var'(AST.Var _ x)=π∘σ$x; var=Var<∘>var'::_->(?)E; a'(AST.A _ x)=T.head x::C;  kv2d::[(E,E)]->E; kv2d=uncurry Dic∘unzip


univ a=a:a^.plE∘(∘univ);vars a=[x |Var x<-v a]where v(Fun Lam{})=[];v a=a:a^.plE∘(∘v)
rwE::(E->(?)E)->_;rwE f=tfE$maybe??rwE f<*>f;tfE f=f∘over plE(tfE f); plE::_=>(E->p E)->_; plE f=z where{g=trv f;
 z(Ls x)=Ls<$>g x;z(Ass v e)=Ass<$>f v<*>f e;z(Fun(Adv'd a e))=Fun∘Adv'd a<$>f e;z(Fun(Lam v e))=Fun∘Lam v<$>f e;
 z(Ap x y)=Ap<$>f x<*>g y;   z(Com x y)=Com<$>f x<*>f y;    z(Cond x)=Cond<$>g x;     z(Dic x y)=Dic<$>g x<*>g y;
 z(Seq x)=Seq<$>g x;z x=π x}

over l f=runIdentity∘l(Identity∘f);view l=getConst∘l Const;(^.)=flip view;infixl 8^.
e2lam e=Lam??e$case L.intersect(π<$>"xyz")∘vars$e of[]->[];(maximum->(y:_))->π<$>['x'..y]

