{-#language FlexibleInstances,NoMonomorphismRestriction,ParallelListComp,PatternSynonyms,PostfixOperators,RankNTypes,
    ScopedTypeVariables,StandaloneDeriving,TupleSections,TypeApplications,TypeOperators,UnicodeSyntax,ViewPatterns#-}
module K where
import Data.Int;import Data.List;import Text.Printf(printf);import Data.Function;import Data.Functor
import A;import P(ps);         import Control.Monad.State;import Test.QuickCheck hiding(discard,Fun)

type ΓΓ=[(V,E)];type Γ=[(V,(Sc,E))];type Env=Γ;type Σ=Γ;type M=StateT Σ((+)S);type(+)=Either
data Sc=Gl|Lo deriving(Eq,Show);pattern R x=Right x;pattern Er x=Left x;pattern T=True::Bool

(∘)=(.);(⊗)=(<>);π=pure;φ=lift;(??)=flip;(<∘>)=fmap∘fmap;infixl 4<∘>;len=length;sw=show;trv=traverse;seqA=sequenceA
rev=reverse;fmt=printf;(∅)=mempty;η=fromIntegral;er=φ∘Er;nyi=er∘("nyi."⊗);wow=φ∘R;such=wow; meh=error"xkcd.com/292"

k0=A∘N∘J$0;k1=A∘N∘J$1;kemp=Ls[];k00=Ls[k0,k0];k01=Ls[k0,k1];[ksp,kca]=A∘C<$>" A";[kes,kfoo]=A∘C<∘>["","foo"]<&>Ls
kid=Fun∘Lam["x"]$Var"x";kadd=Fun∘Lam xy∘Ap(Fun$Op(:+))$Var<$>xy where xy=π<$>"xy"

run::ΓΓ->E->S+E; run z=fst<∘>run' z; run' z e=ev e`runStateT`((Gl,)<∘>z)

getV'::V->M(Sc,E); getV' v=get>>=φ∘maybe(Er"var")R∘lookup v; getV::V->M E;getV=snd<∘>getV'
setV::Sc->V->E->M E; setV s v x=x<$modify((e:)∘deleteBy((==)`on`fst)e) where e=(v,(s,x))

ev::E->M E; ev(Seq x)=last<$>trv ev x;ev(Ls x)=Ls∘rev<$>trv ev(rev x);ev(Ap f x)=do{b<-rev<$>trv ev(rev x);a<-ev f;eap a b}
ev(Ass v e)=do{(s,_)<-getV' v;ev e>>=setV s v};ev(Var v)=getV v;ev x=wow x

eap::E->[E]->M E; eap(Fun(Op o))a=eop o a;eap(Fun(Lam v e))a=elam v e a;eap(Fun(Adv'd d e))a=nyi"adv"
eap(Ls l)i=nyi"v@";eap Ap{}_=nyi"part-ap";eap Nil _=nyi"nil@";eap A{}_=er"atm@";eap Var{}_=meh;eap Ass{}_=meh;eap Seq{}_=meh

elam::[V]->E->[E]->M E
elam v b x|len v/=len x=nyi"part-λ"|T=do{seqA∘zipWith(setV Lo)v$x;r<-ev b;modify∘filter$(/=Lo)∘fst∘snd;π r}

eop::Op->[E]->M E
eop(:+) [x,y]=a2(φn"+"(+))x y;                            eop(:#)[x]=φ$siz x;     eop(:#)[x,Ls y]=tak x y
eop(:*) [x,y]=a2(φn"*"(*))x y;   eop(:*) [x]=frt x;       eop(:!)[x]=iot x;
eop(:-) [x,y]=a2(φn"-"(-))x y;   eop(:-) [x]=a1(-^)x;       
eop(:..)[x,y]=jin x y;           eop(:..)[x]=wow∘Ls$[x];
eop o x=nyi$fmt"op:%s/%d"(sw o)(len x)

a1::(E->M E)->E->M E; a1 f(Ls a)=Ls<$>trv(a1 f)a; a1 f a=f a

a2::(E->E->M E)->E->E->M E; a2 f(Ls a)(Ls b)|len a==len b=Ls<∘>seqA$zipWith(a2 f)a b|T=er"⨝.len"
a2 f a(Ls b)=Ls<$>trv(a2 f a)b;a2 f(Ls a)b=a2(f??)b(Ls a);a2 f a b=f a b

instance Num N where(+)=ltn2(+);(-)=ltn2(-);(*)=ltn2(*);abs=ltn abs;signum=ltn signum;fromInteger=J∘η
ltn::(∀a.Num a=>a->a)->_;ltn(+)(J a)=J$(+)a;ltn(+)(O a)=O$(+)a
ltn2::(∀a.Num a=>a->a->a)->_;ltn2(+)=z where z(O a)(O b)=O a+O b;z(O a)(J b)=O$a+η b;z(J a)(O b)=O$η a+η b;z(J a)(J b)=J$a+b
φn::S->(N->_)->E->E->M E;φn _(×)(A(N a))(A(N b))=π∘A∘N$a×b;φn s _ _ _=er$s⊗"₂.¬num";(-^)(A(N a))=π∘A∘N$(-a);(-^)_=er"-₁.¬num"

class Ix'd a where{kvs::a->[(E,E)]}; instance Ix'd[E]where kvs=zip$A∘N∘η<$>[0..]
--pattern Ix'd<-(isIx'd->T);isIx'd Ls{}=T;isIx'd Dic{}=T;isIx'd _=False

iot(A(N(J i)))|i<0=er"dom"|T=wow∘Ls$A∘N∘J<$>[0..i-1];iot Ls{}=nyi"odo";iot _=er"typ"
tak(A(N(J i)))|i>=0=wow∘Ls∘take(η i)∘cycl|T=such∘Ls∘rev∘take(-η i)∘rev;tak _=π$nyi"#₂case"; cycl[]=[];cycl x=cycle x
frt(Ls' x[])=wow∘maybe kes id$x;frt(Ls a)=wow$a!!0;frt x=such x; jin x y=wow∘Ls$ls x⊗ls y where ls(Ls a)=a;ls a=[a]
siz(Ls a)=R∘A∘N∘η∘len$a;siz _=Er"rank"


class PP α where pp::α->S
instance PP E   where{pp(A l)=pp l;pp(Var v)=v;pp(Ap a x)=fmt"(%s)[%s]"(pp a)$semi$pp<$>x;pp(Ass v e)=fmt"%s:%s"v$pp e;
                      pp(Fun f)=pp f;pp(Ls s)=pr∘semi$pp<$>s;pp Nil="";pp(Seq x)=semi$pp<$>x}
instance PP Fun where pp(Op o)=pp o;pp(Lam a b)=fmt"{[%s]%s}"(semi a)(pp b);pp(Adv'd a x)=(pp x⊗)∘π∘("/\\':"!!)∘fromEnum$a
instance PP L   where pp(N(J x))=sw x⊗"j";pp(N(O x))=sw x⊗"o";pp(C c)=fmt"\"%c\""c;pp(Sy s)='`':s
instance PP Op  where pp(:--)="_";pp(:..)=",";pp o=π∘(!!2)∘sw$o

semi=intercalate";";pr=fmt"(%s)";esc::S->S;esc(c:s)|c`elem`"\"\n"='\\':c:esc s|T=c:esc s;esc _=[]

arb::Arbitrary a=>Gen a;arb=arbitrary
instance Arbitrary L   where arbitrary=N∘J<$>arb
instance Arbitrary E   where arbitrary=frequency[(4,A<$>arb),(2,Ls<$>arb),(1,Fun<$>arb)]
instance Arbitrary Fun where arbitrary=Op<$>arb
instance Arbitrary Op  where arbitrary=elements[minBound..]
