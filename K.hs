{-#language FlexibleContexts,FlexibleInstances,NoMonomorphismRestriction,PartialTypeSignatures,PatternSynonyms,RankNTypes,
           PostfixOperators,ScopedTypeVariables,TupleSections,TypeApplications,TypeOperators,UnicodeSyntax,ViewPatterns#-}
module K where
import A;import P;import Data.List;import Data.Int;import Text.Printf(printf);import Test.QuickCheck hiding(discard,Fun)
import Data.Char;import Data.Function;import Data.Functor.Identity;import Data.Functor.Const;import Control.Monad.State

type ΓΓ=[(V,E)];type Γ=[(V,(Sc,E))];type Env=Γ;type Σ=Γ;type(+)=Either;data Sc=Gl|Lo deriving(Eq,Show)
type M=StateT Σ((+)S); pattern R x=Right x; pattern Er x=Left x; pattern T=True::Bool;pattern NO=False

(∘)=(.);(⊗)=(<>);π=pure;φ=lift;(??)=flip;(<∘>)=fmap∘fmap;infixl 4<∘>;len=length;σ=show;trv=traverse;seqA=sequenceA
rev=reverse;fmt=printf;(∅)=mempty;η=fromIntegral;er=φ∘Er;nyi=er∘("nyi."⊗);wow=φ∘R;such=wow; meh=error"xkcd.com/292"

[k0,k1,k2]=A∘N∘O<$>[0..2];kemp=Ls[];k00=Ls[k0,k0];k01=Ls[k0,k1];k_2=Ls[Ls[k1,k0],k01];[ksp,kca]=A∘C<$>" A";
kid=Fun∘Lam["x"]$Var"x";kadd=Fun∘Lam xy∘Ap(fop(:+))$Var<$>xy where{xy=π<$>"xy"};kfoo=Ls$A∘C<$>"foo";fop=Fun∘Op

run::ΓΓ->E->S+E; run=(fst<∘>)∘run'; run' z e=ev e`runStateT`((Gl,)<∘>z)

getV'::V->M(Sc,E); getV' v=get>>=φ∘maybe(Er"var")R∘lookup v; getV::V->M E;getV=snd<∘>getV'
setV::Sc->V->E->M E; setV s v x=x<$modify((e:)∘deleteBy((==)`on`fst)e) where e=(v,(s,x))

ev::E->M E; ev(Seq x)=last<$>trv ev x;ev(Ls x)=Ls∘rev<$>trv ev(rev x);ev(Ap f x)=do{b<-rev<$>trv ev(rev x);a<-ev f;eap a b}
ev(Cond[c,a,b])=do{i<-ev c;ev$if ist i then a else b};ev Cond{}=nyi"¬3.cond";ev(Var v)=getV v
ev(Ass(Var v)e)=do{(s,_)<-getV' v;ev e>>=setV s v};ev(Ass _ _)=nyi"ass.cmlx";ev x=wow x

ty::E->Ty;ty(A(N(O _)))=Ta To;ty(A(N(J _)))=Ta Tj;ty(A(N(B _)))=Ta Tb;ty(Ls x)|[Ta t]<-nub∘sort$ty<$>x=Tl t|T=TL;ty(Fun _)=TF;ty _=NT
ist::E->B;ist(A(N x))=x/=0;ist(Ls[])=NO;ist Ls{}=T

eap::E->[E]->M E; eap(Fun(Op o))a=eop o a;eap(Fun(Lam v e))a=elam v e a
eap(Fun(Adv'd Fold e))[a]=efld e a;eap Fun{} _=nyi"fun.smth";
eap(Ls l)i=nyi"v@";eap Ap{}_=nyi"part-ap";eap Nil _=nyi"nil@";eap A{}_=er"atm@";eap Var{}_=meh;eap Ass{}_=meh;eap Seq{}_=meh

elam::[V]->E->[E]->M E
elam v b x|len v/=len x=nyi"part-λ"|T=do{seqA∘zipWith(setV Lo)v$x;r<-ev b;modify∘filter$(/=Lo)∘fst∘snd;π r}
efld::E->E->M E; efld _ x@(Ls[])=frt x;efld e(Ls(x:y))=foldM(eap e<∘>(∘π)∘(:))x y;efld _ _=nyi"fold.¬ls"

eop::Op->[E]->M E
eop(:+) [x,y]=a2(φn"+"(+))x y;                            eop(:#)[x]=φ$siz x;     eop(:#)[x,Ls y]=tak x y
eop(:*) [x,y]=a2(φn"*"(*))x y;   eop(:*) [x]=frt x;       eop(:!)[x]=iot x;
eop(:-) [x,y]=a2(φn"-"(-))x y;   eop(:-) [x]=a1(-^)x;     eop(:<)[x]=φ$gup x
eop(:..)[x,y]=jin x y;           eop(:..)[x]=wow∘Ls$[x];
eop o x=nyi$fmt"op:%s/%d"(σ o)(len x)

a1::(E->M E)->E->M E; a1 f(Ls a)=Ls<$>trv(a1 f)a; a1 f a=f a

a2::(E->E->M E)->E->E->M E; a2 f(Ls a)(Ls b)|len a==len b=Ls<∘>seqA$zipWith(a2 f)a b|T=er"⨝.len"
a2 f a(Ls b)=Ls<$>trv(a2 f a)b;a2 f(Ls a)b=a2(f??)b(Ls a);a2 f a b=f a b

instance Num N where(+)=ltn2(+);(-)=ltn2(-);(*)=ltn2(*);abs=ltn abs;signum=ltn signum;fromInteger=O∘η
ltn::(∀a.Num a=>a->a)->_;ltn(+)(J a)=J$(+)a;ltn(+)(O a)=O$(+)a
ltn2::(∀a.Num a=>a->a->a)->_;ltn2(+)=z where z(O a)(O b)=O$a+b;z(O a)(J b)=O$a+η b;z(J a)(O b)=O$η a+η b;z(J a)(J b)=J$a+b
φn::S->(N->_)->E->E->M E;φn _(×)(A(N a))(A(N b))=π∘A∘N$a×b;φn s _ _ _=er$s⊗"₂.¬num";(-^)(A(N a))=π∘A∘N$(-a);(-^)_=er"-₁.¬num"
nup::N->N->N;nup(O a)_=O a;nup(J a)(O _)=O∘η$a;nup(J a)(J _)=J a

class Ix'd a where{kvs::a->[(E,E)]}; instance Ix'd[E]where kvs=zip$A∘N∘η<$>[0..]
--pattern Ix'd<-(isIx'd->T);isIx'd Ls{}=T;isIx'd Dic{}=T;isIx'd _=False

iot(A(N(O i)))|i<0=er"dom"|T=wow∘Ls$A∘N∘O<$>[0..i-1];iot Ls{}=nyi"odo";iot _=er"typ"
tak(A(N(O i)))|i>=0=wow∘Ls∘take(η i)∘cycl|T=such∘Ls∘rev∘take(-η i)∘rev;tak _=π$nyi"#₂case"; cycl[]=[];cycl x=cycle x
frt(Ls[])=nyi"*()";frt(Ls a)=wow$a!!0;frt x=such x; jin x y=wow∘Ls$ls x⊗ls y where ls(Ls a)=a;ls a=[a]
siz(Ls a)=R∘A∘N∘η∘len$a;siz _=Er"rank";gup(Ls a)=R∘Ls$A∘N∘η<$>sortOn(a!!)[0..len a-1];gup _=Er">:typ"

class PP α where pp::α->S
instance PP Fun where pp(Op o)=pp o;pp(Lam a b)=fmt"{[%s]%s}"(semi a)(pp b);pp(Io x)=σ x⊗":";
                      pp(Adv'd a x)=let p(Fun(Adv'd{}))=pp x;p _=cmv x in (p x⊗)∘π∘("/\\'"!!)∘fromEnum$a
instance PP L   where pp(N(J x))=σ x;pp(N(O x))=σ x;pp(N(B x))=fmt"%db"∘fromEnum$x;pp(N(F x))=σ x;pp(C c)=fmt"\"%c\""c;pp(Sy s)='`':s
instance PP Op  where pp(:--)="_";pp(:..)=",";pp o=π∘(!!2)∘σ$o
instance PP E   where{pp(Ls[x])=',':prpf x; pp x@(Ls s)|TL<-ty x=pr∘semi$pp<$>s|Tl Tb<-ty x=(⊗"b")$head∘pp<$>s|T=ict" "$pp<$>s;
                      pp(A l)=pp l;    pp Nil=(∅); pp(Fun f)=pp f;pp(Var v)=v;pp(Ass x e)=cmn x⊗":"⊗pp e;pp(Seq x)=semi$pp<$>x;
                      pp(Dic[][])="{}";--pp(Dic k v)=fmt"{%s}"∘semi∘zipWith((∘pp)∘(⊗)∘(⊗":")∘pp)k$v;
                      pp(Dic k v)=pp$Ap(fop(:!))[Ls k,Ls v];
                      pp(Ap o@AO[x,Nil])|x/=Nil=cmn x⊕pp o;pp(Ap f a)|Nil∈a=psap f a;pp(Ap o@AO[x,y])=cmn x⊕pp o⊕prpf y;
                      pp(Ap o@AO[x])=pp o⊕prpf x;pp(Ap f[])=pp f;pp(Ap a x)=psap a x}

wp A{}=T;wp Fun{}=NO;wp Var{}=T;wp(Ap AO _)=NO;wp(Ap _((Nil∈)->T))=NO;wp(Ap _[])=NO;wp Ap{}=T;wp _=NO

(∈)=elem;hd[]='\0';hd(x:_)=x;lt=hd∘rev;pattern AO<-(let ao(Fun(Op _))=T;ao(Fun(Adv'd _(ao->T)))=T;ao _=NO in ao->T)
prpf x@Fun{}=pr∘pp$x; prpf x=pp x;x⊕y=x⊗s⊗y where s|isDigit(lt x),'.'<-hd y=" "|lt x∈".-",isDigit(hd y)=" "|T=(∅)
cmv x@Fun{}=pp x;cmv x=cmn x;     cmn x=p x where p(wp->T)=pp x;p(Ls[x])=pr(',':prpf x);p Ls{}=pp x;p x=ppr x
psap f x=fmt"%s[%s]"(cmv f)∘semi$pp<$>x
ict=intercalate;semi=ict";";pr=fmt"(%s)";ppr=pr∘pp;esc::S->S;esc(c:s)|c∈"\"\n"='\\':c:esc s|T=c:esc s;esc _=[]

arb::_=>Gen a;arb=arbitrary;frq=frequency;elms=elm[minBound..];smol q=sized$(resize??q)∘(`div`3);tiny=smol∘smol
elm=elements;alist=Ls<∘>(<∘>)A∘smol∘listOf$arb @L;avar=Var<$>elm["x","y","foo"];asy=A∘Sy<∘>elm$π<$>"abc";gs=genericShrink
aargs=frq[(3,π<$>smol a),(3,(∘π)∘(:)<$>smol a<*>smol a),(1,choose(3,5)>>=vectorOf??tiny a)]where a=frq[(7,arb),(1,π Nil)]
adic=uncurry Dic∘unzip<∘>smol∘listOf$(,)<$>asy<*>(A<$>arb); nan(A N{})=NO;nan _=T

instance Arbitrary L   where arbitrary=N<$>frq[(2,O<$>arb),(1,F<$>arb),(1,B<$>arb)]; shrink=π[N 0]
instance Arbitrary E   where arbitrary=frq[(4,A<$>arb),(2,alist),(1,Ls<$>smol arb),(1,avar),(1,adic),(1,Fun<$>arb),
                              (2,Ap<$>frq[(5,Fun<$>arb),(1,arb)]<*>aargs),
                              (1,Ass<$>frq[(4,avar),(1,smol arb`suchThat`nan)]<*>arb)]
                             shrink(Var _)=[Var"x"];shrink(Ap f x)=f:x⊗[p|p@(Ap _ a)<-Ap<$>gs f<*>gs x,a/=[]];
                             shrink x=sh x⊗gs x where sh(Ls x)=x;sh _=(∅)
instance Arbitrary Fun where arbitrary=frq[(5,Op<$>elms),(1,Adv'd<$>elms<*>arb),(1,Io<$>choose(0,9))]
                             shrink=π[Op(:+)]

