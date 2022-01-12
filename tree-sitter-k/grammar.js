/* k9 parser (c)2020–2022 lelf
   e:nve| te|ε-see| ad  n- e|n:e|:e  t:n v:tA|V n:e[E*]| (E)|(E*)|{[..]E}|{v:E..}|[[.]..]|lit:iIfFnbBNcCv
     dap|map|  pe |hocs: dam|ass|exp  /v  avd|     ap  |parn|list|  lam  |  dict |  tabl |              ?
 */                          A=alias, F=field, O=optional, C=choice,  R=repeat, R1=repeat1;
                             S=seq, P=prec, D=P.dynamic, T=token, RS=(e,s)=>S(e,R(S(s,e)));
module.exports=grammar({name:'k',
                 rules:{k:$=>S(O($.nb),R($.KSEP),F('k',$._k), R(S($.KSEP,F('k',O($._k))))),
                       _k:$=>C(D(2,$._v),$._e,$._pe),

_pe:$=>D(-1,C($.pass,$.pdap,$.pdam,$.pmap,$.pexp)),
   pass:$=>D(1,P(1,S(F('v',$._n),         F('f',O($.op)),':',              F('a',$._pe)))),
   pexp:$=>                                            S(':',                    $._pe),
   pdap:$=>    D(2,S(F('a',$._n),R($._sp),F('v',$._v),          R($._sp),O(F('z',$._pe)))),
   pdam:$=>    D(2,S(F('a',$._n),         F('v',A($._ugh,$.op)),R($._sp),O(F('z',$._pe)))),
   pmap:$=>    D(1,C(F('v',$._v),       S(F('f',$._t),          R($._sp),  F('z',$._pe)))),

_e:$=>D(-1,C($.ass,$.dap,$.dam,$.map,$.exp,$._t)),
   ass:$=>D(1,P(1,S(F('v',$._n),         F('f',O($.op)),':',          F('a',O($._e))))),
   exp:$=>                                            S(':',                  $._e),
   dap:$=>    D(2,S(F('a',$._n),R($._sp),F('v',$._v),          R($._sp),F('b',$._e))),
   dam:$=>    D(2,S(F('a',$._n),         F('v',A($._ugh,$.op)),R($._sp),F('b',$._e))),
   map:$=>    D(1,S(F('f',$._t),                               R($._sp),F('a',$._e))),

_t:$=>C($._n,$._v),             _v:$=>C($.avd,$.op,$.io), avd:$=>S(F('f',$._t),F('a',$.a)),
_n:$=>C($.ap,$.parn,$.list,$.dict,$.tabl,$.lit,$.lam,$.lan),

parn:$=>S('(',$._k,    ')'),
list:$=>S('(',O($.seq),')'),             ap:$=>P(2,S(F('f',$._e),'[',F('a',O($.seq)),']')),
dict:$=>S('[',O($.kvls),']'),                              kvls:$=>RS(F('kv',$.kv),$.SEMI),
tabl:$=>S('[','[',O($.kvls),']',$.kvls,']'),      kv:$=>S(F('k',$.var),':',F('v',O($._k))),

lam: $=>S('{[',F('v',O($.args)),']',F('b',O($.seq)),'}'),            args:$=>RS($.var,';'),
lan: $=>S('{',F('b',O($.seq)),'}'),
seq: $=>C(R1($.SEMI),S(R($.SEMI),S($._k,R(S($.SEMI,O($._k)))))),

lit: $=>C($.int1,$.intv,$.flt1,$.fltv,$.sym1,$.symv,$.chr1,$.chrv,$.var), //--------move regex rubbish to lexer
int1:$=>/-?\d+[ijh]?/,            flt1:$=>/-?(\d+\.?|\d*\.\d+)(e-?\d+)?[fe]?/,
intv:$=>/-?\d+( -?\d+)+[ijh]?/,   fltv:$=>/-?(\d+\.?|\d*\.?\d+)(e-?\d+)?( -?(\d+\.?|\d*\.\d+)(e-?\d+)?)+[fe]?/,
sym1:$=>/`[\w.:]*/,               symv:$=>/(`[\w.]*)+/,
chr1:$=>/"([^\\\"]|\\.)"/,        chrv:$=>C(/""/,/"([^\\\"]|\\.)+"/),

io:$=>T(S(/\d/,':')),     op:$=>C('-',P(-1,':'),/[+*%!&|<>=~,^#_$?@.]/),      a:$=>/[\/\\\']:?/,
var:$=>/[a-zA-Z][a-zA-Z0-9]*/,  SEMI:$=>C(/;\s*/,/\n\s+/),  KSEP:$=>C(/;\s*/,/\n/),  _sp:$=>' ',
nb:$=>/\s+\/[^\n]+/,

},conflicts:$=>[[$.pmap,$.pdap,$._e],[$.pass,$.ass],[$.pmap,$._e],[$.pmap,$.pdap],[$.dap,$.map],
                [$.pmap,$._k,$._e],[$.map,$.pmap,$._e],[$.map,$.dap,$.pmap,$.pdap,$._e],[$.parn,$.seq]],
  externals:$=>[$._ugh],   inline:$=>[$._t,$.kvls],  supertypes:$=>[$._e,$._pe,$._n,$._v],
     extras:$=>[$.nb]
})
