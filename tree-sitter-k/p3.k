so::*"./tree-sitter-k.so"2:(,x)!,y

AP2::(x . y). z; Rnode:"iiii"; Rx:6#"i"; Rv1:(5#Ra),(Ra:0;); /fill registers so next arg goes to stack
libc:""2:{strlen:"i";strncpy:"sii"}; strof:{[p]libc[`strncpy][s:n#".";p;n:libc[`strlen]p];s}

root:  so["ts_tree_root_node";                "Ji"] /Node<-Tree*
cnew:  so["ts_tree_cursor_new";     "J",1_Rx,Rnode] /TreeCursor<-Node
cnode: so["ts_tree_cursor_current_node";      "JJ"] /Node<-TreeCursor*
cfield:so["ts_tree_cursor_current_field_name";,"J"] /S<-Node

tofst:so["ts_tree_cursor_goto_first_child";,"J"]; tonext:so["ts_tree_cursor_goto_next_sibling"; ,"J"]
toup: so["ts_tree_cursor_goto_parent";     ,"J"]; tparse:so["ts_parser_parse_string";         "iisi"]
nstr: so["ts_node_string";             Rx,Rnode];   ntyp:so["ts_node_type";                 Rx,Rnode]
nfrom:so["ts_node_start_byte";         Rx,Rnode];    nto:so["ts_node_end_byte";             Rx,Rnode]

K:so["tree_sitter_k";,"i"]; p:so["ts_parser_new";,"i"][0]; so["ts_parser_set_language";"ii"][p;K[0]];

show:{[c;d] cnode[n:4#0j;c]; typ:AP2[ntyp;Rv1;n]; from:AP2[nfrom;Rv1;n]; to:AP2[nto;Rv1;n];
 ` 0:((2*d)#" "), "∙ ", $[f:cfield[c];strof[f],":";""], strof[typ], "  ｢", (code@&&from,to-from), "｣\n"}

trv:{[c;d] show[c;d]; $[tofst c; toup[c],({[c;d]trv[c;d];tonext c}[;1+d]; :)/:c; c]}

code:"2+3*"; parse:{[s]code#:""; code,:s; t:tparse[p;0;code;#code]; root[r:4#0j;t];
                  ` 0:(:x,"\n\n")strof AP2[nstr;Rv1;r]; cur:3#0j; AP2[cnew;(,cur),1_Rv1;r]; trv[cur;0];}


\

 parse"6`div'42"
(k k: (dap a: (lit (int1)) v: (avd f: (lit (sym1)) a: (a)) b: (lit (int1))))

∙ k  ｢6`div'42｣
  ∙ k:dap  ｢6`div'42｣
    ∙ a:lit  ｢6｣
      ∙ int1  ｢6｣
    ∙ v:avd  ｢`div'｣
      ∙ f:lit  ｢`div｣
        ∙ sym1  ｢`div｣
      ∙ a:a  ｢'｣
    ∙ b:lit  ｢42｣
      ∙ int1  ｢42｣
