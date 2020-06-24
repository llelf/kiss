#include<tree_sitter/parser.h>
#include"c.h"
#define wup x->lookahead
#define adv()  x->advance(x,0)
#define mark() x->mark_end(x)
typedef unsigned U; enum TokenType{UGH};

V*tree_sitter_k_external_scanner_create      (){R 0;}
V tree_sitter_k_external_scanner_destroy     (V*p){}
U tree_sitter_k_external_scanner_serialize   (V*p,S b){R 0;}
V tree_sitter_k_external_scanner_deserialize (V*p,CS b,U n){}
I tree_sitter_k_external_scanner_scan        (V*p,TSLexer*x,const C*vs){
 P(wup!='-',0);adv();mark();if(wup=='.')adv();P('0'>wup||wup>'9',0);R x->result_symbol=UGH,1;}
