#include<tree_sitter/api.h>
#include<string.h>
#include"c.h"
#define TC(f) ts_tree_cursor_##f
#define TN(f) ts_node_##f
#define SS(u,s,n) I a=TN(start_byte)(n),b=TN(end_byte)(n);C u[b-a+1];strncpy(u,s+a,b-a);u[b-a]=0;

TSLanguage*tree_sitter_k(); TSLanguage*K; ZI shows=1;

I ttv(TSTreeCursor*c,S s,I d){
 TSNode n=TC(current_node)(c); SS(u,s,n); CS t=TN(type)(n); CS f=TC(current_field_name)(c);
 O("%*s∙ ",2*d,""); O("%s%s%s%s%s ",f?f:"",f?":":"",t,TN(has_error)(n)?"*":"",TN(is_extra)(n)?"+":"");
 $(TN(is_missing)(n),O("MISSING"))$(shows&&strcmp(t,u),O("｢%s｣",u)); O("\n");
 P(!TC(goto_first_child)(c),d); do ttv(c,s,1+d);while(TC(goto_next_sibling)(c)); TC(goto_parent)(c);
 R d;
}

V trv(TSNode r,S s,I d){
 SS(u,s,r); O("%*s ",2*d,"");
 $(TN(is_missing)(r),O("MISSING\n"))O("%s `%s'\n",TN(type)(r),u);
 I cn=TN(child_count)(r);N(cn,{TSNode n=TN(child)(r,i); trv(n,s,1+d);});
}

I main(I ac,S*av){
 S s=av[1]; $(!strcmp(s,"-s"),shows=0,s=av[2]);
 TSParser*p=ts_parser_new(); ts_parser_set_language(p,K=tree_sitter_k());
 TSTree*t=ts_parser_parse_string(p,0,s,strlen(s));
 TSNode r=ts_tree_root_node(t); O("%s\n\n",ts_node_string(r));

 //trv(r,s,0);
 TSTreeCursor c=TC(new)(r); ttv(&c,s,0);
}

