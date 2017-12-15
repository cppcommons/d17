/* Generated from test.scm by the CHICKEN compiler
   http://www.call-cc.org
   2017-12-15 16:44
   Version 4.13.0 (rev 68eeaaef)
   windows-mingw32-x86 [ manyargs dload ptables ]
   compiled 2017-12-11 on yves.more-magic.net (Linux)
   command line: -ignore-repository test.scm
   used units: library eval chicken_2dsyntax
*/

#include "chicken.h"


static int add2(int a, int b)
{
 return a + b;
}


static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_chicken_2dsyntax_toplevel)
C_externimport void C_ccall C_chicken_2dsyntax_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[3];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,18),40,97,100,100,50,32,105,110,116,49,53,32,105,110,116,50,54,41,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


/* from k34 */
C_regparm static C_word C_fcall stub3(C_word C_buf,C_word C_a0,C_word C_a1){
C_word C_r=C_SCHEME_UNDEFINED,*C_a=(C_word*)C_buf;
int t0=(int )C_unfix(C_a0);
int t1=(int )C_unfix(C_a1);
C_r=C_fix((C_word)add2(t0,t1));
return C_r;}

C_noret_decl(f_39)
static void C_ccall f_39(C_word c,C_word *av) C_noret;
C_noret_decl(f_45)
static void C_ccall f_45(C_word c,C_word *av) C_noret;
C_noret_decl(f_27)
static void C_ccall f_27(C_word c,C_word *av) C_noret;
C_noret_decl(f_42)
static void C_ccall f_42(C_word c,C_word *av) C_noret;
C_noret_decl(f_19)
static void C_ccall f_19(C_word c,C_word *av) C_noret;
C_noret_decl(f_22)
static void C_ccall f_22(C_word c,C_word *av) C_noret;
C_noret_decl(f_25)
static void C_ccall f_25(C_word c,C_word *av) C_noret;
C_noret_decl(f_52)
static void C_ccall f_52(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(f_48)
static void C_ccall f_48(C_word c,C_word *av) C_noret;

/* k37 in k23 in k20 in k17 */
static void C_ccall f_39(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_39,2,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_42,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_52,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("test.scm:11: add2");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[0]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[0]+1);
av2[1]=t3;
av2[2]=C_fix(5);
av2[3]=C_fix(8);
tp(4,av2);}}

/* k43 in k40 in k37 in k23 in k20 in k17 */
static void C_ccall f_45(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_45,2,av);}
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_UNDEFINED;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* add2 in k23 in k20 in k17 */
static void C_ccall f_27(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_27,4,av);}
t4=C_i_foreign_fixnum_argumentp(t2);
t5=C_i_foreign_fixnum_argumentp(t3);
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=stub3(C_SCHEME_UNDEFINED,t4,t5);
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}

/* k40 in k37 in k23 in k20 in k17 */
static void C_ccall f_42(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_42,2,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_45,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_48,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[1]);
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[1]+1);
av2[1]=t3;
tp(2,av2);}}

/* k17 */
static void C_ccall f_19(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_19,2,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_22,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k20 in k17 */
static void C_ccall f_22(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_22,2,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_25,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_chicken_2dsyntax_toplevel(2,av2);}}

/* k23 in k20 in k17 */
static void C_ccall f_25(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_25,2,av);}
a=C_alloc(6);
t2=C_mutate2((C_word*)lf[0]+1 /* (set! add2 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_27,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_39,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("test.scm:10: print");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[2]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[2]+1);
av2[1]=t3;
av2[2]=C_fix(33);
tp(3,av2);}}

/* k50 in k37 in k23 in k20 in k17 */
static void C_ccall f_52(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_52,2,av);}
C_trace("test.scm:11: print");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[2]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[2]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point

void C_ccall C_toplevel(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) {C_kontinue(t1,C_SCHEME_UNDEFINED);}
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(C_calculate_demand(3,c,2));
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void*)C_toplevel,c,av);}
toplevel_initialized=1;
if(C_unlikely(!C_demand_2(21))){
C_save(t1);
C_rereclaim2(21*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,3);
lf[0]=C_h_intern(&lf[0],4, C_text("add2"));
lf[1]=C_h_intern(&lf[1],25, C_text("\003sysimplicit-exit-handler"));
lf[2]=C_h_intern(&lf[2],5, C_text("print"));
C_register_lf2(lf,3,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_19,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

/* k46 in k40 in k37 in k23 in k20 in k17 */
static void C_ccall f_48(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_48,2,av);}
t2=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
((C_proc)C_fast_retrieve_proc(t2))(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[11] = {
{C_text("f_39:test_2escm"),(void*)f_39},
{C_text("f_45:test_2escm"),(void*)f_45},
{C_text("f_27:test_2escm"),(void*)f_27},
{C_text("f_42:test_2escm"),(void*)f_42},
{C_text("f_19:test_2escm"),(void*)f_19},
{C_text("f_22:test_2escm"),(void*)f_22},
{C_text("f_25:test_2escm"),(void*)f_25},
{C_text("f_52:test_2escm"),(void*)f_52},
{C_text("toplevel:test_2escm"),(void*)C_toplevel},
{C_text("f_48:test_2escm"),(void*)f_48},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|eliminated procedure checks: 2 
o|safe globals: (add2) 
o|folded constant expression: (+ (quote 11) (quote 22)) 
o|replaced variables: 3 
o|removed binding forms: 7 
o|contracted procedure: k54 
o|removed binding forms: 4 
o|substituted constant variable: r55 
o|removed binding forms: 1 
o|simplifications: ((##core#call . 2)) 
o|  call simplifications:
o|    ##sys#foreign-fixnum-argument	2
o|contracted procedure: k30 
o|contracted procedure: k34 
o|removed binding forms: 2 
o|replaced variables: 1 
o|removed binding forms: 1 
*/
/* end of file */
