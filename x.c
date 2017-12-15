#include "chicken.h"

int main()
{
 C_word x;

 CHICKEN_run(CHICKEN_default_toplevel);
 CHICKEN_eval_string("(print (+ 3 4))", &x);
 CHICKEN_eval_string("(+ 3 4)", &x);
 printf( "%d\n", C_immediatep(x) );
 printf( "%d\n", C_fitsinfixnump(x) );
 printf( "%d\n", C_ufitsinfixnump(x) );
 int d = C_unfix(x);
 printf( "%d\n", d );
 return 0;
}
