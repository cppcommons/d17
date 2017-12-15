#>
static int add2(int a, int b)
{
 return a + b;
}
<#

(define add2 (foreign-lambda int "add2" int int))

(print (+ 11 22))
(print (add2 5 8))
