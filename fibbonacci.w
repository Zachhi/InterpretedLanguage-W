var n = 12; //this will return the fibbonacci number of 10

var a = 0;
var b = 1;
var c = 0;
var i = 2;

if(n == 0)
{
    print a;
}
else
{
    while(i <= n)
    {
        c = a + b;
        a = b;
        b = c;
        i = i + 1;
    }
}
print b;