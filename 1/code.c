#include <stdio.h>
#include <math.h>

long double f(long double x){
	//Given function f(x).
	long double f = ((3*x*x) - expl(x));
	return f;
}
long double f1(long double x){
	//Function f'(x) i.e. differentiation of given function f(x).
	long double f1 = ((6*x) - expl(x));
	return f1;
}

int main(){
	long double x = 0, x1 = 0, temp = 0;
	//Taking input the value of initial guess x0.
	printf ("Enter the value of initial guess x0, to approximate the root of the given equation '3*x^2 - e^x = 0' by Newton-Raphson method. ");
	scanf ("%Lf",&x);
	x1 = x;
	//Iterations of Newton-Raphson method.
	do
	{
		temp = x1;
		x1 = (temp - (f(temp)/f1(temp)));
	} while (fabsl(temp - x1) > 0.00001);
	//Output of the calculated root.
	printf("Root of the given equation '3*x^2 - e^x = 0' as approximated by Newton-Raphson method, using the entered x0 = %Lg, is x = %Lf.\n",x,x1);
	return 0;
}