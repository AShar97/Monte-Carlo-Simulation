#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void plot(long long int a, long long int m, long long int x0, long long int v0){
  long long int q = m/a, r = m%a, k;
  long long int x = x0, v = 0;
	float u, u1;
	u1 = (x/(float)m);
	do{
		u = u1;
//		x = (((a * x) + 0) % m);
///#		x = (((a * (x % q))) - ((x / q) * r));
		k = x/q;
		x = ((a * (x - (k * q))) - (k * r));
		if (x < 0) {x += m;}
		u1 = (x/(float)m);
		printf("(%g,%g)\t", u, u1);
		v++;
	}while(x != x0 && v <= v0);
	printf("\n");
}

void gen(long long int a, long long int m, long long int x0, long long int * f, long long int v0){
	long long int q = m/a, r = m%a, k;
  long long int x = x0, v = 0; int i = 0;
	do{
		x = (((a * x) + 0) % m);
///#		x = (((a * (x % q))) - ((x / q) * r));
		k = x/q;
		x = ((a * (x - (k * q))) - (k * r));
		if (x < 0) {x += m;}
		i = ((x/(float)m)/0.05);
		if (i == 20) {i--;}
		*(f + i) += 1;
		v++;
	}while(x != x0 && v <= v0);

//Printing the frequency table:
	for(int j = 0; j < 20; j++){
//		printf("\t\t%.2f-%.2f\t:\t%llu\n", (j/(float)20), ((j+1)/(float)20), *(f+j));
		printf("%.2f-%.2f,%llu\n", (j/(float)20), ((j+1)/(float)20), *(f+j));
	}
}

void main(){
	long long int x0, f[3][3][20] = {0};

//Frequency Distributuions:
	printf("Frequency Distributions :");
//Part (a)
	printf("\nFor a = 16807, m = 2^(31) - 1, and x0 = 1 :");
	printf("\n\t#For 1000 values ::\n");
	gen(16807, (pow(2,31) - 1), 1, *(*(f+0)+0), 1000);

	printf("\n\t#For 10000 values ::\n");
	gen(16807, (pow(2,31) - 1), 1, *(*(f+0)+1), 10000);

	printf("\n\t#For 100000 values ::\n");
	gen(16807, (pow(2,31) - 1), 1, *(*(f+0)+2), 100000);

//Part (b)
	printf("\n\nFor a = 40692, m = 214748339, and x0 = 1 :");
	printf("\n\t#For 1000 values ::\n");
	gen(40692, 2147483399, 1, *(*(f+1)+0), 1000);

	printf("\n\t#For 10000 values ::\n");
	gen(40692, 2147483399, 1, *(*(f+1)+1), 10000);

	printf("\n\t#For 100000 values ::\n");
	gen(40692, 2147483399, 1, *(*(f+1)+2), 100000);

//Part (c)
	printf("\n\nFor a = 40014, m = 214748356, and x0 = 1 :");
	printf("\n\t#For 1000 values ::\n");
	gen(40014, 2147483563, 1, *(*(f+2)+0), 1000);

	printf("\n\t#For 10000 values ::\n");
	gen(40014, 2147483563, 1, *(*(f+2)+1), 10000);

	printf("\n\t#For 100000 values ::\n");
	gen(40014, 2147483563, 1, *(*(f+2)+2), 100000);

/*
//Printing (u_i, u_i+1)
	printf("Plot data :");
	printf("\nFor a = 16807, m = 2^(31) - 1, and x0 = 1 :\n\tThe values (u_i, u_i+1) are :: ");
	plot(16807, (pow(2,31) - 1), 1, 100000);
*/
}
