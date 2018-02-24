#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void plot(unsigned long long int a, unsigned long long int m, unsigned long long int x0, unsigned long long int v0, FILE * fp){
//  unsigned long long int q = m/a, r = m%a, k;
  unsigned long long int x = x0, v=0;
	float u, u1;
	u1 = (x/(float)m);
	do{
		u = u1;
//    k = x/q;
//		x = ((a * (x - (k * q))) - (k * r));
//		if (x < 0) {x += m;}
///#    x = (((a * (x % q))) - ((x / q) * r));
    x = (((a * x) + 0) % m);
    u1 = (x/(float)m);
//		fprintf(fp, "%g,%g\n", u, u1);
    fprintf(fp, "%g %g\n", u, u1);
    v++;
	}while(x != x0 && v <= v0);
//	fprintf(fp, "\n");
}

void main(){
	FILE * fp = fopen("plot.tsv", "w+");
  plot(16807, (pow(2,31) - 1), 1, 100000, fp);
  fclose(fp);
  printf("\n");
}
