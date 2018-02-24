#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void list(int v0, float * u){
  long long int x[v0]; int v = 0;
  x[0] = 1;
	u[0] = (x[0]/(float)pow(2,31));
//LCG
	while(v < 17 && v < v0){
    v++;
		x[v] = (((16807 * x[v-1]) + 0) % (long long int)(pow(2,31) - 1));
		u[v] = (x[v]/(float)(pow(2,31) - 1));
  }
//Extended Fibonacci Generator
  while(v < v0){
    v++;
		x[v] = ((x[v-17] + x[v-5]) % (long long int)pow(2,31));
    u[v] = (x[v]/(float)pow(2,31));
	}
  
}

void data(float * u, int n){
  for (int i = 1; i < n; i++) {
    printf("(%g,%g)\t", u[i-1], u[i]);
  }
}

float mean(float * u, int n){
  float m = 0;
  for (int i = 0; i < n; i++) {
    m += (u[i]/(float)n);
  }
  return m;
}

float variance(float * u, int n, float mu){
  float v = 0;
  for (int i = 0; i < n; i++) {
    v += (pow((u[i]- mu),2)/(float)n);
  }
  return v;
}

float rho(float * u, int n, int l, float mu){
  float num = 0, denom = 0;

  for (int t = l+1; t <= n; t++) {
    num += ((u[t] - mu) * (u[t - l] - mu));
  }

  for (int t = 1; t <= n; t++) {
    denom += pow((u[t]- mu),2);
  }

  return (num / denom);
}

void main(){
  float u[1000000];
  list(100000, u);

  printf("Plot data :");
  printf("\n\t#For 1000 values ::\n");
  data(u, 1000);

  printf("\n\t#For 10000 values ::\n");
  data(u, 10000);

  printf("\n\t#For 100000 values ::\n");
  data(u, 100000);
  printf("\n");

  float mu = mean(u, 1000);
  printf("\nThe sample mean for 1000 values is %g.\n", mu);

  float var = variance(u, 1000, mu);
  printf("\nThe sample variance for 1000 values is %g.\n\n", var);

//Autocorrelation of lags
  for (int i = 1; i <= 5; i++) {
    printf("Autocorrelation of lag %d, with 1000 generated values, is %g.\n", i, rho(u, 1000, i, mu));
  }
}
