#include <stdio.h>
void main(){
	int x0, x;
	float u, u1;
	printf("x0 ? ");
	scanf("%d", &x0);
	x = x0;
	u1 = (x/(float)2048);
	do{
		u = u1;
		x = (((1229 * x) + 1) % 2048);
		u1 = (x/(float)2048);
		printf("(%g,%g)\t", u, u1);
	}while(x != x0);
	printf("\n");
}