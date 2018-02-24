#include <stdio.h>
void gen(int a, int x0, int * f){
	long long int x = x0; int i;
	do{
		x = (((a * x) + 0) % 244944);
		i = ((x/(float)244944)/0.05);
		if (i == 20) {i--;}
		*(f + i) += 1;
	}while(x != x0);
	for(i = 0; i < 20; i++){
		*(f + 20) += *(f + i);
	}
}
void main(){
	int x0[5], f[5][21] = {0}, i;
//Generating the frequeny distributions, along with taking input the value of x0s:	
	printf("For a = 1597, b = 0 three values of x0:\n");
	for(i = 0; i < 3; i++){
		printf("x0_%d ? ", (i+1));
		scanf("%d", &x0[i]);
		gen(1597, x0[i], *(f + i));
	}

	printf("For a = 51749, b = 0 two values of x0:\n");
	for(i = 3; i < 5; i++){
		printf("x0_%d ? ", (i+1 - 3));
		scanf("%d", &x0[i]);
		gen(51749, x0[i], *(f + i));
	}
//Printing the frequency distibutions:
	printf("With a = 1597, b = 0, m = 244944 :\n");
	for(i = 0; i < 3; i++){
		printf("And, with the value of x0 = %d, the frequency distribution is:\n", x0[i]);
		for(int j = 0; j < 20; j++){
			printf("\t%.2f-%.2f\t:\t%d\n", (j/(float)20), ((j+1)/(float)20), f[i][j]);
		}
		printf("Total numbers : %d\n\n", f[i][20]);
	}

		printf("\n\nWith a = 51749, b = 0, m = 244944 :\n");
	for(i = 3; i < 5; i++){
		printf("And, with the value of x0 = %d, the frequency distribution is:\n", x0[i]);
		for(int j = 0; j < 20; j++){
			printf("\t%.2f-%.2f\t:\t%d\n", (j/(float)20), ((j+1)/(float)20), f[i][j]);
		}
		printf("Total numbers : %d\n\n", f[i][20]);
	}
}
