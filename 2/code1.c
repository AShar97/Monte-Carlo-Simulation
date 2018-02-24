#include <stdio.h>
void gen(int a, int b, int m, int x0, int * value, int v){
	int x = x0, i = 0;
	printf("[");
	do{
		printf(" %f",(x/(float)m));
		x = (((a * x) + b) % m);
		i += 1;
	}while(x != x0);
	printf(" ]\tRepetition after %d terms.\n", i);
	value[v] = i;
}
void main(){
	int x0, value[22] = {0}, v = 0;
	for(x0 = 0; x0 <= 10; x0++){
		printf("For a = 6, b = 0, m = 11, x0 = %d :\n\t",x0);
		gen(6, 0, 11, x0, value, v);
		v++;
	}
	for(x0 = 0; x0 <= 10; x0++){
		printf("For a = 3, b = 0, m = 11, x0 = %d :\n\t",x0);
		gen(3, 0, 11, x0, value, v);
		v++;
	}
	
	int max = 0;
	for(v = 0; v < 22; v++){
		if (max < value[v]){
			max = value[v];
		}
	}
	
	printf("The best choice/choices are (generating maximum, i.e. %d,terms without repetition):\n", max);
	
	for(v = 0; v < 22; v++){
		if (max == value[v]){
			if (v < 11){
				printf("\ta = 6, b = 0, m = 11, x0 = %d\n",(v));
			}
			else{
				printf("\ta = 3, b = 0, m = 11, x0 = %d\n",(v-11));
			}
		}
	}
}