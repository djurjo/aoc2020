#include<stdlib.h>
#include <stdio.h>

long unsigned int *getLines();

long unsigned int *getLines(){
    FILE* file = fopen("input9.txt", "r");
    int  counter = 0;
    static long unsigned int numbers[9999];

    while (!feof(file)) {
	fscanf(file, "%lu", &numbers[counter++]);
    }
    
    return numbers;
}


int isCombination(int pointer, long unsigned int *numbers2)
{
    int sol = 0;
    int i, j;
    for (i=0; i < 26; i ++){
	for (j=0; j < 26; j++){
	    if (! (j == i)){
		if (numbers2[pointer - i] + numbers2[pointer - j] == numbers2[pointer]){
		    sol = 1;
		    break;
		}
	}
    }
    }
    return sol;
}

long unsigned int sequence(long unsigned int target,int nonvalid,  long unsigned int *numbers2 ){
    int i, j;
    long unsigned int sol = 0;
    for (i = 0; i < 9999; i ++){
	if (i == nonvalid){
	    ;
	}
	else{
	    long unsigned int accum = 0;
	    long unsigned int smallest = numbers2[i];
	    long unsigned int biggest = numbers2[i];
	    for (j=i; j<9999; j++){
		accum = accum + numbers2[j];
	        if (numbers2[j] < smallest){
		    smallest = numbers2[j];
		}
		if (numbers2[j] > biggest){
		    biggest = numbers2[j];
		}
		if (accum == target){
		    sol = smallest + biggest;
		    i = 9999;
		    break;
		}
		else{
		    if (accum < target){
			;
		    }
		    else{
			break;
		    }
		}

	    }
	}
    }
    return sol;
}


int main(){
    long unsigned int * numbers;
    long unsigned int header[26];
    int i = 0;
    numbers = getLines();
    //printf("%d", getLines());
    while (! (numbers[i] == 0)){
	if (i < 25){
	    header[i] = numbers[i];
	}
	else{
	    //printf("%d \n", i );
	    if (! isCombination(i, numbers)){
		int nonvalid;
		printf("Star1 %lu \n", numbers[i]);
		nonvalid = i;
		printf("Start2 %lu \n", sequence(numbers[i], nonvalid, numbers));
		break;
	    }
	}
	i ++ ;
    }
    return 0;
}

