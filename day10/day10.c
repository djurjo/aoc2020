#include<stdlib.h>
#include <stdio.h>
#include<stdbool.h>

unsigned int *getLines();

unsigned int *getLines(){

    FILE* file = fopen("input10.txt", "r");
    int  counter = 0;
    static unsigned int numbers[9999];
    
    while (!feof(file)) {
	fscanf(file, "%u", &numbers[counter++]);
    }

    return numbers;
}

int countElems (unsigned int *numbers){
    int counter =0;
    while (numbers[counter] != 0) {
	counter++;
    }
    return counter;
}

void  sortArray(unsigned int *numbers, int size){
    //static long unsigned int * sortedArray[1000];

    // Worst sorting algorithm ever
    int i, j;
    for (i = 0; i < size; ++i){
	for(j = i; j< size; ++j){
	    if (numbers[i] > numbers[j]){
		unsigned int a = numbers[i];
		numbers[i] = numbers[j];
		numbers[j] = a;
	    }
	    else{
		;
	    }
	}
    }
}

unsigned int star1(unsigned int * input, int size){
    unsigned int n1 = 1;
    unsigned int n2 = 0;
    unsigned int n3 = 1;
    int i;
    printf("%d \n", size);
    for (i =0; i < size; i++){
	if (input[i] + 1 == input[i + 1]){
	    n1 = n1 + 1;
	}
	if  (input[i]  + 2 == input[i + 1]){
	    n2 = n2 + 1;
	}
	if (input[i] + 3 == input[i + 1]){
	    n3 = n3 + 1;
	}
    }
    printf("Number of 1s %u \n", n1);
    printf("Number of 2s %u \n", n2);
    printf("Number of 3s %u \n", n3);    
    return (n1 * n3);
}
long long int star2(unsigned int * input, int size){
    bool poss1j[size];
    bool poss2j[size];
    bool poss3j[size];
    int i;
    for (i = 0; i < size; i ++){
	poss1j[i] = false;
        poss2j[i] = false;
	poss3j[i] = false;
    }
    for (i = 0; i < size; i++){
	if (input[i] + 1 == input[i  +1]){
	    poss1j[i] = true ;
	}
	if (input[i] + 2 == input[i + 1]){
	    poss2j[i] = true;
	}
	if (input[i] + 3 == input[i + 1]){
	    poss3j[i]  = true;
	}
	
	if (i < size -1){
	    if (input[i] + 2 == input[i + 2]){
		poss2j[i] = true ;
	    }
	    if (input[i] + 3 == input[i +2]){
		poss3j[i]  = true ;
	    }
	}
	if (i < size -2){
	    if (input[i] + 3 == input[i + 3]){
		poss3j[i] = true;
	    }
	}
    }
    long long int paths[size];
    
    for (i = 0; i < size; i++){
	/*
	  printf("Jump 1 %d %i \n",i, poss1j[i]);
	printf("Jump 2 %d %i \n",i, poss2j[i]);
	printf("Jump 3 %d %i \n",i, poss3j[i]);
	printf("\n");
	*/
	paths[i] = 1;
    }

    for (i =0; i < size + 1; i++){
	int j;
	j = size - i;
	if ( poss1j[j] && poss2j[j] && poss3j[j]){
	    paths[j] = paths[j + 1] + paths[j + 2] + paths[j + 3];
	}
	else{
	    if ((poss1j[j] && poss2j[j]) || (poss1j[j]  && poss3j[j]) || (poss2j[j] && poss3j[j])){
		paths[j] = paths[j + 1] + paths[j + 2];
	    }
	    else{
		if (poss1j[j] || poss2j[j]  || poss3j[j]){
		    paths[j] = paths[j + 1];
		}
	    }    
	}
	//printf("%d %d \n",j, paths[j]);
    }
    printf("0 %lld \n", paths[0]);
    printf("1 %lld \n", paths[1]);
    printf("2 %lld \n", paths[2]);
    return paths[0] + paths[1] + paths[2];
}


int main(){
    unsigned int * input;
    input = getLines();
    int size = countElems(input);
    //printf("%i \n", size);
    //long unsigned int * sorted;
    sortArray(input, size);
    /*
    printf("sort? \n");
    for (i =0; i < size; i ++){
	printf("Sorted? %u \n", input[i]);
    }
    */
    printf("Star 1 %u \n", star1(input, size));
    //star2(input, size);
    printf("Star 2 %lld \n", star2(input, size));
    //printf("%d", getLines());
    return 0;
}

