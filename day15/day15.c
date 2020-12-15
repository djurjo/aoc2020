#include<stdlib.h>
#include <stdio.h>




//int lastTimeSeen(int x, int y);
//int game[2021] = {-1, 1, 20 , 11, 6 , 12, 0};
int last[30000000] = {0}; 

void initialize(){
    last[1] = 1;
    last[20] = 2;
    last[11] = 3;
    last[6] = 4;
    last[12] = 5;
    //last[0] = 6;
}

void play(){
    long long int number = 0; 
    int turn = 7; // With ith counter I have the i-1 currentN
    while (turn < 30000001){
	
	int lastPos = last[number]; //I get the last pos of the previus number
	//printf("%i \n", lastPos);
	last[number] = turn - 1; // I update that value
	
	if (lastPos == 0){ //If the value did not appear my number to pos turn is 0 
	    number = 0; // If it doesnt appear before I say 0!
	}
	else{
	    number =  (turn - 1) -  lastPos;
	    if (turn == 2020){printf("%lli \n", number);}
	}
	if (turn == 2020){
	    printf("Star 1 %lli \n", number);
	 }
	if (turn == 30000000){
	    printf("Star 2 %lli \n", number);

	}
	turn ++ ;
	/*
	if (prevIndx == -1){
	    game[i] = 0 ;
	}
	else{
	    game[i] = (i-1) - prevIndx;
	}
	*/
    }
}
/*
int lastTimeSeen(int currentPos, int target){
    int sol = -1;
    for (int i = currentPos -1 ; 0 < i; i --){
	//printf("target %i and now im in %i \n", target, game[i]);
	if (game[i] == target){
	    sol = i;
	    break;
	}
    }
    return sol;
}

*/

int main(){
    initialize();
    play();

    //printf("Star 1 is %i", game[2020]);
    //printf("Star2 is %i", game[30000000]);
    return 0;
}
