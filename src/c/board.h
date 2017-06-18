
#ifndef __BOARD_H__
#define __BOARD_H__

typedef union 
{
	char element[3][3];
	char eline[9];
	
}T_ELEMENT_RECD;




typedef enum 
{
	STATUS_PLAYING = 9,
	STATUS_X_WON   = 10,
	STATUS_O_WON   = 11,
	STATUS_FAIR    = 12
}E_STATUS;

typedef enum 
{
	PLAYER_X = 0,
	PLAYER_O
}E_PLAYER;

typedef struct
{
    int number;
    int indexs[4];
}T_ASSO_WEIGHT;


typedef struct
{
    int iXwon;
    int iOwon;
    int iFair;
}T_STATISTIC;

typedef struct 
{
public:
    void init();
public:
	T_ELEMENT_RECD rcd;
	int status;
    int weight[2][8];
    unsigned char choices[9];
    unsigned char choice_number;
    int seq[9];
    int seqNumber;
}T_ELEMENT;

typedef struct
{
public:
    void init();
public:
    T_ELEMENT board[3][3];	
	int status;
    int weight[2][8];
    unsigned char choices[9];
    unsigned char choice_number;
    unsigned char ucBlocks[9];
    unsigned char block_number;
    int seq[81];
    int seqNumber;
}T_BOARD_RECD;


void print(T_BOARD_RECD &board);
int getRandom(int max);
void timerun_ms();
int play_one_step(T_ELEMENT &grid, E_PLAYER player);
int gridUpStatus(T_ELEMENT &grid, E_PLAYER player, int index);
void game_play();

#endif

