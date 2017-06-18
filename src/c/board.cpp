

#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "board.h"
#include "windows.h"

char index2gridIndex[9] = {2,4,6,9,11,13, 16, 18, 20};
int g_weights[9] = {1,  2,  4,
                    8, 16, 32,
                    64,128,256};




T_ASSO_WEIGHT g_assiWeight[9] = {
              {3, {0, 3, 6}},
              {2, {0, 4}},
              {3, {0, 5, 7}},
              {2, {1, 3}},
              {4, {1, 4, 6, 7}},
              {2, {1, 5}},
              {3, {2, 3, 7}},
              {2, {2, 4}},
              {3, {2, 5, 6}}
    
    };
#define MAX_RANDOM_NUM  (32768)
int iRandoms[MAX_RANDOM_NUM] = {0};
int iRandCurNum = MAX_RANDOM_NUM;


void T_ELEMENT::init()
{
    memset(&(rcd), '-', sizeof(T_ELEMENT_RECD));
    status = STATUS_PLAYING;
    unsigned char choice[9] = {0,1,2,3,4,5,6,7,8};
    memcpy(choices, choice, 9);
    choice_number = 9;

}


void T_BOARD_RECD::init()
{
    for (int i = 0; i < 3; i++)
        for(int j = 0; j < 3; j++)
            board[i][j].init();

    status = STATUS_PLAYING;
    unsigned char choice[9] = {0,1,2,3,4,5,6,7,8};
    memcpy(choices, choice, 9);
    choice_number = 9;   
 
}

void timerun_ms()
{
    unsigned int t1 = GetTickCount();

    int j = 0;
    for (int i = 0; i < 81*100000; i++)
    {
        getRandom(9);
    }

    unsigned int t2 = GetTickCount();

    printf("time diff %d ms\n",  t2 - t1);
}

int getRandom(int max)
{
    if (iRandCurNum == MAX_RANDOM_NUM)
    {
        time_t t = time(0);
        srand((unsigned int)t);

        
        for (int i = 0; i < MAX_RANDOM_NUM; i++)
        {
            iRandoms[i] = rand();
        }  
        iRandCurNum = 0;      
    }

    int r = iRandoms[iRandCurNum++];

    return r % max; 
}



char getSign(E_PLAYER player)
{
    if (PLAYER_X == player)
    {
        return 'X';
    }
    else
    {
        return 'O';
    }
}

void gridUpChoices(int randn, unsigned char *pChoices, unsigned char &iChoice_number)
{
    for (int i = randn; i < iChoice_number; i++)
    {
        pChoices[i] = pChoices[i+1];
    }

    iChoice_number--;

}

int whoWon(E_PLAYER player)
{
    if (PLAYER_X == player)
    {
        return STATUS_X_WON;
    }
    else
    {
        return STATUS_O_WON;
    }
}

int calcStatus(int *pWeight, E_PLAYER player, int index, int iChoiceNum)
{
    int status = STATUS_PLAYING;

    T_ASSO_WEIGHT &tAweight = g_assiWeight[index];
    for (int i = 0; i < tAweight.number; i++)
    {
        int wonIndex = tAweight.indexs[i];
        int &w = pWeight[wonIndex];
        if (++w == 3)
        {
            status = whoWon(player);
            break;
        }
    }
    
    if ((STATUS_PLAYING == status) && (0 == iChoiceNum))
    {
        status = STATUS_FAIR;
    }

    return status;
}

int gridUpStatus(T_ELEMENT &grid, E_PLAYER player, int index)
{
    int status = STATUS_PLAYING;

    T_ASSO_WEIGHT &tAweight = g_assiWeight[index];
    for (int i = 0; i < tAweight.number; i++)
    {
        int wonIndex = tAweight.indexs[i];
        int &w = grid.weight[player][wonIndex]; 
        if (++w == 3)
        {
            status = whoWon(player);
            break;
        }
    }
    

    
    
    if ((STATUS_PLAYING == status) && (0 == grid.choice_number))
    {
        status = STATUS_FAIR;
    }


    if ((STATUS_PLAYING == status) && (1 == grid.choice_number))
    {

        int tStas = STATUS_PLAYING;
        int lastIndex = grid.choices[0];

        T_ASSO_WEIGHT &tAweight = g_assiWeight[lastIndex];
        
        for (int i = 0; i < tAweight.number; i++)
        {
            int wonIndex = tAweight.indexs[i];
            int wX = grid.weight[PLAYER_X][wonIndex];
            if (++wX == 3)
            {
                tStas = STATUS_X_WON;
                break;
            }

            int wO = grid.weight[PLAYER_O][wonIndex];
            if (++wO == 3)
            {
                tStas = STATUS_O_WON;
                break;
            }
        }    

    
        if ((STATUS_X_WON != tStas) && (STATUS_O_WON != tStas))
        {
            status = STATUS_FAIR;
        }
    }

    grid.status = status;
    
    return status;
}

int play_one_step(T_ELEMENT &grid, E_PLAYER player)
{
    if (STATUS_PLAYING != grid.status)
    {
        return grid.status;
    }

    int randn = 0;
    int index = 0;
    
    if (grid.choice_number > 1)
    {
        randn = getRandom(grid.choice_number);
        index = grid.choices[randn];
    }
    else
    {
        randn = 0;
        index = grid.choices[0];
    }
    
    grid.rcd.eline[index] = getSign(player);

    grid.choice_number--;

    grid.choices[randn] = grid.choices[grid.choice_number];

    
    gridUpStatus(grid, player, index);

    
    return index;
} 

E_PLAYER getNextPlayer(E_PLAYER ePlayer)
{
    return (PLAYER_X == ePlayer)? PLAYER_O : PLAYER_X;
}

void UpdatStatistics(int status, T_STATISTIC &tStatic)
{
    if (STATUS_X_WON == status)
    {
        tStatic.iXwon++;
    }
    else if (STATUS_O_WON == status)
    {
        tStatic.iOwon++;
    }
    else if (STATUS_FAIR == status)
    {
        tStatic.iFair++;
    }
}

void game_play()
{
    T_STATISTIC tStatic = {0};
    T_BOARD_RECD tBoard = {0};

    unsigned int t1 = GetTickCount();
    
    for (int itest = 0; itest < 100000; itest++)
    {
    
    /*init*/

    memset(&tBoard, 0, sizeof(tBoard));
    tBoard.init();

    /*player X first*/
    int randn = getRandom(tBoard.choice_number);
    int curr_index = tBoard.choices[randn];
    int row = curr_index / 3; int volu = curr_index % 3;
    E_PLAYER ePlayer = PLAYER_X;

    while (true)
    {
        //printf("row volu [%d, %d] begin\n", row, volu);
        
        if (tBoard.ucBlocks[curr_index])
        {
            randn = getRandom(tBoard.choice_number);
            curr_index = tBoard.choices[randn];
            row = curr_index / 3; volu = curr_index % 3;

            //printf("choice row volu [%d, %d] \n", row, volu);
        }
        
        T_ELEMENT &tEle = tBoard.board[row][volu];

        int next_index = play_one_step(tEle, ePlayer);
        
        //printf("next index %d\n", next_index);

        if (tEle.status > STATUS_PLAYING)
        {
            //printf("row volu [%d, %d] status %d\n", row, volu, tEle.status);
            //printf("block %d\n", curr_index);
            tBoard.ucBlocks[curr_index] = 1;
            tBoard.block_number++;

            for (int i = 0; i < tBoard.choice_number; i++)
            {
                if (curr_index == tBoard.choices[i])
                {
                    tBoard.choice_number--;
                    tBoard.choices[i] = tBoard.choices[tBoard.choice_number];

                    break;
                }
            }
                        
            tBoard.status = calcStatus(tBoard.weight[ePlayer], ePlayer, curr_index, tBoard.choice_number);

            if ((STATUS_X_WON == tBoard.status)
                || (STATUS_O_WON == tBoard.status)
                || (STATUS_FAIR == tBoard.status))
            {
                UpdatStatistics(tBoard.status, tStatic);
                break;
            }

        }

        curr_index = next_index;
        row = curr_index / 3; volu = curr_index % 3;

        ePlayer = (PLAYER_X == ePlayer)? PLAYER_O : PLAYER_X;

    }
    
    }

    //print(tBoard);
    unsigned int t2 = GetTickCount();

    printf("time diff %d ms\n",  t2 - t1);
    
    printf("xwon %d, owon %d, fair %d\n", tStatic.iXwon, tStatic.iOwon, tStatic.iFair);
    
}

void print_line(char *lines)
{
    static char line[25] = {'|', '|', 
                     ' ', '|', ' ', '|',' ', '|', '|',
                     ' ', '|', ' ', '|',' ', '|', '|',
                     ' ', '|', ' ', '|',' ', '|', '|',
                     '\n', '\0'};
    
    for (int i = 0; i < 9; i++)
    {
        line[index2gridIndex[i]] = lines[i];
    }

    printf("%s", line);

}

void construct_line(T_BOARD_RECD &board, int outindex)
{
    T_ELEMENT &e1 = board.board[outindex][0];
    T_ELEMENT &e2 = board.board[outindex][1];
    T_ELEMENT &e3 = board.board[outindex][2];

    {
    char line[9] = {0};
    memcpy(line,     e1.rcd.element[0], 3);
    memcpy(line + 3, e2.rcd.element[0], 3);
    memcpy(line + 6, e3.rcd.element[0], 3);
    print_line(line);
    }

    {
    char line[9] = {0};
    memcpy(line,     e1.rcd.element[1], 3);
    memcpy(line + 3, e2.rcd.element[1], 3);
    memcpy(line + 6, e3.rcd.element[1], 3);
    print_line(line);
    }   

    {
    char line[9] = {0};
    memcpy(line,     e1.rcd.element[2], 3);
    memcpy(line + 3, e2.rcd.element[2], 3);
    memcpy(line + 6, e3.rcd.element[2], 3);
    print_line(line);
    }   
}



void print(T_BOARD_RECD &board)
{
    printf("||-----||-----||-----||\n");
    construct_line(board, 0); 
    printf("||-----||-----||-----||\n");
    construct_line(board, 1); 
    printf("||-----||-----||-----||\n");
    construct_line(board, 2); 
    printf("||-----||-----||-----||\n");   
}




