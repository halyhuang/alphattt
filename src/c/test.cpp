
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "board.h"
#include "assert.h"
#include "windows.h"

/*
{1,  2,  4,
 8, 16, 32,
 64,128,256};

*/
void test_status()
{
#if 0
    {
    /* X X X
       O O -
       - - - */
    T_ELEMENT ele = {{0}, STATUS_PLAYING,
        {{3, 0, 0, 1, 2, 0, 1, 0}, 
         {0, 24, 0, 8, 16, 0, 16, 16}
        },
        {5,6,7,8},
        4
    };  

    assert(STATUS_X_WON == gridUpStatus(ele, PLAYER_X, 2));
    }

    /* - O X
       - O X
       - O - */
    {
    T_ELEMENT ele = {{0}, STATUS_PLAYING,
        {{4, 32, 0, 0, 0, 36, 0, 4},
         {2, 16, 0, 0, 18, 0, 16, 16}
        },
        {0,3,6,8},
        4
    };

    assert(STATUS_O_WON == gridUpStatus(ele, PLAYER_O, 7));
    }

    /* O X O
       X X O
       - O X */
    {
    T_ELEMENT ele = {{0}, STATUS_PLAYING,
        {{2, 24, 256, 8, 18, 256, 272, 17},
         {5, 0, 128, 1, 128, 4, 1, 4},
        },
        {6},
        1
        };

    assert(STATUS_FAIR == gridUpStatus(ele, PLAYER_O, 5));
    }

    /* O X O
       O X X
       O O X */    
    {
    T_ELEMENT ele = {{0}, STATUS_PLAYING,
        {{2, 48, 256, 0, 18, 288, 272, 16},
         {5, 8, 128, 9, 128, 4, 1, 4},
        },
        {0},
        0
        };

    assert(STATUS_O_WON == gridUpStatus(ele, PLAYER_O, 6));
    }
    
    #endif
    printf("test_status ok!\n");
}

void test_grid()
{


    unsigned int t1 = GetTickCount();
    int wons[2] = {0};
    int fair = 0;
    for (int i = 0; i < 1000000; i++)
    {
        T_ELEMENT ele;
        memset(&ele, 0, sizeof(T_ELEMENT));
        ele.init();
        
        E_PLAYER player[2] = {PLAYER_O, PLAYER_X};
        int j = 0;
        while (STATUS_PLAYING == ele.status)
        {
            int index = play_one_step(ele, player[j]);
            ele.seq[ele.seqNumber++] = index;

            j++;
            j %= 2;
        }
        
        if (STATUS_FAIR == ele.status)
        {
            fair++;
        }
        else
        {
            j++;
            j %= 2;
            wons[j]++;
        }
    }

    printf("A wins %d, B wins %d, fair %d\n", wons[0], wons[1], fair);
    
    unsigned int t2 = GetTickCount();

    printf("time diff %d ms\n",  t2 - t1);
      
}

void test_tick()
{
    unsigned int t1 = GetTickCount();
    unsigned int tt = 0;
    for (int i = 0; i < 1000000; i++)
    {
        tt = GetTickCount();
        if ((tt - t1) > 999)
        {
        }
    }

    
    unsigned int t2 = GetTickCount();

    printf("time tick %d ms\n",  t2 - t1);    
}
void main()
{
    T_BOARD_RECD board = {
            {
             {{{'-', 'X', '-',
               '-', 'X', '-',
               '-', 'X', '-'}},{{'-', 'X', '-',
                                 '-', 'X', '-',
                                 '-', 'X', '-'}},{{'-', 'X', '-',
                                                   '-', 'X', '-',
                                                   '-', 'X', '-'}}},
             {{{'O', 'X', '-',
               '-', 'O', '-',
               '-', 'X', 'O'}},{{'-', 'O', '-',
                                 '-', 'O', '-',
                                 '-', 'O', '-'}},{{'-', 'X', 'O',
                                                   '-', 'O', '-',
                                                   'O', 'X', '-'}}},
             {{{'O', 'X', '-',
               '-', 'O', '-',
               '-', 'X', 'O'}},{{'-', 'O', '-',
                                 '-', 'O', '-',
                                 '-', 'O', '-'}},{{'-', 'X', 'O',
                                                   '-', 'O', '-',
                                                   'O', 'X', '-'}}}
            },
             


       };
    print(board); 


    for (int i = 0; i < 10; i++)
    {
        printf("%d ", getRandom(9));
    }
    printf("\n");

    timerun_ms();

    //test_grid();

    test_status();

    game_play();

    test_tick();
    
    getchar();   
}
