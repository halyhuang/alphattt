package com.zte.alphattt.game;

public interface Player {
	void stop();
	void show();
	void update(int[] move, int[] gameState);
	int[] getMove();
	void display(int[] move, int[] gameState);
	void setThinkMiliSecs(int miliSecs);
}
