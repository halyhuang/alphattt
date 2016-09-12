package com.zte.alphattt.game;

import java.util.Random;

public class RandomPlayer implements Player {

	private Board board = new Board();
	private GameState lastState = null;
	private Random random = new Random();

	@Override
	public void stop() {
		return;
	}

	@Override
	public void show() {
		return;
	}

	@Override
	public void update(int[] move, int[] gameState) {
		lastState = new GameState(gameState);
	}

	@Override
	public int[] getMove() {
		int[][] legalMoves = board.legal_moves(this.lastState.state);
		return legalMoves[random.nextInt(legalMoves.length)];
	}

	@Override
	public void display(int[] move, int[] gameState) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setThinkMiliSecs(int miliSecs) {
		// TODO Auto-generated method stub
		
	}
}

