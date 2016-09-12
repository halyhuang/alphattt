package com.zte.alphattt;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

import com.zte.alphattt.game.Board;

public class BoradTest {

	private Board board = new Board();

	@Test
	public void testHuman() {
		int[] gameState = new int[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 1 };
		assertArrayEquals(gameState, board.start());
		
		gameState = moveAndAssert(gameState, new int[] { 0, 2, 1, 2 },
				new int[] { 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2 });
		gameState = moveAndAssert(gameState, new int[] { 1, 2, 1, 1 },
				new int[] { 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1 });
		gameState = moveAndAssert(gameState, new int[] { 1, 1, 1, 1 },
				new int[] { 0, 0, 0, 0, 32, 0, 0, 0, 16, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2 });
		gameState = moveAndAssert(gameState, new int[] { 1, 1, 0, 0 },
				new int[] { 0, 0, 0, 0, 32, 0, 0, 0, 16, 1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 });
		gameState = moveAndAssert(gameState, new int[] { 0, 0, 1, 1 },
				new int[] { 16, 0, 0, 0, 32, 0, 0, 0, 16, 1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2 });
		gameState = moveAndAssert(gameState, new int[] { 1, 1, 0, 1 },
				new int[] { 16, 0, 0, 0, 32, 0, 0, 0, 16, 3, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1 });
		
		sleep(1);
	}

	private int[] moveAndAssert(int[] gameState, int[] move, int[] expectedGameState) {
		gameState = board.next_state(gameState, move);
		assertArrayEquals(expectedGameState, gameState);
		return gameState;
	}

	private void disPlay(int[] state) {
		board.display(state);
	}

	@SuppressWarnings("static-access")
	private void sleep(int seconds) {
		try {
			Thread.currentThread().sleep(seconds * 1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	void printMove(int[][] moves){
		System.out.print("Moves:[");
		for(int[] move : moves){
			System.out.print(Arrays.toString(move));
		}
		System.out.println("]");
	}
}
