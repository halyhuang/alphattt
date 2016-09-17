package com.zte.alphattt.game;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Board {
	public static final int PlAYER_1_WIN = 1;
	public static final int PLAYER_2_WIN = 2;
	public static final int DRAW = 3;
	public static final int ON_GOING = 4;
	
	/*
	 * 1 2 4 8 16 32 64 128 256
	 */
	private int[] gameState = new int[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 1 };
	private int[] posValue = new int[] { 1, 2, 4, 8, 16, 32, 64, 128, 256 };
	private int[] wins = new int[] { 1 + 2 + 4, 8 + 16 + 32, 64 + 128 + 256, 1 + 8 + 64, 2 + 16 + 128, 4 + 32 + 256,
			1 + 16 + 256, 4 + 16 + 64 };
	private static Map<Integer, Integer> lineIndex2grid = new HashMap<Integer, Integer>();
	static {
		lineIndex2grid.put(0, 2);
		lineIndex2grid.put(1, 4);
		lineIndex2grid.put(2, 6);
		lineIndex2grid.put(3, 9);
		lineIndex2grid.put(4, 11);
		lineIndex2grid.put(5, 13);
		lineIndex2grid.put(6, 16);
		lineIndex2grid.put(7, 18);
		lineIndex2grid.put(8, 20);
	}

	public int[] start() {
		return gameState.clone();
	}

	public int[] next_state(int[] state, int[] move) {
		int[] res = state.clone(); 
		if (is_legal(res, move)) {
			int player = res[22];
			int gridIndex = (move[0] * 3 + move[1]) * 2 - 1 + player;
			int gridValue = posValue[move[2] * 3 + move[3]];
			res[gridIndex] = res[gridIndex] + gridValue;
			res[18] = judgeBigWin(1, res);
			res[19] = judgeBigWin(2, res);
			res[20] = move[2];
			res[21] = move[3];
			res[22] = 3 - player;
			return res;
		}
		return null;
	}

	private int judgeBigWin(int player, int[] state) {
		int res = 0;
		for(int i = 0; i < 9; i++){
			if(isWin(state[i*2 + player - 1])){
				res += this.posValue[i];
			}
		}
		return res;
	}

	public int currentPlayer(int[] state){
		return state[22];
	}
	
	public int[][] legal_moves(int[] state) {
		int bGridX = state[20];
		int bGridY = state[21];
		if (bGridX == -1 || bGridY == -1) {
			return findAllLegal(state);
		}

		int gridIndex = (bGridX * 3 + bGridY) * 2 - 1;
		int player1GirdValue = state[gridIndex + 1];
		int player2GridValue = state[gridIndex + 2];
		if (isWin(player1GirdValue) || isWin(player2GridValue) || isFull(player1GirdValue + player2GridValue)) {
			return findAllLegal(state);
		}

		return findGirdLegalMoves(state[gridIndex + 1], state[gridIndex + 2], bGridX, bGridY).toArray(new int[0][]);
	}

	public int max_moves(){
		return 81;
	}
	
	/**
	 * 
	 * @param state
	 * @return 1|2|3:draw|4:ongoing
	 */
	public int winner(int[] state){
		if(isWin(state[18])){
			return PlAYER_1_WIN;
		}
		if(isWin(state[19])){
			return PLAYER_2_WIN;
		}
		if(isFull(state)){
			return DRAW;
		}
		return ON_GOING;
	}
	
	private boolean isFull(int[] state){
		for(int i = 0; i < 9; i++){
			int total = state[i*2] + state[i*2 + 1];
			if(!isFull(total) && !isWin(state[i*2]) && !isWin(state[i*2 + 1])){
				return false;
			}
		}
		return true;
	}
	
	private boolean isFull(int i) {
		return i == 511;
	}

	private List<int[]> findGirdLegalMoves(int player1value, int player2value, int gridx, int gridy) {
		List<int[]> res = new ArrayList<int[]>();
		int total = player1value + player2value;
		for (int i = 0; i < this.posValue.length; i++) {
			if ((total & posValue[i]) == 0) {
				res.add(new int[] { gridx, gridy, i / 3, i % 3 });
			}
		}
		return res;
	}

	private int[][] findAllLegal(int[] state) {
		List<int[]> res = new ArrayList<int[]>();
		for(int i = 0; i < 9; i++){
			int player1GirdValue = state[i*2];
			int player2GridValue = state[i*2 + 1];
			if (isWin(player1GirdValue) || isWin(player2GridValue) || isFull(player1GirdValue + player2GridValue)) {
				continue;
			}
			List<int[]> tmp = findGirdLegalMoves(player1GirdValue, player2GridValue, i / 3, i % 3);
			res.addAll(tmp);
		}
		return res.toArray(new int[0][]);
	}

	private boolean isWin(int i) {
		for (int win : this.wins) {
			if ((i & win) == win) {
				return true;
			}
		}
		return false;
	}

	private boolean is_legal(int[] state, int[] move) {
		return true;
	}

	public void display(int[] state) {
		int[][] wholeGrid = new int[9][9];
		for (int i = 0; i < 9; i++) {
			int[][] grid = parseGrid(state[2 * i], state[2 * i + 1]);
			copyGrid(wholeGrid, grid, i);
		}

		for (int line = 0; line < wholeGrid.length; line++) {
			if (line % 3 == 0) {
				displayBorder();
			}
			this.displayOneLine(wholeGrid[line]);
		}
		displayBorder();
	}

	private void copyGrid(int[][] wholeGrid, int[][] grid, int i) {
		int offsetX = (i / 3) * 3;
		int offsetY = (i % 3) * 3;
		for (int xi = 0; xi < 3; xi++) {
			for (int yi = 0; yi < 3; yi++) {
				wholeGrid[xi + offsetX][yi + offsetY] = grid[xi][yi];
			}
		}
	}

	private int[][] parseGrid(int player1, int player2) {
		int[][] res = new int[][] { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } };
		for (int i = posValue.length - 1; i >= 0; i--) {
			player1 = matchStep(player1, posValue[i], i, 1, res);
			player2 = matchStep(player2, posValue[i], i, 2, res);
		}
		return res;
	}

	private int matchStep(int player, int value, int pos, int desc, int[][] res) {
		if (player / value > 0) {
			player = player - value;
			res[pos / 3][pos % 3] = desc;
		}
		return player;
	}

	private void displayOneLine(int[] line) {
		byte[] bytes = "|| | | || | | || | | ||".getBytes();
		for (int i = 0; i < line.length; i++) {
			if (line[i] == 1) {
				bytes[lineIndex2grid.get(i)] = 'x';
			} else if (line[i] == 2) {
				bytes[lineIndex2grid.get(i)] = 'o';
			}
		}
		System.out.println(new String(bytes));
	}

	public void displayBorder() {
		System.out.println("||-|-|-||-|-|-||-|-|-||");
	}
	
}
