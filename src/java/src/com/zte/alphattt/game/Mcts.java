package com.zte.alphattt.game;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class Mcts implements Player {

	private Board board = new Board();
	private GameState lastState = null;
	Map<GameState, PlayStats> cache = new HashMap<GameState, PlayStats>();
	private int runTime = 1000; // miliSeconds
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
		if (legalMoves.length == 0) {
			System.out.println("illegal move");
		}

		if (legalMoves.length == 1) {
			return legalMoves[0];
		}

		int[][] legalStates = new int[legalMoves.length][];
		for (int i = 0; i < legalMoves.length; i++) {
			legalStates[i] = board.next_state(this.lastState.state, legalMoves[i]);
		}

		run_simulation(legalStates);
		int bestStateIndex = chooseBest(legalMoves, legalStates);
		int[] move = legalMoves[bestStateIndex];
		System.out.println("Choosed move:" + Arrays.toString(move));
		return move;
	}

	private int chooseBest(int[][] legalMoves, int[][] legalStates) {
		double[] winRates = new double[legalStates.length];
		for (int i = 0; i < winRates.length; i++) {
			PlayStats playStats = this.cache.get(new GameState(legalStates[i]));
			winRates[i] = (playStats == null ? 0 : playStats.winRate());

			log(legalMoves, winRates, i, playStats);
		}

		double maxRate = max(winRates);
		return getIndex(maxRate, winRates);
	}

	private void log(int[][] legalMoves, double[] winRates, int i, PlayStats playStats) {
		DecimalFormat df = new DecimalFormat("#.00");
		System.out.println(Arrays.toString(legalMoves[i]) + ":" + (playStats == null ? 0 : playStats.wins) + "/"
				+ (playStats == null ? 0 : playStats.plays) + ",(" + df.format(winRates[i] * 100) + "%).");
	}

	private int getIndex(double maxRate, double[] winRates) {
		int res = 0;
		for (int i = 0; i < winRates.length; i++) {
			if (winRates[i] == maxRate) {
				return i;
			}
		}
		return res;
	}

	private double max(double[] winRates) {
		double max = winRates[0];
		for (double winRate : winRates) {
			if (winRate > max) {
				max = winRate;
			}
		}
		return max;
	}

	private void run_simulation(int[][] legalStates) {
		long startTime = System.currentTimeMillis();
		int RandomGamePlay = 0;
		while (System.currentTimeMillis() - startTime < this.runTime) {
			randomGame(legalStates);
			RandomGamePlay++;
		}
		System.out.println("Played [" + RandomGamePlay + "] games.");
	}

	private void randomGame(int[][] legalStates) {
		int[] state = choose(legalStates);
		int winner = board.winner(state);
		switch (winner) {
		case Board.PlAYER_1_WIN:
			propagate_back(state, Board.PlAYER_1_WIN);
			break;
		case Board.PLAYER_2_WIN:
			propagate_back(state, Board.PLAYER_2_WIN);
			break;
		case Board.DRAW:
			propagate_back(state, Board.DRAW);
			break;
		case Board.ON_GOING:
			int res = randomGame2End(state);
			propagate_back(state, res);
			break;
		default:
			System.out.println("Meet a impossible winner:[" + winner + "].");
		}
	}

	private int randomGame2End(int[] state) {
		int winner = -1;
		do {
			state = moveOnce(state);
			winner = this.board.winner(state);
		} while (winner == Board.ON_GOING);
		return winner;
	}

	private int[] moveOnce(int[] state) {
		int[][] legalMoves = this.board.legal_moves(state);
		int[] move = choose(legalMoves);
		return board.next_state(state, move);
	}

	private void propagate_back(int[] state, int winner) {
		GameState gameState = new GameState(state);
		if (!this.cache.containsKey(gameState)) {
			PlayStats playStats = new PlayStats();
			playStats.plays++;
			playStats.wins = ((3-board.currentPlayer(state)) == winner ? (playStats.wins + 1) : playStats.wins);
			this.cache.put(gameState, playStats);
		} else {
			PlayStats playStats = this.cache.get(gameState);
			playStats.wins = ((3-board.currentPlayer(state)) == winner ? (playStats.wins + 1) : playStats.wins);
			playStats.plays++;
		}
	}

	private int[] choose(int[][] list) {
		int length = list.length;
		return list[random.nextInt(length)];
	}

	@Override
	public void display(int[] move, int[] gameState) {
		board.display(gameState);
	}

	@Override
	public void setThinkMiliSecs(int miliSecs) {
		this.runTime = miliSecs;
	}

}

class GameState {
	int[] state;

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(state);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		GameState other = (GameState) obj;
		if (!Arrays.equals(state, other.state))
			return false;
		return true;
	}

	public GameState(int[] state) {
		this.state = state;
	}
}

class PlayStats {
	int plays;
	int wins;

	double winRate() {
		return wins * 1.0 / plays * 1.0;
	}
}
