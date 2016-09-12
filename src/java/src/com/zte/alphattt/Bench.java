package com.zte.alphattt;

import java.text.DecimalFormat;
import java.util.Arrays;

import com.zte.alphattt.game.Board;
import com.zte.alphattt.game.Mcts;
import com.zte.alphattt.game.Player;
import com.zte.alphattt.game.RandomPlayer;

public class Bench {
	Board board = new Board();
	Player player1;
	Player player2;
	
	public Bench(Player player1, Player player2) {
		this.player1 = player1;
		this.player2 = player2;
	}
	
	public void run(int games, int thinkMiliSecs){
		player1.setThinkMiliSecs(thinkMiliSecs);
		player2.setThinkMiliSecs(thinkMiliSecs);
		int i = 0;
		int player1Wins = 0;
		int player2Wins = 0;
		while(i++ < games){
			int winner = pkOneGame();
			if(winner == 1){
				player1Wins++;
			}else if(winner == 2){
				player2Wins++;
			}
			
			
			DecimalFormat df = new DecimalFormat("#.00"); 
			System.out.println("Total play " + i + " games, "
					+ this.player1.getClass().getSimpleName() + " wins " + player1Wins 
					+ "(" +  df.format((player1Wins*1.0 / i*1.0) * 100) + "%), "
					+ this.player2.getClass().getSimpleName() + " wins " + player2Wins 
					+ "(" + df.format((player2Wins*1.0 / i*1.0) * 100) + "%).");
		}
	}

	private int pkOneGame() {
		int[] state = board.start();
		int winner = -1;
		do{
			state = pkOneStep(this.player1, state);
			winner = board.winner(state);
			if(isEnd(winner)){
				return winner;
			}
			state = pkOneStep(this.player2, state);
			winner = board.winner(state);
			if(isEnd(winner)){
				return winner;
			}
		}while(true);
	}
	
	private int[] pkOneStep(Player player, int[] state){
		player.update(null, state);
		int[] move = player.getMove();
		state = board.next_state(state, move);
		//board.display(state);
		//System.out.println();
		return state;
	}
	
	private boolean isEnd(int winner){
		return winner == Board.PlAYER_1_WIN ||  winner == Board.PLAYER_2_WIN 
				|| winner == Board.DRAW;
	}
	
	public static void main(String[] args){
		Bench bench = new Bench(new Mcts(), new RandomPlayer());
		bench.run(100, 1000);
	}
	
}
