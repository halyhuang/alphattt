package com.zte.alphattt;

import com.zte.alphattt.comm.PlayerClient;
import com.zte.alphattt.game.Board;
import com.zte.alphattt.game.Human;
import com.zte.alphattt.game.Mcts;

public class Boot {

	public static void main(String[] args) {
		PlayerClient playerClient = new PlayerClient(new Mcts(), new Board(), "hello");
		wait(2);
		playerClient.enterRoom("dairy");
		wait(60 * 60);
	}

	@SuppressWarnings("static-access")
	private static void wait(int seconds) {
		try {
			Thread.currentThread().sleep(seconds * 1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}
