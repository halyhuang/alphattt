package com.zte.alphattt;

import com.zte.alphattt.comm.PlayerClient;
import com.zte.alphattt.game.Board;
import com.zte.alphattt.game.Mcts;

public class Boot {
	public static void main(String[] args) {
		PlayerClient playerClient = new PlayerClient(new Mcts(), new Board());
		wait(2);
		playerClient.connect("127.0.0.1", 8011);
		playerClient.login("pybot", "password");
		playerClient.enterRoom("pybot", 6);
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
