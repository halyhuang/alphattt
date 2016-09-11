package com.zte.alphattt.game;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Human implements Player {

	private Board board = new Board();

	@Override
	public void stop() {

	}

	@Override
	public void show() {

	}

	@Override
	public void update(int[] move, int[] gameState) {
		return;
	}

	@Override
	public int[] getMove() {

		BufferedReader bufferedReader = null;
		int[] res = null;
		while (true) {
			try {
				bufferedReader = new BufferedReader(new InputStreamReader(System.in));
				System.out.println("Enter a String :");
				String val = bufferedReader.readLine();
				res = parse(val);
				if (res != null) {
					break;
				}
				System.out.println("Your String is wrong, should be n,n,n,n not:" + val + "\r");
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return res;
	}

	private int[] parse(String val) {
		int[] res = new int[4];
		String[] valueS = val.trim().split(",");
		if (valueS.length != 4) {
			return null;
		}
		try {
			for (int i = 0; i < res.length; i++) {
				res[i] = Integer.parseInt(valueS[i]);
			}
		} catch (Exception e) {
			return null;
		}

		return res;
	}

	@Override
	public void display(int[] move, int[] gameState) {
		board.display(gameState);
	}

	@Override
	public void setThinkMiliSecs(int miliSecs) {
		return;
	}

}
