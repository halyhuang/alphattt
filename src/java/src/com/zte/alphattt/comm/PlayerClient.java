package com.zte.alphattt.comm;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.zte.alphattt.comm.cmd.EchoHandler;
import com.zte.alphattt.comm.cmd.PlayHandler;
import com.zte.alphattt.comm.cmd.UpdateHandler;
import com.zte.alphattt.game.Board;
import com.zte.alphattt.game.Player;

import static com.zte.alphattt.erlangtools.ErlangUtil.*;

public class PlayerClient {
	private static final String JAVA_NODE_NAME = "java_ttt_node@127.0.0.1";
	private static final String PID_FOR_MALLBOX = "java_ttt";
	private static final String ERLANG_BRIDGE_NODE = "bridge@127.0.0.1";
	private static final String ERLANG_BRIDGE_PID = "java_client";
	
	public Player player;
	protected Board board;
	private OtpMbox otpMbox;

	private static List<CmdHandler> handlers = new ArrayList<CmdHandler>();
	static {
		handlers.add(new EchoHandler());
		handlers.add(new UpdateHandler());
		handlers.add(new PlayHandler());
	}

	public PlayerClient(Player player, Board board, String cookie) {
		this.player = player;
		this.board = board;
		this.otpMbox = makeConnection(cookie);
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				while (true) {
					try {
						OtpErlangObject o = PlayerClient.this.otpMbox.receive();
						log("Received:" + o);
						dispatch(o);
					} catch (Exception e) {
						e.printStackTrace();
					}

				}
			}
		});
		thread.start();
	}

	protected void dispatch(OtpErlangObject object) {
		for (CmdHandler cmdHandler : handlers) {
			if (cmdHandler.isMe(object)) {
				cmdHandler.handle(this, object);
			}
		}
	}

	private OtpMbox makeConnection(String cookie) {
		OtpMbox otpMbox = null;
		try {
			OtpNode node = new OtpNode(JAVA_NODE_NAME, cookie);
			otpMbox = node.createMbox(PID_FOR_MALLBOX);
			node.registerName(PID_FOR_MALLBOX, otpMbox);
			if (node.ping(ERLANG_BRIDGE_NODE, 2000)) {
				System.out.println("remote is up");
			} else {
				System.out.println("remote is down");
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return otpMbox;
	}

	public void enterRoom(String nickName, int roomId) {
		sendMsg(new OtpErlangTuple(
				new OtpErlangObject[] { new OtpErlangAtom("enter_room"), new OtpErlangString(nickName), new OtpErlangInt(roomId) }));
	}
	
	public void login(String nickName){
		sendMsg(tuple(atom("login"), string(nickName), string("")));
	}

	public void LeaveRoom(String nickName) {
		sendMsg(new OtpErlangTuple(
				new OtpErlangObject[] { new OtpErlangAtom("leave_room"), new OtpErlangString(nickName) }));
	}

	public void stop() {
		this.player.stop();
	}

	public void show() {
		this.player.show();
	}

	public void sendMsg(OtpErlangObject o) {
		try {
			log("Write:" + o);
			this.otpMbox.send(ERLANG_BRIDGE_PID, ERLANG_BRIDGE_NODE, o);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void log(String log) {
		System.out.println(log);
	}

}
