package com.zte.alphattt.comm.cmd;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.zte.alphattt.comm.CmdHandler;
import com.zte.alphattt.comm.PlayerClient;
import com.zte.alphattt.erlangtools.ErlangUtil;

public class PlayHandler implements CmdHandler {

	@Override
	public boolean isMe(OtpErlangObject o) {
		if (o instanceof OtpErlangAtom) {
			OtpErlangAtom atom = (OtpErlangAtom) o;
			if ("play".equals(atom.atomValue())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void handle(PlayerClient adapter, OtpErlangObject otpErlangObject) {
		int[] move = adapter.player.getMove();
		adapter.sendMsg(
				new OtpErlangTuple(new OtpErlangObject[] { new OtpErlangAtom("play"), ErlangUtil.makeTuple(move) }));
	}

}
