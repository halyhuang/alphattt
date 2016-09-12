package com.zte.alphattt.comm.cmd;

import static com.zte.alphattt.erlangtools.ErlangUtil.getEleOfTuple;
import static com.zte.alphattt.erlangtools.ErlangUtil.getFirstAtomOfTuple;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.zte.alphattt.comm.CmdHandler;
import com.zte.alphattt.comm.PlayerClient;
import com.zte.alphattt.erlangtools.ErlangUtil;

public class UpdateHandler implements CmdHandler {

	@Override
	public boolean isMe(OtpErlangObject otpErlangObject) {
		OtpErlangAtom atom = getFirstAtomOfTuple(otpErlangObject);
		if (atom == null) {
			return false;
		}
		if ("update".equals(atom.atomValue())) {
			return true;
		}
		return false;
	}

	@Override
	public void handle(PlayerClient adapter, OtpErlangObject otpErlangObject) {
		OtpErlangObject move = getEleOfTuple(otpErlangObject, 1, otpErlangObject);
		OtpErlangObject gameState = getEleOfTuple(otpErlangObject, 2, otpErlangObject);
		int[] intMove = move instanceof OtpErlangAtom ? null : ErlangUtil.parseIntTuple(move);
		int[] intGameState = ErlangUtil.parseIntTuple(gameState);
		
		adapter.player.update(intMove, intGameState);
		adapter.player.display(intMove, intGameState);
	}
	
}
