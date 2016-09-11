package com.zte.alphattt.comm.cmd;

import static com.zte.alphattt.erlangtools.ErlangUtil.getEleOfTuple;
import static com.zte.alphattt.erlangtools.ErlangUtil.getFirstAtomOfTuple;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.zte.alphattt.comm.CmdHandler;
import com.zte.alphattt.comm.PlayerClient;

public class EchoHandler implements CmdHandler {

	@Override
	public boolean isMe(OtpErlangObject otpErlangObject) {
		OtpErlangAtom atom = getFirstAtomOfTuple(otpErlangObject);
		if (atom == null) {
			return false;
		}
		if ("echo".equals(atom.atomValue()) || "notify".equals(atom.atomValue())) {
			return true;
		}
		return false;
	}

	@Override
	public void handle(PlayerClient adapter, OtpErlangObject otpErlangObject) {
		OtpErlangObject object = getEleOfTuple(otpErlangObject, 1, otpErlangObject);
		System.out.println("echo or notify: " + object.toString());
	}

}
