package com.zte.alphattt.comm;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface CmdHandler {
	boolean isMe(OtpErlangObject otpErlangObject);

	void handle(PlayerClient adapter, OtpErlangObject otpErlangObject);
}
