package com.zte.alphattt.erlangtools;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangUtil {
	public static OtpErlangAtom erlangAtom(String name) {
		return new OtpErlangAtom(name);
	}

	public static OtpErlangTuple erlangTuple(OtpErlangObject... objs) {
		return new OtpErlangTuple(objs);
	}

	public static OtpErlangString erlangString(String name) {
		return new OtpErlangString(name);
	}
	
	public static OtpErlangInt erlangInt(int value){
		return new OtpErlangInt(value);
	}

	@SuppressWarnings("unchecked")
	public static <T> T getEleOfTuple(OtpErlangObject otpErlangObject, int index, T t) {
		try {
			if (otpErlangObject instanceof OtpErlangTuple) {
				OtpErlangTuple echoTuple = (OtpErlangTuple) otpErlangObject;
				OtpErlangObject echoObject = echoTuple.elementAt(index);
				return (T) echoObject;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public static OtpErlangAtom getFirstAtomOfTuple(OtpErlangObject otpErlangObject) {
		return getEleOfTuple(otpErlangObject, 0, new OtpErlangAtom("instance"));
	}

	public static OtpErlangTuple makeTuple(int[] intEles) {
		OtpErlangLong[] otpErlangInts = new OtpErlangLong[intEles.length];
		for (int i = 0; i < intEles.length; i++) {
			otpErlangInts[i] = new OtpErlangLong(intEles[i]);
		}
		OtpErlangTuple otpErlangTuple = new OtpErlangTuple(otpErlangInts);
		return otpErlangTuple;
	}

	public static int[] parseIntTuple(OtpErlangObject OtpErlangTuple) {
		OtpErlangTuple tuple = (OtpErlangTuple) OtpErlangTuple;
		OtpErlangObject[] eles = tuple.elements();
		int[] res = new int[eles.length];
		for (int i = 0; i < eles.length; i++) {
			try {
				if (eles[i] instanceof OtpErlangLong) {
					res[i] = ((OtpErlangLong) eles[i]).intValue();
				} else {
					res[i] = -1;
				}

			} catch (OtpErlangRangeException e) {
				e.printStackTrace();
			}
		}
		return res;
	}

}
