module msgpack_d2;
import my_common;
import vibe.data.bson;
import vibe.data.json;
import msgpack;

import std.array;
import std.conv;
import std.net.curl;
import std.stdio;
import std.string;

import core.sync.rwmutex;
import core.sync.semaphore;
import core.thread;
import core.time;
import std.algorithm;
import std.algorithm.mutation;
import std.algorithm.sorting;
import std.algorithm.setops;
import std.array;
import std.bigint;
import std.conv;
import std.datetime;

//import std.datetime.stopwatch;
import std.datetime.systime;
import std.file;
import std.format;
import std.math;
import std.numeric;
import std.typecons;
import std.variant;

private void exit(int code)
{
	import std.c.stdlib;

	std.c.stdlib.exit(code);
}

string github_get_username()
{
	string username = retrieveString("github", "username");
	if (username !is null)
		return username;
	stdout.write("github username: ");
	stdout.flush();
	string line;
	line = stdin.readln();
	if (line !is null)
		line = line.strip;
	if (line is null || line.empty)
	{
		stderr.writeln("username is required!");
		exit(1);
		return null;
	}
	else
	{
		registerString("github", "username", line);
		return line;
	}
}

string github_get_token()
{
	string token = retrieveString("github", "token");
	if (token !is null)
	{
		writeln(`token=`, token);
		return token;
	}
	stdout.write("github token: ");
	stdout.flush();
	string line;
	line = stdin.readln();
	if (line !is null)
		line = line.strip;
	if (line is null || line.empty)
	{
		stderr.writeln("token is required!");
		exit(1);
		return null;
	}
	else
	{
		registerString("github", "token", line);
		return line;
	}
}

string github_get_password()
{
	shared static string enc_pass = "*";
	string password = decryptString(enc_pass, "github", "password");
	if (password !is null)
		return password;
	stdout.write("github password: ");
	stdout.flush();
	string line;
	line = stdin.readln();
	if (line !is null)
		line = line.strip;
	if (line is null || line.empty)
	{
		stderr.writeln("password is required!");
		exit(1);
		return null;
	}
	else
	{
		encryptString(enc_pass, "github", "password", line);
		return line;
	}
}

class C_GitHubHttp
{
	HTTP.StatusLine statusLine;
	string[string] headers;
	ubyte[] data;
	int get(string url)
	{
		this.headers.clear();
		this.data.length = 0;
		auto http = HTTP(url);
		//http.clearRequestHeaders();
		//string username = github_get_username();
		//string password = github_get_password();
		string token = github_get_token();
		//http.setAuthentication(username, password);
		http.addRequestHeader(`Authorization`, format!`token %s`(token));
		http.onReceiveStatusLine = (in HTTP.StatusLine statusLine) {
			this.statusLine = statusLine;
		};
		http.onReceiveHeader = (in char[] key, in char[] value) {
			this.headers[key] = to!string(value);
		};
		http.onReceive = (ubyte[] bytes) {
			this.data ~= bytes;
			return bytes.length;
		};
		return http.perform(No.throwOnError);
	}

	override string toString() const
	{
		return "???";
	}
}

class C_GitHubApi
{
	C_GitHubHttp http;
	Json jsonValue;
	this()
	{
		this.http = new C_GitHubHttp();
	}

	~this()
	{
		delete this.http;
	}

	int get(string url)
	{
		_loop_a: for (;;)
		{
			this.jsonValue = null;
			int rc = this.http.get(url);
			if (rc != 0)
				return rc;
			writefln("this.http.statusLine.code=%d", this.http.statusLine.code);
			if (this.http.statusLine.code != 200 && this.http.statusLine.code != 403)
			{
				writeln(this.http.headers);
				writeln(cast(string) this.http.data);
				exit(1);
			}
			if (this.http.statusLine.code == 403)
			{
				writeln(cast(string) this.http.data);
				//string data = cast(string) this.http.data;
				//if (data.canFind(`API rate limit exceeded`))
				if (("x-ratelimit-remaining" in this.http.headers)
						&& to!long(
							this.http.headers["x-ratelimit-remaining"]) == 0)
				{
					long rateRemaining;
					SysTime rateResetTime;
					if ("x-ratelimit-remaining" in this.http.headers)
						rateRemaining = to!long(
								this.http
								.headers["x-ratelimit-remaining"]);
					long v_rate_reset = 0;
					if ("x-ratelimit-reset" in this.http.headers)
						v_rate_reset = to!long(
								this.http
								.headers["x-ratelimit-reset"]);
					rateResetTime = SysTime(unixTimeToStdTime(v_rate_reset));
					writeln(`rate_limit_exceeded error!: rateResetTime=`,
							rateResetTime);
					/+
					SysTime currentTime = Clock.currTime();
					writeln(currentTime);
					Duration diff = rateResetTime - currentTime;
					writeln(diff);
					Duration diff2 = diff + dur!`seconds`(60);
					writeln(diff2);
					writeln(`Sleeping for: `, diff2);
					sleepForSeconds(diff2.total!`seconds`);
					+/
					sleepUntil(rateResetTime + dur!`seconds`(60));
					continue _loop_a;
				}
				write("\a");
				return -1;
			}
			if (this.http.statusLine.code != 200)
			{
				write("\a");
				return -1;
			}
			if (this.http.headers["content-type"] != "application/json"
					&& this.http.headers["content-type"]
					!= "application/json; charset=utf-8")
			{
				writeln(`not application/json`);
				writeln(this.http.headers);
				return -1;
			}
			try
			{
				this.jsonValue = parseJsonString(cast(string) this.http.data);
			}
			catch (JSONException ex)
			{
				write("\a");
				writeln(ex);
				return -1;
			}
			break _loop_a;
		}

		return 0;
	}
}

class CommandExitException : Exception
{
	int code;
	this(string msg, int code, string file = __FILE__, size_t line = __LINE__)
	{
		super(msg, file, line);
		this.code = code;
	}
}

void sub()
{
	throw new CommandExitException("test exit", 1234);
}

alias nint = Nullable!(int, int.min);
alias nint2 = Nullable!(int);
pragma(inline) int getDefault(nint x, int d)
{
	return x.isNull ? d : x.get;
}

int main()
{
	SysTime v_curr_time = Clock.currTime();
	scope (exit)
	{
		Duration v_dur = Clock.currTime() - v_curr_time;
		writeln(`v_dur=`, v_dur);
	}
	scope (success)
		writeln("test @", __FILE__, ":", __LINE__, " succeeded.");

	nint i0 = int.min;
	assert(i0.isNull);
	i0 = 1234;
	int i1 = 1234;
	assert(i0 == i1);
	i0.nullify;
	assert(i0.isNull);
	Nullable!(int, int.min) ix = int.min;

	Json[] ary;
	ary ~= Json("X");
	ary ~= Json("Y");
	ary ~= Json("Z");
	Json jsonValue = parseJsonString("{}");
	jsonValue[`e`] = ary;
	writeln(jsonValue.toPrettyString.replace("\t", "  "));
	jsonValue[`z`] = 7777;
	writeln(jsonValue.toPrettyString.replace("\t", "  "));
	//jsonValue[`e`] = ary;
	jsonValue[`d`] = "2222";
	jsonValue[`a`] = 1234;
	jsonValue[`b`] = "6666";
	writeln(jsonValue.toPrettyString.replace("\t", "  "));
	Json[] ary2 = jsonValue[`e`].get!(Json[]);
	ary2[0] = Json("XX");
	//Json d = jsonValue[`d`];
	jsonValue[`d`] = Json("4444");
	//ary2[1].get!string("YY");
	writeln(jsonValue.toPrettyString.replace("\t", "  "));
	foreach (key, value; jsonValue.byKeyValue)
	{
		//writefln("%s: %s", key, value);
		writeln(key);
	}
	/+
	shared static string enc_pass = "*";
	SysTime v_time_0 = Clock.currTime();
	for (int i=0; i<10; i++)
	{
		string password = decryptString(enc_pass, "github", "password");
	}
	Duration v_dur_0 = Clock.currTime() - v_time_0;
	writeln(`v_dur_0=`, v_dur_0);
	+/
	try
	{
		sub();
	}
	catch (CommandExitException ex)
	{
		writeln(ex.code, ` `, ex.message, ` `, ex.file, ` `, ex.line);
	}
	Variant a = i0;
	writeln(a.convertsTo!(int));
	writeln(a.convertsTo!(nint));
	nint x = a.get!nint;
	writeln(x);
	//int y = x.get(int.min);
	//int y = x.get();
	int y = x.isNull ? int.min : x.get;
	writeln(y);

	int p = x.getDefault(-1234);
	writeln(`p=`, p);

	alias nbigint = Nullable!(BigInt);
	nbigint z = nbigint.init;
	writeln(`z=`, z);

	nint aa;
	writeln(`aa=`, aa);

	//CustomFloat!(26, 5) cf;
	//CustomFloat!(58, 5) cf;
	CustomFloat!(80) cf;
	real rf;

	//DoubleDouble ddf = 1234.5;
	string s = "abc漢字";
	wstring ws = to!wstring(s);
	writeln(ws, ws.length);
	dstring ds = to!dstring(ws);
	writeln(ds, ds.length);

	writeln(nint.sizeof);
	writeln(nint2.sizeof);

	nint[2] vec1;
	nint2[2] vec2;

	writeln(vec1.sizeof);
	writeln(vec2.sizeof);

	writeln(Nullable!(wchar).sizeof);

	writefln(`0x%08x`, dchar.max);

	alias ndchar = Nullable!(dchar, cast(dchar) 0xffffffff);
	alias ndchar2 = Nullable!(dchar, cast(dchar)-1);

	assert(cast(dchar) 0xffffffff == cast(dchar)-1);

	bool is_ok_helper(BigInt n)
	{
		string c_str = format("%d", n);
		ulong c_ul;
		try
		{
			c_ul = to!ulong(c_str);
		}
		catch (std.conv.ConvOverflowException ex)
		{
			return false;
		}
		double c_real = to!double(c_ul);
		return to!ulong(c_real) == c_ul;
	}
	// range_max is ok: 9007199254740992 200000_00000000

	/+
	bool is_ok_helper(BigInt n)
	{
		string c_str = format("%d", n);
		ulong c_ul;
		try
		{
			c_ul = to!ulong(c_str);
		}
		catch (std.conv.ConvOverflowException ex)
		{
			return false;
		}
		real c_real = to!real(c_ul);
		return to!ulong(c_real) == c_ul;
	}
	//range_max is ok: 18446744073709551615 FFFFFFFF_FFFFFFFF
	+/

	bool is_ok(BigInt n)
	{
		if (!is_ok_helper(n))
			return false;
		BigInt p = n;
		for (size_t i = 1; i <= 10; i++)
		{
			p = n;
			if ((p - i) <= 0)
				break;
			writeln(`i=`, i, ` p=`, p);
			writeln(i, ` `, p - i);
			if (!is_ok_helper(p - i))
				return false;
		}
		writeln();
		return true;
	}

	SysTime v_start = Clock.currTime();
	BigInt range_min = 0;
	BigInt range_max = 1;
	//range_max = pow(range_max, 128);
	range_max <<= 128;
	for (;;)
	{
		writeln(`[RANGE] `, range_min, `=`, is_ok(range_min), `==>`,
				range_max, `=`, is_ok(range_max));
		if (is_ok(range_max))
		{
			writeln(`range_max is ok: `, range_max, ` `, range_max.toHex);
			break;
		}
		BigInt range_mid = (range_min + range_max) / 2;
		writeln(`range_mid=`, range_mid);
		if (range_mid == range_min)
		{
			writeln(`1`);
			range_max -= 1;
		}
		else if (range_mid == range_max)
		{
			writeln(`2`);
			range_max -= 1;
		}
		else if (is_ok(range_mid))
		{
			writeln(`3`);
			range_min = range_mid;
		}
		else
		{
			writeln(`4`);
			range_max = range_mid;
		}
		writeln(`5`);
	}

	//cent my_cent;
	writeln(Nullable!(double).sizeof);
	writeln(Nullable!(real).sizeof);
	writeln(BigInt.sizeof);
	writeln(Nullable!(BigInt).sizeof);
	writeln(Nullable!(Variant).sizeof);

	class A
	{
		int f_a;
	}

	alias myvariant1 = Algebraic!(int, long, double, real, string, A,
			int[int], BigInt, ubyte[]);
	alias myvariant2 = Algebraic!(int, long, double /*, real*/ );
	writeln(myvariant1.sizeof);
	writeln(myvariant2.sizeof);

	for (Variant lc = BigInt("0"); lc < BigInt(5); lc += BigInt(1))
	{
		writeln(lc);
		lc.get!BigInt += 1;
	}

	void myappend(ref Variant v, string str)
	{
		if (!v.hasValue())
		{
			//v = `<null>`;
			v = ``;
		}
		else if (!v.convertsTo!(string))
		{
			v = v.toString();
		}
		v ~= str;
		/+
		v.tryVisit!((string x) { v ~= str; }, () {
			if (v.hasValue)
				v = v.toString;
			else
				v = `<null>`;
			v ~= str;
		})();
		+/
	}

	Variant vstr = "abc";
	//vstr ~= "xyz";
	myappend(vstr, "xyz");
	writeln(vstr);
	Variant vstr2;
	myappend(vstr2, "xyz");
	writeln(vstr2);
	Variant vstr3 = cast(string) null;
	writeln(vstr3.convertsTo!(string));

	//Bson b3 = S("foo", long.max, long.max).serializeToBson();

	//auto s1 = S("foo", 1234, 1234);
	//ubyte[] s1_data = toBsonData(s1);
	//auto s2 = fromBsonData!S(s1_data);
	//writeln(`s2=`, s2);
	Bson b2 = Bson.emptyObject;
	b2["field1"] = "foo";
	b2["field2"] = 42;
	b2["field3"] = true;
	b2["field4"] = long.max;
	b2["field5"] = cast(real) long.max;

	//Bson b3 = S("foo", 1234, 1234).serializeToBson();
	writeln(long.max);
	writeln(ulong.max);
	//writeln(b3);
	immutable(ubyte)[] b3_data = b2.data;
	writeln(`A`);
	auto b4 = Bson(Bson.Type.object, b3_data);
	writeln(`B`);
	writeln(`b4=`, b4);
	real zzz = b4["field5"].get!double;
	writeln(b4["field4"].type);
	writeln(b4["field5"].type);

	struct S
	{
		int x;
		float y;
		string z;
		real r;
		real[string] tbl;
	}

	//S input = S(10, 25.5, "message", long.max);
	S input = S(10, 25.5, "message", ulong.max);
	input.tbl["xyz"] = ulong.max;

	// serialize data
	ubyte[] inData = pack(input);

	// write data to a file
	std.file.write("file.dat", inData);

	// read data from a file
	ubyte[] outData = cast(ubyte[]) read("file.dat");

	// unserialize the data
	S target = outData.unpack!S();

	// verify data is the same
	assert(target.x == input.x);
	assert(target.y == input.y);
	assert(target.z == input.z);
	writeln(target.r == input.r);

	writeln(input);
	writeln(target);

	//auto zzz2 = std.conv.to!long(target.r);
	auto zzz2 = std.conv.to!ulong(target.r);
	writeln(zzz2);

	class Base
	{
		string str = "foo";
	}

	class C : Base
	{
		int num;
		this(int n)
		{
			num = n;
		}
	}

	registerClass!(C);

	struct Trans
	{
		Base[string] tbl;
	}

	{
		Packer pk;
		Base c = new C(1000);
		pk.pack(c);

		Base c2 = new C(5);
		unpack(pk.stream.data, c2);
		assert(1000 == (cast(C) c2).num);
		writeln(c2, ` `, (cast(C) c2).num);
	}

	{
		import std.datetime.stopwatch;

		StopWatch sw;
		sw.start();
		Packer pk;
		pk.beginArray(3).pack(true, cast(real) 123);
		pk.pack("ABC");
		writeln(pk);
		auto unpacker = StreamingUnpacker(pk.stream.data);
		foreach (unpacked; unpacker)
		{
			if (unpacked.type == Value.Type.array)
			{
				foreach (obj; unpacked)
				{
					switch (obj.type)
					{
					case Value.Type.boolean:
						writeln(`[boolean]`, obj.as!(bool));
						break;
					case Value.Type.signed:
						writeln(`[signed]`, obj.as!(long));
						break;
					case Value.Type.unsigned:
						writeln(`[unsigned]`, obj.as!(ulong));
						break;
					case Value.Type.floating:
						writeln(`[floating]`, obj.as!(real));
						break;
					case Value.Type.raw:
						writeln(`[raw]`, obj.as!(string));
						break;
					default:
						writeln(obj.type);
						throw new Exception("Unknown type");
					}
				}
			}
			else
			{
				/+
				if (unpacked.type == Value.Type.boolean)
					writeln(unpacked.as!(bool));
				else
					writeln("Message: ", unpacked.as!(string));
				+/
			}
		}
		writeln(sw.peek());
	}

	{
		import std.datetime.stopwatch;

		StopWatch sw;
		sw.start();
		Packer pk;
		pk.beginMap(2);
		pk.pack("A", 123);
		pk.pack("B", cast(real) 123);
		writeln(pk);
		auto unpacker = StreamingUnpacker(pk.stream.data);
		foreach (unpacked; unpacker)
		{
			if (unpacked.type == Value.Type.map)
			{
				writeln(`[map]`);
				auto mp = unpacked.via.map;
				auto keys = mp.keys;
				writeln(keys);
				writeln(keys.length);
				foreach (key; keys)
				{
					writeln(key.as!(string));
					auto val = mp[key];
					writeln(val);
				}
				/+
				foreach (obj; unpacked)
				{
					switch (obj.type)
					{
					case Value.Type.boolean:
						writeln(`[boolean]`, obj.as!(bool));
						break;
					case Value.Type.signed:
						writeln(`[signed]`, obj.as!(long));
						break;
					case Value.Type.unsigned:
						writeln(`[unsigned]`, obj.as!(ulong));
						break;
					case Value.Type.floating:
						writeln(`[floating]`, obj.as!(real));
						break;
					case Value.Type.raw:
						writeln(`[raw]`, obj.as!(string));
						break;
					default:
						writeln(obj.type);
						throw new Exception("Unknown type");
					}
				}
				+/
			}
			else
			{
				/+
				if (unpacked.type == Value.Type.boolean)
					writeln(unpacked.as!(bool));
				else
					writeln("Message: ", unpacked.as!(string));
				+/
			}
		}
		writeln(sw.peek());
	}

	Variant[Variant] vmap;
	vmap[Variant(1234)] = 5678;
	vmap[Variant("abc")] = "tttt";
	writeln(vmap);
	writeln(vmap[Variant(1234)]);
	writeln(vmap[Variant("abc")]);

	Variant v1 = cast(real)7777;
	writeln(v1.convertsTo!(double));
	writeln(v1.convertsTo!(real));
	writeln(v1.type);
	writeln(v1.type == typeid(real));

	Variant v2 = cast(double)7777;
	writeln(v2.convertsTo!(double));
	writeln(v2.convertsTo!(real));
	writeln(v2.type);
	writeln(v2.type == typeid(double));

	return 0;
}
