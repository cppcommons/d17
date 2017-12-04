import my_common;
import vibe.data.json;

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
import std.datetime.systime;
import std.file;
import std.format;
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
						&& to!long(this.http.headers["x-ratelimit-remaining"]) == 0)
				{
					long rateRemaining;
					SysTime rateResetTime;
					if ("x-ratelimit-remaining" in this.http.headers)
						rateRemaining = to!long(this.http.headers["x-ratelimit-remaining"]);
					long v_rate_reset = 0;
					if ("x-ratelimit-reset" in this.http.headers)
						v_rate_reset = to!long(this.http.headers["x-ratelimit-reset"]);
					rateResetTime = SysTime(unixTimeToStdTime(v_rate_reset));
					writeln(`rate_limit_exceeded error!: rateResetTime=`, rateResetTime);
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
					&& this.http.headers["content-type"] != "application/json; charset=utf-8")
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

	return 0;
}
