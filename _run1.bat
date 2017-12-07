/+ 1>nul 2>nul
@chcp 65001
::copy %~dpnx0 tmp___.d
copy %~dpnx0 tmp_%~n0___.d
edub tmp_%~n0___.exe run tmp_%~n0___.d arch=dm32 inc=. ^
def=EnableReal ^
[vibe-d:data] [my-msgpack-d#@my-msgpack-d]
@if %errorlevel% neq 0 (exit /b)
@goto :eof
+/

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

int main()
{
	import std.stdio;
	writeln(`hello!`);
	
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

	TypeInfo type;
	Variant v1 = cast(real) 7777;
	writeln(v1.convertsTo!(double));
	writeln(v1.convertsTo!(real));
	writeln(v1.type);
	writeln(v1.type == typeid(real));
	type = v1.type;
	if (type == typeid(real))
		writeln("[real]");
	else if (type == typeid(double))
		writeln("[double]");
	Variant v2 = cast(double) 7777;
	writeln(v2.convertsTo!(double));
	writeln(v2.convertsTo!(real));
	writeln(v2.type);
	writeln(v2.type == typeid(double));
	type = v2.type;
	if (type == typeid(real))
		writeln("[real]");
	else if (type == typeid(double))
		writeln("[double]");
	else if (type == typeid(Variant[Variant]))
		writeln("<Variant[Variant]>");

	Variant v3 = vmap;
	type = v3.type;
	if (type == typeid(real))
		writeln("[real]");
	else if (type == typeid(double))
		writeln("[double]");
	else if (type == typeid(Variant[Variant]))
		writeln("<Variant[Variant]>");

	Variant[] vary;
	Variant v4 = vary;
	type = v4.type;
	if (type == typeid(real))
		writeln("[real]");
	else if (type == typeid(double))
		writeln("[double]");
	else if (type == typeid(Variant[Variant]))
		writeln("<Variant[Variant]>");
	else if (type == typeid(Variant[]))
		writeln("<Variant[]>");

	alias NVariant = Nullable!Variant;
	NVariant[] nvary;
	Variant v5 = nvary;
	writeln(v5.type == typeid(NVariant[]));
	writeln(NVariant.sizeof);

	//alias Variant = VariantN!(maxSize!(creal, char[], void delegate()));
	//alias MyVariant = VariantN!(maxSize!(real, char[], void delegate()));
	//alias MyVariant = VariantN!(maxSize!(real, char[]));
	alias MyVariant = VariantN!(maxSize!(real, char[]));
	//alias MyVariant = VariantN!(8);
	//alias MyVariant = VariantN!(real.sizeof);
	writeln(MyVariant.sizeof);
	writeln((char[]).sizeof);
	MyVariant v6 = nvary;
	creal cr0;
	v6 = cr0;

	return 0;
}
