module var2mod;

import std.stdio;

import std.array;
import std.conv;
import std.format;
import std.traits;
import std.variant;

version (all)
{
    public abstract class os_value
    {
        bool getBoolean()
        {
            return false;
        }

        long getIntegral()
        {
            return 0;
        }

        real getFloating()
        {
            return 0;
        }

        string getString();
        var2[]* getVector()
        {
            return null;
        }

        var2[string]* getDictionary()
        {
            return null;
        }

        os_callable getCallable()
        {
            return null;
        }
    }

    public class os_null_value : os_value
    {
        package static const os_null_value _singleton = new os_null_value;
        package this()
        {
        }
        public override string toString()
        {
            return `null`;
        }
        override string getString()
        {
            return `null`;
        }
    }

    public class os_bool_value : os_value
    {
        private bool _data;
        package this(bool data)
        {
            this._data = data;
        }

        public override string toString()
        {
            return this.getString;
        }

        override bool getBoolean()
        {
            return this._data;
        }

        override long getIntegral()
        {
            return to!long(this._data);
        }

        override real getFloating()
        {
            return to!real(this._data);
        }

        override string getString()
        {
            return this._data ? "true" : "false";
        }
    }

    public class os_long_value : os_value
    {
        private long _data;
        package this(long data)
        {
            this._data = data;
        }

        public override string toString()
        {
            return this.getString;
        }

        override bool getBoolean()
        {
            return (this._data != 0);
        }

        override long getIntegral()
        {
            return this._data;
        }

        override real getFloating()
        {
            return to!real(this._data);
        }

        override string getString()
        {
            return to!string(this._data);
        }
    }

    public class os_real_value : os_value
    {
        private real _data;
        package this(real data)
        {
            this._data = data;
        }

        public override string toString()
        {
            return this.getString;
        }

        override bool getBoolean()
        {
            return (this._data != 0);
        }

        override long getIntegral()
        {
            return to!long(this._data);
        }

        override real getFloating()
        {
            return this._data;
        }

        override string getString()
        {
            return to!string(this._data);
        }
    }

    public class os_string_value : os_value
    {
        private string _data;
        package this(string data)
        {
            this._data = data;
        }

        public override string toString()
        {
            return this.getString;
        }

        override bool getBoolean()
        {
            return !this._data.empty;
        }

        override long getIntegral()
        {
            return to!long(this._data);
        }

        override real getFloating()
        {
            return to!real(this._data);
        }

        override string getString()
        {
            return this._data;
        }
    }

    alias var2 delegate(var2[]) os_callable;
    public class os_func_value : os_value
    {
        private os_callable _data;
        package this(os_callable data)
        {
            this._data = data;
        }

        public override string toString()
        {
            return this.getString;
        }

        override string getString()
        {
            return `<function>`;
        }

        override os_callable getCallable()
        {
            return this._data;
        }
    }

    package class Vector : os_value
    {
        var2[] _data;
        public override string toString()
        {
            return to!string(this._data);
        }
        /+
    package this(XXX)
    {
    }
    +/
        override string getString()
        {
            return to!string(this._data);
        }

        override var2[]* getVector()
        {
            return &_data;
        }
    }

    package class Dictionary : os_value
    {
        private var2[string] _data;
        public override string toString()
        {
            return to!string(this._data);
        }
        /+
    package this(XXX)
    {
    }
    +/
        override string getString()
        {
            return to!string(this._data);
        }

        override var2[string]* getDictionary()
        {
            return &_data;
        }
    }

}

public struct var2
{
    public enum Type
    {
        Null,
        Object,
        Array,
        Integral,
        Floating,
        String,
        Function,
        Boolean
    }

    private Type _type;
    private os_value _value;
    public Type payloadType()
    {
        return _type;
    }

    public this(T)(T t)
    {
        static if (is(T == var2))
            this = t;
        else
            this.opAssign(t);
    }

    public var2 apply(var2[] args)
    {
        if (this.payloadType() == Type.Function)
        {
            os_callable func = this._value.getCallable;
            return func(args);
        }
        return var2(null);
    }

    public var2 call(T...)(T t)
    {
        var2[] args;
        foreach (a; t)
        {
            args ~= var2(a);
        }
        return this.apply(args);
    }

    public var2 opCall(T...)(T t)
    {
        return this.call(t);
    }

    public T get(T)() if (!is(T == void))
    {
        static if (is(T == var2))
        {
            return this;
        }
        else static if (__traits(compiles, T(this)))
        {
            return T(this);
        }
        else static if (__traits(compiles, new T(this)))
        {
            return new T(this);
        }
        else
            final switch (payloadType)
        {
        case Type.Null:
        case Type.Boolean:
        case Type.Floating:
        case Type.Integral:
        case Type.String:
        case Type.Object:
        case Type.Function:
            os_value value = this._value;
            if (payloadType == Type.Null) value = cast(os_value)os_null_value._singleton;
            static if (is(T == bool))
                return value.getBoolean();
            else static if (isFloatingPoint!T)
                return to!T(value.getFloating());
            else static if (isIntegral!T)
                return to!T(value.getIntegral());
            else static if (isSomeString!T)
                return value.getString();
            else
                return T.init;
            /+
        case Type.String:
            static if (__traits(compiles, to!T(this._value.getString)))
            {
                try
                {
                    return to!T(this._value.getString);
                }
                catch (Exception e)
                {
                    return T.init;
                }
            }
            else
                return T.init;
        +/
        case Type.Array:
            ////Vector vec = this._payload.get!Vector;
            static if (isSomeString!T)
            {
                return this._value.getString;
            }
            else static if (isArray!T)
            {
                T ret;
                static if (is(ElementType!T == void))
                {
                }
                else
                {
                    var2[]* vec = this._value.getVector();
                    if (vec)
                    {
                        alias ElemType = ElementType!T;
                        foreach (item; (*vec))
                            ret ~= item.get!(ElemType);
                    }
                }
                return ret;
            }
            else
                return T.init;
        }
    }

    public var2 opAssign(T)(T t) if (!is(T == var2))
    {
        static if (isFloatingPoint!T)
        {
            this._type = Type.Floating;
            this._value = new os_real_value(t);
        }
        else static if (isIntegral!T)
        {
            this._type = Type.Integral;
            this._value = new os_long_value(t);
        }
        else static if (isCallable!T)
        {
            this._type = Type.Function;
            os_callable func = delegate var2(var2[] args) {
                var2 ret;
                ParameterTypeTuple!T fargs;
                foreach (idx, a; fargs)
                {
                    if (idx == args.length)
                        break;
                    cast(Unqual!(typeof(a))) fargs[idx] = args[idx].get!(typeof(a));
                }
                static if (is(ReturnType!t == void))
                {
                    t(fargs);
                }
                else
                {
                    ret = t(fargs);
                }
                return ret;
            };
            this._value = new os_func_value(func);
        }
        else static if (isSomeString!T)
        {
            this._type = Type.String;
            this._value = new os_string_value(t);
        }
        else static if (is(T : var2[string]))
        {
            this._type = Type.Object;
            Dictionary value = new Dictionary;
            var2[string]* dict = value.getDictionary();
            foreach (k, v; cast(var2[string]) t)
            {
                (*dict)[k] = v;
            }
            ////this._payload = dict; /**/
            this._value = value;
        }
        else static if (isArray!T)
        {
            this._type = Type.Array;
            Vector value = new Vector;
            var2[]* vec = value.getVector();
            (*vec).length = t.length;
            writeln(`t.length=`, t.length);
            static if (!is(T == void[]))
                foreach (i, item; t)
                    (*vec)[i] = var2(item);
            this._value = value;
        }
        else static if (is(T == bool))
        {
            this._type = Type.Boolean;
            this._value = new os_bool_value(t);
        }
        else
            static assert(0, "unsupported type");

        return this;
    }

    // obj.prop;
    public @property ref var2 opDispatch(string name, string file = __FILE__, size_t line = __LINE__)()
    {
        return this[name];
    }

    // obj.prop = X;
    public @property ref var2 opDispatch(string name, string file = __FILE__,
            size_t line = __LINE__, T)(T r)
    {
        return this.opIndexAssign!T(r, name);
    }

    // P.T.
    public ref var2 opIndex(string name, string file = __FILE__, size_t line = __LINE__)
    {
        if (name == "length" && this.payloadType() == Type.String)
        {
            var2* tmp = new var2;
            *tmp = this._value.getString.length;
            return *tmp;
        }
        if (name == "length" && this.payloadType() == Type.Array)
        {
            var2[]* vec = this._value.getVector;
            var2* tmp = new var2;
            *tmp = (*vec).length;
            return *tmp;
        }
        if (this.payloadType() == Type.Object)
        {
            var2[string]* dict = this._value.getDictionary();
            var2* found = name in (*dict);
            if (found)
                return (*found);
        }
        var2* tmp = new var2;
        return *tmp;
    }

    // P.T.
    public ref var2 opIndexAssign(T)(T t, string name, string file = __FILE__, size_t line = __LINE__)
    {
        if (name.length && name[0] >= '0' && name[0] <= '9')
            return opIndexAssign(t, to!size_t(name), file, line);
        if (this._type != Type.Object)
        {
            this._type = Type.Object;
            //this._payload = new Dictionary;
            this._value = new Dictionary;
        }
        //Dictionary dict = this._payload.get!Dictionary;
        var2[string]* dict = this._value.getDictionary();
        (*dict)[name] = var2(t);
        return (*dict)[name];
    }

    // N.T.
    public ref var2 opIndex(size_t idx, string file = __FILE__, size_t line = __LINE__)
    {
        if (_type == Type.Array)
        {
            var2[]* vec = this._value.getVector;
            if (idx < (*vec).length)
                return (*vec)[idx];
        }
        var2* n = new var2;
        return *n;
    }

    // N.T.
    public ref var2 opIndexAssign(T)(T t, size_t idx, string file = __FILE__, size_t line = __LINE__)
    {
        if (this._type == Type.Array)
        {
            //Vector vec = this._payload.get!Vector;
            var2[]* vec = this._value.getVector();
            if (idx >= (*vec).length)
                (*vec).length = idx + 1;
            (*vec)[idx] = t;
            return (*vec)[idx];
        }
        var2* n = new var2;
        return *n;
    }

    public string toString()
    {
        switch (this._type)
        {
        case Type.Null:
            return `Null`;
        case Type.Array:
            return `Array` ~ this._value.toString;
        case Type.Object:
            return `Object` ~ this._value.toString;
        default:
            return to!string(this._type) ~ `(` ~ this._value.toString ~ `)`;
            //return to!string(this._type) ~ this._value.toString;
            //break;
        }
    }
}

version (unittest)
{
    import fluent.asserts;
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    //true.should.equal(false).because("this is a failing assert");
    var2 x0 = true;
    writeln(__LINE__, "==>", x0);
    (x0.get!string).should.equal("true");
    (x0.toString).should.equal("Boolean(true)");
    (x0.get!long).should.equal(1);
    (x0.get!int).should.equal(1);
    (x0.get!real).should.equal(1);
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x1 = 123.45;
    writeln(__LINE__, "==>", x1);
    (x1.get!string).should.equal("123.45");
    (x1.toString).should.equal("Floating(123.45)");
    (x1.get!long).should.equal(123);
    (x1.get!real).should.be.approximately(123.45, 0.01);
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x2 = 123;
    writeln(__LINE__, "==>", x2);
    (x2.get!string).should.equal("123");
    (x2.toString).should.equal("Integral(123)");
    (x2.get!long).should.equal(123);
    (x2.get!real).should.equal(123);
    (x2.get!string).should.equal("123");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    int add2(int a, int b)
    {
        return a + b;
    }
    var2 x3 = &add2;
    writeln(__LINE__, "==>", x3);
    var2 answer = x3(11, 22.5);
    answer.toString.should.equal("Integral(33)");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x4;
    writeln(__LINE__, "==>", x4);
    (x4.get!string).should.equal("null");
    (x4.toString).should.equal("Null");
    (x4.get!long).should.equal(0);
    (x4.get!real).should.equal(0);
    (x4.get!string).should.equal("null");
}

