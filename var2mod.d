module var2mod;

import std.stdio;

import std.conv;
import std.format;
import std.traits;
import std.variant;

public abstract class os_value
{
    abstract bool getBoolean();
    abstract long getIntegral();
    abstract real getFloating();
    abstract string getString();
    abstract var2[string] *getDictionary();
}

public class os_bool_value : os_value
{
    private bool _data;
    package this(bool data)
    {
        this._data = data;
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
    override var2[string] *getDictionary()
    {
        return null;
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
    override bool getBoolean()
    {
        return false;
    }
    override long getIntegral()
    {
        return 0;
    }
    override real getFloating()
    {
        return 0;
    }
    override string getString()
    {
        return to!string(this._data);
    }
    override var2[string] *getDictionary()
    {
        return &_data;
    }
}

public struct var2
{
    public enum Type
    {
        Object,
        Array,
        Integral,
        Floating,
        String,
        Function,
        Boolean
    }

    private Type _type;
    private Variant _payload;
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
            alias var2 delegate(var2[]) FuncType;
            FuncType func = this._payload.get!FuncType;
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
        case Type.Boolean:
        case Type.Object:
            static if (is(T == bool))
                return this._value.getBoolean();
            else static if (isFloatingPoint!T)
                return to!T(this._value.getFloating());
            else static if (isIntegral!T)
                return to!T(this._value.getIntegral());
            else static if (isSomeString!T)
                return this._value.getString();
            else if (isAssociativeArray!T)
            {
                var2[string] *dict = this._value.getDictionary();
                if (!dict) return T.init;
                T ret;
                foreach (k, v; (*dict))
                    ret[to!(KeyType!T)(k)] = v.get!(ValueType!T);
                return ret;
            }
            else
                return T.init;
        /+
        case Type.Object:
            Dictionary dict = this._payload.get!Dictionary;
            static if (isAssociativeArray!T)
            {
                T ret;
                foreach (k, v; dict._dict)
                    ret[to!(KeyType!T)(k)] = v.get!(ValueType!T);
                return ret;
            }
            else static if (isSomeString!T)
            {
                return dict.toString;
            }
            else
                return T.init;
        +/
        case Type.Integral:
            long val = this._payload.get!long;
            static if (isFloatingPoint!T || isIntegral!T)
                return to!T(val);
            else static if (isSomeString!T)
                return to!string(val);
            else
                return T.init;
        case Type.Floating:
            real val = this._payload.get!real;
            static if (isFloatingPoint!T || isIntegral!T)
                return to!T(val);
            else static if (isSomeString!T)
                return to!string(val);
            else
                return T.init;
        case Type.String:
            static if (__traits(compiles, to!T(this._payload.get!string)))
            {
                try
                {
                    return to!T(this._payload.get!string);
                }
                catch (Exception e)
                {
                    return T.init;
                }
            }
            else
                return T.init;
        case Type.Array:
            Vector vec = this._payload.get!Vector;
            static if (isSomeString!T)
            {
                return vec.toString;
            }
            else static if (isArray!T)
            {
                T ret;
                static if (is(ElementType!T == void))
                {
                }
                else
                {
                    ////Vector vec = this._payload.get!Vector;
                    alias ElemType = ElementType!T;
                    foreach (item; vec._vec)
                        ret ~= item.get!(ElemType);
                }
                return ret;
            }
            else
                return T.init;
        case Type.Function:
            static if (isSomeString!T)
                return "<function>";
            else
                return T.init;
        }
    }

    public var2 opAssign(T)(T t) if (!is(T == var2))
    {
        static if (isFloatingPoint!T)
        {
            this._type = Type.Floating;
            this._payload = cast(real) t;
        }
        else static if (isIntegral!T)
        {
            this._type = Type.Integral;
            this._payload = cast(long) t;
        }
        else static if (isCallable!T)
        {
            this._type = Type.Function;
            this._payload = delegate var2(var2[] args) {
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
        }
        else static if (isSomeString!T)
        {
            this._type = Type.String;
            this._payload = to!string(t);
        }
        else static if (is(T : var2[string]))
        {
            this._type = Type.Object;
            Dictionary value = new Dictionary;
            var2[string] *dict = value.getDictionary();
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
            Vector vec = new Vector;
            vec._vec.length = t.length;
            static if (!is(T == void[]))
                foreach (i, item; t)
                    vec._vec[i] = var2(item);
            this._payload = vec;
        }
        else static if (is(T == bool))
        {
            this._type = Type.Boolean;
            this._payload = t; /**/
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
            *tmp = _payload.get!string.length;
            return *tmp;
        }
        if (name == "length" && this.payloadType() == Type.Array)
        {
            Vector vec = this._payload.get!Vector;
            var2* tmp = new var2;
            *tmp = vec._vec.length;
            return *tmp;
        }
        if (this.payloadType() == Type.Object)
        {
            //Dictionary dict = this._payload.get!Dictionary;
            var2[string] *dict = this._value.getDictionary();
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
        var2[string] *dict = this._value.getDictionary();
        (*dict)[name] = var2(t);
        return (*dict)[name];
    }

    // N.T.
    public ref var2 opIndex(size_t idx, string file = __FILE__, size_t line = __LINE__)
    {
        if (_type == Type.Array)
        {
            Vector vec = this._payload.get!Vector;
            if (idx < vec._vec.length)
                return vec._vec[idx];
        }
        var2* n = new var2;
        return *n;
    }

    // N.T.
    public ref var2 opIndexAssign(T)(T t, size_t idx, string file = __FILE__, size_t line = __LINE__)
    {
        if (this._type == Type.Array)
        {
            Vector vec = this._payload.get!Vector;
            if (idx >= vec._vec.length)
                vec._vec.length = idx + 1;
            vec._vec[idx] = t;
            return vec._vec[idx];
        }
        var2* n = new var2;
        return *n;
    }

    public string toString()
    {
        switch (this._type)
        {
        case Type.Array:
            return `Array` ~ to!string(this._payload.get!Vector);
        case Type.Object:
            return `Object` ~ to!string(this._payload.get!Dictionary);
        default:
            return to!string(this._type) ~ `(` ~ this._payload.toString ~ `)`;
            //break;
        }
    }
}

package class Vector
{
    var2[] _vec;
    public override string toString()
    {
        //return format!`%s`(this._vec);
        return to!string(this._vec);
    }
}
