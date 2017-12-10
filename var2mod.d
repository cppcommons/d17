module var2mod;

import std.stdio;
import std.array;
import std.conv;

//import std.json;
import std.traits;
import std.variant;

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

    alias var2 delegate(var2[]) FuncType;
    public var2 apply(var2[] args)
    {
        if (this.payloadType() == Type.Function)
        {
            /+
            if (this._payload.get!FuncType is null)
            {
                return var2(null);
            }
            +/
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
            static if (is(T == bool))
                return this._payload.get!bool;
            else static if (isFloatingPoint!T || isIntegral!T)
                return cast(T)(this._payload.get!bool ? 1 : 0);
            else static if (isSomeString!T)
                return this._payload.get!bool ? "true" : "false";
            else
                return T.init;
        case Type.Object:
            static if (isAssociativeArray!T)
            {
                T ret;
                if (this._payload.type == typeid(var2[string]))
                {
                    foreach (k, v; this._payload.get!(var2[string]))
                        ret[to!(KeyType!T)(k)] = v.get!(ValueType!T);
                }
                return ret;
            }
            else static if (isSomeString!T)
            {
                if (this._payload.type == typeid(var2[string]))
                    return this._payload.toString();
                return "null";
            }
            else
                return T.init;
        case Type.Integral:
            static if (isFloatingPoint!T
                    || isIntegral!T)
                return to!T(this._payload.get!long);
            else static if (isSomeString!T)
                return to!string(this._payload.get!long);
            else
                return T.init;
        case Type.Floating:
            static if (isFloatingPoint!T
                    || isIntegral!T)
                return to!T(this._payload.get!real);
            else static if (isSomeString!T)
                return to!string(this._payload.get!real);
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
            import std.range;

            auto pl = this._payload.get!(var2[]);
            static if (isSomeString!T)
            {
                return to!string(pl);
            }
            else static if (isArray!T)
            {
                T ret;
                static if (is(ElementType!T == void))
                {
                    static assert(0, "try wrapping the function to get rid of void[] args");
                    //alias getType = ubyte;
                }
                else
                    alias getType = ElementType!T;
                foreach (item; pl)
                    ret ~= item.get!(getType);
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
            this._payload = t;
        }
        else static if (isArray!T)
        {
            this._type = Type.Array;
            var2[] arr;
            arr.length = t.length;
            static if (!is(T == void[]))
                foreach (i, item; t)
                    arr[i] = var2(item);
            this._payload = arr;
        }
        else static if (is(T == bool))
        {
            this._type = Type.Boolean;
            this._payload = t;
        }
        else
            static assert(0, "unsupported type");

        return this;
    }
}
