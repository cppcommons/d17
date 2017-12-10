module var2;

import std.stdio;
import std.array;
import std.conv;

//import std.json;
import std.traits;
import std.variant;

public struct var2
{
    alias var2 delegate(var2, var2[]) FuncType;
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

    public var2 apply(var2 _this, var2[] args)
    {
        if (this.payloadType() == Type.Function)
        {
            if (this._payload.get!FuncType is null)
            {
                return var2(null);
            }
            FuncType func = this._payload.get!FuncType;
            return func(_this, args);
        }

        if (this.payloadType() == Type.Integral || this.payloadType() == Type.Floating)
        {
            if (args.length)
                return var2(this.get!real * args[0].get!real);
        }

        return var2(null);
    }

    public var2 call(T...)(var2 _this, T t)
    {
        var2[] args;
        foreach (a; t)
        {
            args ~= var2(a);
        }
        return this.apply(_this, args);
    }

    public var2 opCall(T...)(T t)
    {
        return this.call(this, t);
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
                return this._payload._boolean;
            else static if (isFloatingPoint!T || isIntegral!T)
                return cast(T)(this._payload.get!bool ? 1 : 0); // the cast is for enums, I don't like this so FIXME
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
            /+
            else static if (is(T : PrototypeObject))
            {
                // they are requesting an implementation object, just give it to them
                return cast(T) this._payload._object;
            }
            +/
            else static if (is(T == struct) || is(T == class))
            {
                // first, we'll try to give them back the native object we have, if we have one
                static if (is(T : Object))
                {
                    if (auto wno = cast(WrappedNativeObject) this._payload._object)
                    {
                        auto no = cast(T) wno.getObject();
                        if (no !is null)
                            return no;
                    }

                    // FIXME: this is kinda weird.
                    return null;
                }
                else
                {

                    // failing that, generic struct or class getting: try to fill in the fields by name
                    T t;
                    bool initialized = true;
                    static if (is(T == class))
                    {
                        static if (__traits(compiles, new T()))
                            t = new T();
                        else
                            initialized = false;
                    }

                    if (initialized)
                        foreach (i, a; t.tupleof)
                        {
                            cast(Unqual!(typeof((a)))) t.tupleof[i] = this[t.tupleof[i].stringof[2 .. $]].get!(
                                    typeof(a));
                        }

                    return t;
                }
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
            // is it sane to translate anything else?
        case Type.Function:
            static if (isSomeString!T)
                return "<function>";
            else static if (isDelegate!T)
            {
                // making a local copy because otherwise the delegate might refer to a struct on the stack and get corrupted later or something
                auto func = this._payload._function;

                // the static helper lets me pass specific variables to the closure
                static T helper(typeof(func) func)
                {
                    return delegate ReturnType!T(ParameterTypeTuple!T args) {
                        var[] arr;
                        foreach (arg; args)
                            arr ~= var(arg);
                        var ret = func(var(null), arr);
                        static if (is(ReturnType!T == void))
                            return;
                        else
                            return ret.get!(ReturnType!T);
                    };
                }

                return helper(func);

            }
            else
                return T.init;
            // FIXME: we just might be able to do better for both of these
            //break;
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
            this._payload = delegate var2(var2 _this, var2[] args) {
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
        //else static if (is(T : PrototypeObject))
        else static if (is(T : var2[string]))
        {
            // support direct assignment of pre-made implementation objects
            // so prewrapped stuff can be easily passed.
            this._type = Type.Object;
            this._payload = t;
        }
        else static if (isArray!T)
        {
            this._type = Type.Array;
            var2[] arr;
            arr.length = t.length;
            static if (!is(T == void[])) // we can't append a void array but it is nice to support x = [];
                foreach (i, item; t)
                    arr[i] = var2(item);
            this._payload = arr;
        }
        else static if (is(T == bool))
        {
            this._type = Type.Boolean;
            this._payload._boolean = t;
        }
        /+
        else static if (isSomeChar!T)
        {
            this._type = Type.String;
            this._payload._string = "";
            import std.utf;

            char[4] ugh;
            auto size = encode(ugh, t);
            this._payload._string = ugh[0 .. size].idup;
        }
        +/
        else
            static assert(0, "unsupported type");

        return this;
    }
}
