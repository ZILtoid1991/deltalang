module deltalang.common;
///If set, then it'll throw on out of bounds, if not null will be returned instead.
version (throw_on_oob)
	package static enum THROW_ON_OOB = true;
else
	package static enum THROW_ON_OOB = false;

///Marks the location of a script for errors, etc.
struct ScriptLocation {
	string scriptFilename;		///The file which contains the script
	int lineNumber;				///The line number where the error, etc occured
}

///Parent exception for all other exceptions within this library.
public class DSException : Exception {
	@nogc @safe pure nothrow this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null)
    {
        super(msg, file, line, nextInChain);
    }

    @nogc @safe pure nothrow this(string msg, Throwable nextInChain, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line, nextInChain);
    }
}