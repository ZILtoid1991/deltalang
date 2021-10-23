module deltalang.expressions;

import deltalang.script;

import std.conv;
/** 
 * Parent class of all other expressions.
 */
class Expression {
	abstract InterpretResult interpret(PrototypeObject sc);

	/// this returns an AST object that can be inspected and possibly altered
	/// by the script. Calling the returned object will interpret the object in
	/// the original scope passed
	var toScriptExpressionObject(PrototypeObject sc) {
		var obj = var.emptyObject;

		obj["type"] = typeid(this).name;
		obj["toSourceCode"] = (var _this, var[] args) {
			Expression e = this;
			return var(e.toString());
		};
		obj["opCall"] = (var _this, var[] args) {
			Expression e = this;
			// FIXME: if they changed the properties in the
			// script, we should update them here too.
			return e.interpret(sc).value;
		};
		obj["interpolate"] = (var _this, var[] args) {
			StringLiteralExpression e = cast(StringLiteralExpression) this;
			if(!e)
				return var(null);
			return e.interpolate(args.length ? args[0] : var(null), sc);
		};


		// adding structure is going to be a little bit magical
		// I could have done this with a virtual function, but I'm lazy.
		addChildElementsOfExpressionToScriptExpressionObject(typeid(this), this, sc, obj);

		return obj;
	}

	string toInterpretedString(PrototypeObject sc) {
		return toString();
	}
}
/** 
 * Implements a `mixin` expression, similar to D's own `mixin`.
 * Enables mixing a string data as code.
 */
class MixinExpression : Expression {
	Expression e1;
	this(Expression e1) {
		this.e1 = e1;
	}

	override string toString() { return "mixin(" ~ e1.toString() ~ ")"; }

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(.interpret(e1.interpret(sc).value.get!string ~ ";", sc), sc);
	}
}

class StringLiteralExpression : Expression {
	string content;
	bool allowInterpolation;

	ScriptToken token;

	override string toString() {
		import std.string : replace;
		return "\"" ~ content.replace(`\`, `\\`).replace("\"", "\\\"") ~ "\"";
	}

	this(ScriptToken token) {
		this.token = token;
		this(token.str);
		if(token.wasSpecial == "\"")
			allowInterpolation = true;

	}

	this(string s) {
		content = s;
	}

	var interpolate(var funcObj, PrototypeObject sc) {
		import std.string : indexOf;
		if(allowInterpolation) {
			string r;

			auto c = content;
			auto idx = c.indexOf("#{");
			while(idx != -1) {
				r ~= c[0 .. idx];
				c = c[idx + 2 .. $];
				idx = 0;
				int open = 1;
				while(idx < c.length) {
					if(c[idx] == '}')
						open--;
					else if(c[idx] == '{')
						open++;
					if(open == 0)
						break;
					idx++;
				}
				if(open != 0)
					throw new ScriptRuntimeException("Unclosed interpolation thing", token.scriptFilename, token.lineNumber);
				auto code = c[0 .. idx];

				var result = .interpret(code, sc);

				if(funcObj == var(null))
					r ~= result.get!string;
				else
					r ~= funcObj(result).get!string;

				c = c[idx + 1 .. $];
				idx = c.indexOf("#{");
			}

			r ~= c;
			return var(r);
		} else {
			return var(content);
		}
	}

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(interpolate(var(null), sc), sc);
	}
}

class BoolLiteralExpression : Expression {
	bool literal;
	this(string l) {
		literal = to!bool(l);
	}

	override string toString() { return to!string(literal); }

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(var(literal), sc);
	}
}

class IntLiteralExpression : Expression {
	long literal;

	this(string s) {
		literal = to!long(s);
	}

	override string toString() { return to!string(literal); }

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(var(literal), sc);
	}
}
class FloatLiteralExpression : Expression {
	this(string s) {
		literal = to!real(s);
	}
	real literal;
	override string toString() { return to!string(literal); }
	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(var(literal), sc);
	}
}
class NullLiteralExpression : Expression {
	this() {}
	override string toString() { return "null"; }

	override InterpretResult interpret(PrototypeObject sc) {
		var n;
		return InterpretResult(n, sc);
	}
}
class NegationExpression : Expression {
	Expression e;
	this(Expression e) { this.e = e;}
	override string toString() { return "-" ~ e.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		var n = e.interpret(sc).value;
		return InterpretResult(-n, sc);
	}
}
class NotExpression : Expression {
	Expression e;
	this(Expression e) { this.e = e;}
	override string toString() { return "!" ~ e.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		var n = e.interpret(sc).value;
		return InterpretResult(var(!n), sc);
	}
}
class BitFlipExpression : Expression {
	Expression e;
	this(Expression e) { this.e = e;}
	override string toString() { return "~" ~ e.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		var n = e.interpret(sc).value;
		// possible FIXME given the size. but it is fuzzy when dynamic..
		return InterpretResult(var(~(n.get!long)), sc);
	}
}

class ArrayLiteralExpression : Expression {
	this() {}

	override string toString() {
		string s = "[";
		foreach(i, ele; elements) {
			if(i) s ~= ", ";
			s ~= ele.toString();
		}
		s ~= "]";
		return s;
	}

	Expression[] elements;
	override InterpretResult interpret(PrototypeObject sc) {
		var n = var.emptyArray;
		foreach(i, element; elements)
			n[i] = element.interpret(sc).value;
		return InterpretResult(n, sc);
	}
}
class ObjectLiteralExpression : Expression {
	Expression[string] elements;

	override string toString() {
		string s = "#{";
		bool first = true;
		foreach(k, e; elements) {
			if(first)
				first = false;
			else
				s ~= ", ";

			s ~= "\"" ~ k ~ "\":"; // FIXME: escape if needed
			s ~= e.toString();
		}

		s ~= "}";
		return s;
	}

	PrototypeObject backing;
	this(PrototypeObject backing = null) {
		this.backing = backing;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var n;
		if(backing is null)
			n = var.emptyObject;
		else
			n._object = backing;

		foreach(k, v; elements)
			n[k] = v.interpret(sc).value;

		return InterpretResult(n, sc);
	}
}
class FunctionLiteralExpression : Expression {
	this() {
		// we want this to not be null at all when we're interpreting since it is used as a comparison for a magic operation
		if(DefaultArgumentDummyObject is null)
			DefaultArgumentDummyObject = new PrototypeObject();
	}

	this(VariableDeclaration args, Expression bod, PrototypeObject lexicalScope = null) {
		this();
		this.arguments = args;
		this.functionBody = bod;
		this.lexicalScope = lexicalScope;
	}

	override string toString() {
		string s = (isMacro ? "macro" : "function") ~ " (";
		if(arguments !is null)
			s ~= arguments.toString();

		s ~= ") ";
		s ~= functionBody.toString();
		return s;
	}

	/*
		function identifier (arg list) expression

		so
		var e = function foo() 10; // valid
		var e = function foo() { return 10; } // also valid

		// the return value is just the last expression's result that was evaluated
		// to return void, be sure to do a "return;" at the end of the function
	*/
	VariableDeclaration arguments;
	Expression functionBody; // can be a ScopeExpression btw

	PrototypeObject lexicalScope;

	bool isMacro;

	override InterpretResult interpret(PrototypeObject sc) {
		assert(DefaultArgumentDummyObject !is null);
		var v;
		v._metadata = new ScriptFunctionMetadata(this);
		v._function = (var _this, var[] args) {
			auto argumentsScope = new PrototypeObject();
			PrototypeObject scToUse;
			if(lexicalScope is null)
				scToUse = sc;
			else {
				scToUse = lexicalScope;
				scToUse._secondary = sc;
			}

			argumentsScope.prototype = scToUse;

			argumentsScope._getMember("this", false, false) = _this;
			argumentsScope._getMember("_arguments", false, false) = args;
			argumentsScope._getMember("_thisfunc", false, false) = v;

			if(arguments)
			foreach(i, identifier; arguments.identifiers) {
				argumentsScope._getMember(identifier, false, false); // create it in this scope...
				if(i < args.length && !(args[i].payloadType() == var.Type.Object && args[i]._payload._object is DefaultArgumentDummyObject))
					argumentsScope._getMember(identifier, false, true) = args[i];
				else
				if(arguments.initializers[i] !is null)
					argumentsScope._getMember(identifier, false, true) = arguments.initializers[i].interpret(sc).value;
			}

			if(functionBody !is null)
				return functionBody.interpret(argumentsScope).value;
			else {
				assert(0);
			}
		};
		if(isMacro) {
			var n = var.emptyObject;
			n._object = new MacroPrototype(v);
			v = n;
		}
		return InterpretResult(v, sc);
	}
}

class CastExpression : Expression {
	string type;
	Expression e1;

	override string toString() {
		return "cast(" ~ type ~ ") " ~ e1.toString();
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var n = e1.interpret(sc).value;
		switch(type) {
			foreach(possibleType; CtList!("int", "long", "float", "double", "real", "char", "dchar", "string", "int[]", "string[]", "float[]")) {
			case possibleType:
				n = mixin("cast(" ~ possibleType ~ ") n");
			break;
			}
			default:
				// FIXME, we can probably cast other types like classes here.
		}

		return InterpretResult(n, sc);
	}
}

class VariableDeclaration : Expression {
	string[] identifiers;
	Expression[] initializers;
	string[] typeSpecifiers;

	this() {}

	override string toString() {
		string s = "";
		foreach(i, ident; identifiers) {
			if(i)
				s ~= ", ";
			s ~= "var ";
			if(typeSpecifiers[i].length) {
				s ~= typeSpecifiers[i];
				s ~= " ";
			}
			s ~= ident;
			if(initializers[i] !is null)
				s ~= " = " ~ initializers[i].toString();
		}
		return s;
	}


	override InterpretResult interpret(PrototypeObject sc) {
		var n;

		foreach(i, identifier; identifiers) {
			n = sc._getMember(identifier, false, false);
			auto initializer = initializers[i];
			if(initializer) {
				n = initializer.interpret(sc).value;
				sc._getMember(identifier, false, false) = n;
			}
		}
		return InterpretResult(n, sc);
	}
}

class FunctionDeclaration : Expression {
	DotVarExpression where;
	string ident;
	FunctionLiteralExpression expr;

	this(DotVarExpression where, string ident, FunctionLiteralExpression expr) {
		this.where = where;
		this.ident = ident;
		this.expr = expr;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var n = expr.interpret(sc).value;

		var replacement;

		if(expr.isMacro) {
			// can't overload macros
			replacement = n;
		} else {
			var got;

			if(where is null) {
				got = sc._getMember(ident, false, false);
			} else {
				got = where.interpret(sc).value;
			}

			OverloadSet os = got.get!OverloadSet;
			if(os is null) {
				os = new OverloadSet;
			}

			os.addOverload(OverloadSet.Overload(expr.arguments ? toTypes(expr.arguments.typeSpecifiers, sc) : null, n));

			replacement = var(os);
		}

		if(where is null) {
			sc._getMember(ident, false, false) = replacement;
		} else {
			where.setVar(sc, replacement, false, true);
		}

		return InterpretResult(n, sc);
	}

	override string toString() {
		string s = (expr.isMacro ? "macro" : "function") ~ " ";
		s ~= ident;
		s ~= "(";
		if(expr.arguments !is null)
			s ~= expr.arguments.toString();

		s ~= ") ";
		s ~= expr.functionBody.toString();

		return s;
	}
}

template CtList(T...) { alias CtList = T; }

class BinaryExpression : Expression {
	string op;
	Expression e1;
	Expression e2;

	override string toString() {
		return e1.toString() ~ " " ~ op ~ " " ~ e2.toString();
	}

	override string toInterpretedString(PrototypeObject sc) {
		return e1.toInterpretedString(sc) ~ " " ~ op ~ " " ~ e2.toInterpretedString(sc);
	}

	this(string op, Expression e1, Expression e2) {
		this.op = op;
		this.e1 = e1;
		this.e2 = e2;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var left = e1.interpret(sc).value;
		var right = e2.interpret(sc).value;

		//writeln(left, " "~op~" ", right);

		var n;
		sw: switch(op) {
			// I would actually kinda prefer this to be static foreach, but normal
			// tuple foreach here has broaded compiler compatibility.
			foreach(ctOp; CtList!("+", "-", "*", "/", "==", "!=", "<=", ">=", ">", "<", "~", "&&", "||", "&", "|", "^", "%")) //, ">>", "<<", ">>>")) // FIXME
			case ctOp: {
				n = mixin("left "~ctOp~" right");
				break sw;
			}
			default:
				assert(0, op);
		}

		return InterpretResult(n, sc);
	}
}

class OpAssignExpression : Expression {
	string op;
	Expression e1;
	Expression e2;

	this(string op, Expression e1, Expression e2) {
		this.op = op;
		this.e1 = e1;
		this.e2 = e2;
	}

	override string toString() {
		return e1.toString() ~ " " ~ op ~ "= " ~ e2.toString();
	}

	override InterpretResult interpret(PrototypeObject sc) {

		auto v = cast(VariableExpression) e1;
		if(v is null)
			throw new ScriptRuntimeException("not an lvalue", null, 0 /* FIXME */);

		var right = e2.interpret(sc).value;

		//writeln(left, " "~op~"= ", right);

		var n;
		foreach(ctOp; CtList!("+=", "-=", "*=", "/=", "~=", "&=", "|=", "^=", "%="))
			if(ctOp[0..1] == op)
				n = mixin("v.getVar(sc) "~ctOp~" right");

		// FIXME: ensure the variable is updated in scope too

		return InterpretResult(n, sc);

	}
}

class PipelineExpression : Expression {
	Expression e1;
	Expression e2;
	CallExpression ce;
	ScriptLocation loc;

	this(ScriptLocation loc, Expression e1, Expression e2) {
		this.loc = loc;
		this.e1 = e1;
		this.e2 = e2;

		if(auto ce = cast(CallExpression) e2) {
			this.ce = new CallExpression(loc, ce.func);
			this.ce.arguments = [e1] ~ ce.arguments;
		} else {
			this.ce = new CallExpression(loc, e2);
			this.ce.arguments ~= e1;
		}
	}

	override string toString() { return e1.toString() ~ " |> " ~ e2.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		return ce.interpret(sc);
	}
}

class AssignExpression : Expression {
	Expression e1;
	Expression e2;
	bool suppressOverloading;

	this(Expression e1, Expression e2, bool suppressOverloading = false) {
		this.e1 = e1;
		this.e2 = e2;
		this.suppressOverloading = suppressOverloading;
	}

	override string toString() { return e1.toString() ~ " = " ~ e2.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		auto v = cast(VariableExpression) e1;
		if(v is null)
			throw new ScriptRuntimeException("not an lvalue", null, 0 /* FIXME */);

		auto ret = v.setVar(sc, e2 is null ? var(null) : e2.interpret(sc).value, false, suppressOverloading);

		return InterpretResult(ret, sc);
	}
}
class VariableExpression : Expression {
	string identifier;
	ScriptLocation loc;

	this(string identifier, ScriptLocation loc = ScriptLocation.init) {
		this.identifier = identifier;
		this.loc = loc;
	}

	override string toString() {
		return identifier;
	}

	override string toInterpretedString(PrototypeObject sc) {
		return getVar(sc).get!string;
	}

	ref var getVar(PrototypeObject sc, bool recurse = true) {
		try {
			return sc._getMember(identifier, true /* FIXME: recurse?? */, true);
		} catch(DynamicTypeException dte) {
			dte.callStack ~= loc;
			throw dte;
		}
	}

	ref var setVar(PrototypeObject sc, var t, bool recurse = true, bool suppressOverloading = false) {
		return sc._setMember(identifier, t, true /* FIXME: recurse?? */, true, suppressOverloading);
	}

	ref var getVarFrom(PrototypeObject sc, ref var v) {
		return v[identifier];
	}

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(getVar(sc), sc);
	}
}

class SuperExpression : Expression {
	VariableExpression dot;
	string origDot;
	this(VariableExpression dot) {
		if(dot !is null) {
			origDot = dot.identifier;
			//dot.identifier = "__super_" ~ dot.identifier; // omg this is so bad
		}
		this.dot = dot;
	}

	override string toString() {
		if(dot is null)
			return "super";
		else
			return "super." ~ origDot;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var a = sc._getMember("super", true, true);
		if(a._object is null)
			throw new Exception("null proto for super");
		PrototypeObject proto = a._object.prototype;
		if(proto is null)
			throw new Exception("no super");
		//proto = proto.prototype;

		if(dot !is null)
			a = proto._getMember(dot.identifier, true, true);
		else
			a = proto._getMember("__ctor", true, true);
		return InterpretResult(a, sc);
	}
}

class DotVarExpression : VariableExpression {
	Expression e1;
	VariableExpression e2;
	bool recurse = true;

	this(Expression e1) {
		this.e1 = e1;
		super(null);
	}

	this(Expression e1, VariableExpression e2, bool recurse = true) {
		this.e1 = e1;
		this.e2 = e2;
		this.recurse = recurse;
		//assert(typeid(e2) == typeid(VariableExpression));
		super("<do not use>");//e1.identifier ~ "." ~ e2.identifier);
	}

	override string toString() {
		return e1.toString() ~ "." ~ e2.toString();
	}

	override ref var getVar(PrototypeObject sc, bool recurse = true) {
		if(!this.recurse) {
			// this is a special hack...
			if(auto ve = cast(VariableExpression) e1) {
				return ve.getVar(sc)._getOwnProperty(e2.identifier);
			}
			assert(0);
		}

		if(e2.identifier == "__source") {
			auto val = e1.interpret(sc).value;
			if(auto meta = cast(ScriptFunctionMetadata) val._metadata)
				return *(new var(meta.convertToString()));
			else
				return *(new var(val.toJson()));
		}

		if(auto ve = cast(VariableExpression) e1) {
			return this.getVarFrom(sc, ve.getVar(sc, recurse));
		} else if(cast(StringLiteralExpression) e1 && e2.identifier == "interpolate") {
			auto se = cast(StringLiteralExpression) e1;
			var* functor = new var;
			//if(!se.allowInterpolation)
				//throw new ScriptRuntimeException("Cannot interpolate this string", se.token.lineNumber);
			(*functor)._function = (var _this, var[] args) {
				return se.interpolate(args.length ? args[0] : var(null), sc);
			};
			return *functor;
		} else {
			// make a temporary for the lhs
			auto v = new var();
			*v = e1.interpret(sc).value;
			return this.getVarFrom(sc, *v);
		}
	}

	override ref var setVar(PrototypeObject sc, var t, bool recurse = true, bool suppressOverloading = false) {
		if(suppressOverloading)
			return e1.interpret(sc).value.opIndexAssignNoOverload(t, e2.identifier);
		else
			return e1.interpret(sc).value.opIndexAssign(t, e2.identifier);
	}


	override ref var getVarFrom(PrototypeObject sc, ref var v) {
		return e2.getVarFrom(sc, v);
	}

	override string toInterpretedString(PrototypeObject sc) {
		return getVar(sc).get!string;
	}
}

class IndexExpression : VariableExpression {
	Expression e1;
	Expression e2;

	this(Expression e1, Expression e2) {
		this.e1 = e1;
		this.e2 = e2;
		super(null);
	}

	override string toString() {
		return e1.toString() ~ "[" ~ e2.toString() ~ "]";
	}

	override ref var getVar(PrototypeObject sc, bool recurse = true) {
		if(auto ve = cast(VariableExpression) e1)
			return ve.getVar(sc, recurse)[e2.interpret(sc).value];
		else {
			auto v = new var();
			*v = e1.interpret(sc).value;
			return this.getVarFrom(sc, *v);
		}
	}

	override ref var setVar(PrototypeObject sc, var t, bool recurse = true, bool suppressOverloading = false) {
        	return getVar(sc,recurse) = t;
	}
}

class SliceExpression : Expression {
	// e1[e2 .. e3]
	Expression e1;
	Expression e2;
	Expression e3;

	this(Expression e1, Expression e2, Expression e3) {
		this.e1 = e1;
		this.e2 = e2;
		this.e3 = e3;
	}

	override string toString() {
		return e1.toString() ~ "[" ~ e2.toString() ~ " .. " ~ e3.toString() ~ "]";
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var lhs = e1.interpret(sc).value;

		auto specialScope = new PrototypeObject();
		specialScope.prototype = sc;
		specialScope._getMember("$", false, false) = lhs.length;

		return InterpretResult(lhs[e2.interpret(specialScope).value .. e3.interpret(specialScope).value], sc);
	}
}


class LoopControlExpression : Expression {
	InterpretResult.FlowControl op;
	this(string op) {
		if(op == "continue")
			this.op = InterpretResult.FlowControl.Continue;
		else if(op == "break")
			this.op = InterpretResult.FlowControl.Break;
		else assert(0, op);
	}

	override string toString() {
		import std.string;
		return to!string(this.op).toLower();
	}

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(var(null), sc, op);
	}
}


class ReturnExpression : Expression {
	Expression value;

	this(Expression v) {
		value = v;
	}

	override string toString() { return "return " ~ value.toString(); }

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(value.interpret(sc).value, sc, InterpretResult.FlowControl.Return);
	}
}

class ScopeExpression : Expression {
	this(Expression[] expressions) {
		this.expressions = expressions;
	}

	Expression[] expressions;

	override string toString() {
		string s;
		s = "{\n";
		foreach(expr; expressions) {
			s ~= "\t";
			s ~= expr.toString();
			s ~= ";\n";
		}
		s ~= "}";
		return s;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var ret;

		auto innerScope = new PrototypeObject();
		innerScope.prototype = sc;

		innerScope._getMember("__scope_exit", false, false) = var.emptyArray;
		innerScope._getMember("__scope_success", false, false) = var.emptyArray;
		innerScope._getMember("__scope_failure", false, false) = var.emptyArray;

		scope(exit) {
			foreach(func; innerScope._getMember("__scope_exit", false, true))
				func();
		}
		scope(success) {
			foreach(func; innerScope._getMember("__scope_success", false, true))
				func();
		}
		scope(failure) {
			foreach(func; innerScope._getMember("__scope_failure", false, true))
				func();
		}

		foreach(expression; expressions) {
			auto res = expression.interpret(innerScope);
			ret = res.value;
			if(res.flowControl != InterpretResult.FlowControl.Normal)
				return InterpretResult(ret, sc, res.flowControl);
		}
		return InterpretResult(ret, sc);
	}
}

class SwitchExpression : Expression {
	Expression expr;
	CaseExpression[] cases;
	CaseExpression default_;

	override InterpretResult interpret(PrototypeObject sc) {
		auto e = expr.interpret(sc);

		bool hitAny;
		bool fallingThrough;
		bool secondRun;

		var last;

		again:
		foreach(c; cases) {
			if(!secondRun && !fallingThrough && c is default_) continue;
			if(fallingThrough || (secondRun && c is default_) || c.condition.interpret(sc) == e) {
				fallingThrough = false;
				if(!secondRun)
					hitAny = true;
				InterpretResult ret;
				expr_loop: foreach(exp; c.expressions) {
					ret = exp.interpret(sc);
					with(InterpretResult.FlowControl)
					final switch(ret.flowControl) {
						case Normal:
							last = ret.value;
						break;
						case Return:
						case Goto:
							return ret;
						case Continue:
							fallingThrough = true;
							break expr_loop;
						case Break:
							return InterpretResult(last, sc);
					}
				}

				if(!fallingThrough)
					break;
			}
		}

		if(!hitAny && !secondRun) {
			secondRun = true;
			goto again;
		}

		return InterpretResult(last, sc);
	}
}

class CaseExpression : Expression {
	this(Expression condition) {
		this.condition = condition;
	}
	Expression condition;
	Expression[] expressions;

	override string toString() {
		string code;
		if(condition is null)
			code = "default:";
		else
			code = "case " ~ condition.toString() ~ ":";

		foreach(expr; expressions)
			code ~= "\n" ~ expr.toString() ~ ";";

		return code;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		// I did this inline up in the SwitchExpression above. maybe insane?!
		assert(0);
	}
}

class ForeachExpression : Expression {
	VariableDeclaration decl;
	Expression subject;
	Expression subject2;
	Expression loopBody;

	override string toString() {
		return "foreach(" ~ decl.toString() ~ "; " ~ subject.toString() ~ ((subject2 is null) ? "" : (".." ~ 
                subject2.toString)) ~ ") " ~ loopBody.toString();
	}

	override InterpretResult interpret(PrototypeObject sc) {
		var result;

		assert(loopBody !is null);

		auto loopScope = new PrototypeObject();
		loopScope.prototype = sc;

		InterpretResult.FlowControl flowControl;

		static string doLoopBody() { return q{
			if(decl.identifiers.length > 1) {
				sc._getMember(decl.identifiers[0], false, false) = i;
				sc._getMember(decl.identifiers[1], false, false) = item;
			} else {
				sc._getMember(decl.identifiers[0], false, false) = item;
			}

			auto res = loopBody.interpret(loopScope);
			result = res.value;
			flowControl = res.flowControl;
			if(flowControl == InterpretResult.FlowControl.Break)
				break;
			if(flowControl == InterpretResult.FlowControl.Return)
				break;
			//if(flowControl == InterpretResult.FlowControl.Continue)
				// this is fine, we still want to do the advancement
		};}

		var what = subject.interpret(sc).value;
		var termination = subject2 is null ? var(null) : subject2.interpret(sc).value;
		if(what.payloadType == var.Type.Integral && subject2 is null) {
			// loop from 0 to what
			int end = what.get!int;
			foreach(item; 0 .. end) {
				auto i = item;
				mixin(doLoopBody());
			}
		} else if(what.payloadType == var.Type.Integral && termination.payloadType == var.Type.Integral) {
			// loop what .. termination
			int start = what.get!int;
			int end = termination.get!int;
			int stride;
			if(end < start) {
				stride = -1;
			} else {
				stride = 1;
			}
			int i = -1;
			for(int item = start; item != end; item += stride) {
				i++;
				mixin(doLoopBody());
			}
		} else {
			if(subject2 !is null)
				throw new ScriptRuntimeException("foreach( a .. b ) invalid unless a is an integer", null, 0); // FIXME
			foreach(i, item; what) {
				mixin(doLoopBody());
			}
		}

		if(flowControl != InterpretResult.FlowControl.Return)
			flowControl = InterpretResult.FlowControl.Normal;

		return InterpretResult(result, sc, flowControl);
	}
}

class ForExpression : Expression {
	Expression initialization;
	Expression condition;
	Expression advancement;
	Expression loopBody;

	this() {}

	override InterpretResult interpret(PrototypeObject sc) {
		var result;

		assert(loopBody !is null);

		auto loopScope = new PrototypeObject();
		loopScope.prototype = sc;
		if(initialization !is null)
			initialization.interpret(loopScope);

		InterpretResult.FlowControl flowControl;

		static string doLoopBody() { return q{
			auto res = loopBody.interpret(loopScope);
			result = res.value;
			flowControl = res.flowControl;
			if(flowControl == InterpretResult.FlowControl.Break)
				break;
			if(flowControl == InterpretResult.FlowControl.Return)
				break;
			//if(flowControl == InterpretResult.FlowControl.Continue)
				// this is fine, we still want to do the advancement
			if(advancement)
				advancement.interpret(loopScope);
		};}

		if(condition !is null) {
			while(condition.interpret(loopScope).value) {
				mixin(doLoopBody());
			}
		} else
			while(true) {
				mixin(doLoopBody());
			}

		if(flowControl != InterpretResult.FlowControl.Return)
			flowControl = InterpretResult.FlowControl.Normal;

		return InterpretResult(result, sc, flowControl);
	}

	override string toString() {
		string code = "for(";
		if(initialization !is null)
			code ~= initialization.toString();
		code ~= "; ";
		if(condition !is null)
			code ~= condition.toString();
		code ~= "; ";
		if(advancement !is null)
			code ~= advancement.toString();
		code ~= ") ";
		code ~= loopBody.toString();

		return code;
	}
}

class IfExpression : Expression {
	Expression condition;
	Expression ifTrue;
	Expression ifFalse;

	this() {}

	override InterpretResult interpret(PrototypeObject sc) {
		InterpretResult result;
		assert(condition !is null);

		auto ifScope = new PrototypeObject();
		ifScope.prototype = sc;

		if(condition.interpret(ifScope).value) {
			if(ifTrue !is null)
				result = ifTrue.interpret(ifScope);
		} else {
			if(ifFalse !is null)
				result = ifFalse.interpret(ifScope);
		}
		return InterpretResult(result.value, sc, result.flowControl);
	}

	override string toString() {
		string code = "if ";
		code ~= condition.toString();
		code ~= " ";
		if(ifTrue !is null)
			code ~= ifTrue.toString();
		else
			code ~= " { }";
		if(ifFalse !is null)
			code ~= " else " ~ ifFalse.toString();
		return code;
	}
}

class TernaryExpression : Expression {
	Expression condition;
	Expression ifTrue;
	Expression ifFalse;

	this() {}

	override InterpretResult interpret(PrototypeObject sc) {
		InterpretResult result;
		assert(condition !is null);

		auto ifScope = new PrototypeObject();
		ifScope.prototype = sc;

		if(condition.interpret(ifScope).value) {
			result = ifTrue.interpret(ifScope);
		} else {
			result = ifFalse.interpret(ifScope);
		}
		return InterpretResult(result.value, sc, result.flowControl);
	}

	override string toString() {
		string code = "";
		code ~= condition.toString();
		code ~= " ? ";
		code ~= ifTrue.toString();
		code ~= " : ";
		code ~= ifFalse.toString();
		return code;
	}
}

// this is kinda like a placement new, and currently isn't exposed inside the language,
// but is used for class inheritance
class ShallowCopyExpression : Expression {
	Expression e1;
	Expression e2;

	this(Expression e1, Expression e2) {
		this.e1 = e1;
		this.e2 = e2;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		auto v = cast(VariableExpression) e1;
		if(v is null)
			throw new ScriptRuntimeException("not an lvalue", null, 0 /* FIXME */);

		v.getVar(sc, false)._object.copyPropertiesFrom(e2.interpret(sc).value._object);

		return InterpretResult(var(null), sc);
	}

}

class NewExpression : Expression {
	Expression what;
	Expression[] args;
	this(Expression w) {
		what = w;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		assert(what !is null);

		var[] args;
		foreach(arg; this.args)
			args ~= arg.interpret(sc).value;

		var original = what.interpret(sc).value;
		var n = original._copy_new;
		if(n.payloadType() == var.Type.Object) {
			var ctor = original.prototype ? original.prototype._getOwnProperty("__ctor") : var(null);
			if(ctor)
				ctor.apply(n, args);
		}

		return InterpretResult(n, sc);
	}
}

class ThrowExpression : Expression {
	Expression whatToThrow;
	ScriptToken where;

	this(Expression e, ScriptToken where) {
		whatToThrow = e;
		this.where = where;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		assert(whatToThrow !is null);
		throw new ScriptException(whatToThrow.interpret(sc).value, ScriptLocation(where.scriptFilename, where.lineNumber));
		assert(0);
	}
}

class ExceptionBlockExpression : Expression {
	Expression tryExpression;

	string[] catchVarDecls;
	string[] catchVarTypeSpecifiers;
	Expression[] catchExpressions;

	Expression[] finallyExpressions;

	override InterpretResult interpret(PrototypeObject sc) {
		InterpretResult result;
		result.sc = sc;
		assert(tryExpression !is null);
		assert(catchVarDecls.length == catchExpressions.length);

		void caught(var ex) {
			if(catchExpressions.length)
			foreach(i, ce; catchExpressions) {
				if(catchVarTypeSpecifiers[i].length == 0 || isCompatibleType(ex, catchVarTypeSpecifiers[i], sc)) {
					auto catchScope = new PrototypeObject();
					catchScope.prototype = sc;
					catchScope._getMember(catchVarDecls[i], false, false) = ex;

					result = ce.interpret(catchScope);
					break;
				}
			} else
				result = InterpretResult(ex, sc);
		}

		if(catchExpressions.length || (catchExpressions.length == 0 && finallyExpressions.length == 0))
			try {
				result = tryExpression.interpret(sc);
			} catch(NonScriptCatchableException e) {
				// the script cannot catch these so it continues up regardless
				throw e;
			} catch(ScriptException e) {
				// FIXME: what about the other information here? idk.
				caught(e.payload);
			} catch(Exception e) {
				var ex = var.emptyObject;
				ex.type = typeid(e).name;
				ex.msg = e.msg;
				ex.file = e.file;
				ex.line = e.line;

				caught(ex);
			} finally {
				foreach(fe; finallyExpressions)
					result = fe.interpret(sc);
			}
		else
			try {
				result = tryExpression.interpret(sc);
			} finally {
				foreach(fe; finallyExpressions)
					result = fe.interpret(sc);
			}

		return result;
	}
}

class ParentheticalExpression : Expression {
	Expression inside;
	this(Expression inside) {
		this.inside = inside;
	}

	override string toString() {
		return "(" ~ inside.toString() ~ ")";
	}

	override InterpretResult interpret(PrototypeObject sc) {
		return InterpretResult(inside.interpret(sc).value, sc);
	}
}

class AssertKeyword : Expression {
	ScriptToken token;
	this(ScriptToken token) {
		this.token = token;
	}
	override string toString() {
		return "assert";
	}

	override InterpretResult interpret(PrototypeObject sc) {
		if(AssertKeywordObject is null)
			AssertKeywordObject = new PrototypeObject();
		var dummy;
		dummy._object = AssertKeywordObject;
		return InterpretResult(dummy, sc);
	}
}

PrototypeObject AssertKeywordObject;
PrototypeObject DefaultArgumentDummyObject;

class CallExpression : Expression {
	Expression func;
	Expression[] arguments;
	ScriptLocation loc;

	override string toString() {
		string s = func.toString() ~ "(";
		foreach(i, arg; arguments) {
			if(i) s ~= ", ";
			s ~= arg.toString();
		}

		s ~= ")";
		return s;
	}

	this(ScriptLocation loc, Expression func) {
		this.loc = loc;
		this.func = func;
	}

	override string toInterpretedString(PrototypeObject sc) {
		return interpret(sc).value.get!string;
	}

	override InterpretResult interpret(PrototypeObject sc) {
		if(auto asrt = cast(AssertKeyword) func) {
			auto assertExpression = arguments[0];
			Expression assertString;
			if(arguments.length > 1)
				assertString = arguments[1];

			var v = assertExpression.interpret(sc).value;

			if(!v)
				throw new ScriptException(
					var(this.toString() ~ " failed, got: " ~ assertExpression.toInterpretedString(sc)),
					ScriptLocation(asrt.token.scriptFilename, asrt.token.lineNumber));

			return InterpretResult(v, sc);
		}

		auto f = func.interpret(sc).value;
		bool isMacro =  (f.payloadType == var.Type.Object && ((cast(MacroPrototype) f._payload._object) !is null));
		var[] args;
		foreach(argument; arguments)
			if(argument !is null) {
				if(isMacro) // macro, pass the argument as an expression object
					args ~= argument.toScriptExpressionObject(sc);
				else // regular function, interpret the arguments
					args ~= argument.interpret(sc).value;
			} else {
				if(DefaultArgumentDummyObject is null)
					DefaultArgumentDummyObject = new PrototypeObject();

				var dummy;
				dummy._object = DefaultArgumentDummyObject;

				args ~= dummy;
			}

		var _this;
		if(auto dve = cast(DotVarExpression) func) {
			_this = dve.e1.interpret(sc).value;
		} else if(auto ide = cast(IndexExpression) func) {
			_this = ide.interpret(sc).value;
		} else if(auto se = cast(SuperExpression) func) {
			// super things are passed this object despite looking things up on the prototype
			// so it calls the correct instance
			_this = sc._getMember("this", true, true);
		}

		try {
			return InterpretResult(f.apply(_this, args), sc);
		} catch(DynamicTypeException dte) {
			dte.callStack ~= loc;
			throw dte;
		} catch(ScriptException se) {
			se.callStack ~= loc;
			throw se;
		}
	}
}
