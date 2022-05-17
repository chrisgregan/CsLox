using CsLox.Collections;
using CsLox.Runtime;
using CsLox.SyntaxTree;
using CsLox.Tokens;
using CsLox.ExtensionMethods;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CsLox.ErrorHandlers;

namespace CsLox.Runtime
{
    class Transpiler : Expr.IVisitor<object>, Stmt.IVisitor<object>
    {
        private readonly IErrorHandler _error_handler;
        private readonly StackList<HashMap<string, bool?>> _scopes = new StackList<HashMap<string, bool?>>();
        private readonly HashMap<string, string> _returnTypes = new HashMap<string, string>();

        private FunctionType _current_function = FunctionType.NONE;
        private ClassType _current_class = ClassType.NONE;

        private StringBuilder _code = new StringBuilder();
        private int _indent;

        public Transpiler(IErrorHandler error_handler)
        {
            _error_handler = error_handler;
        }

        public string GetCode()
        {
            return _code.ToString();
        }

        /// <summary>
        /// Transpile the scope for a block
        /// </summary>
        /// <param name="stmt">The block statement</param>
        public object Visit(Stmt.Block stmt)
        {
            Indent();
            _code.Append("{\n");
            _indent++;

            BeginScope();
            Transpile(stmt.Statements);
            EndScope();

            _indent--;
            Indent();
            _code.Append("}\n");

            return null;
        }

        /// <summary>
        /// Transpile a variable declaration
        /// </summary>
        /// <param name="stmt"></param>
        /// <returns></returns>
        public object Visit(Stmt.VarDeclaration stmt)
        {
            switch (stmt.VarType)
            {
                case VarType.Number:
                    _code.Append("double");
                    break;
                case VarType.String:
                    _code.Append("string");
                    break;
            }

            _code.Append($" {stmt.Name.Lexeme}");

            // Declare(stmt.Name);
            if (stmt.Initializer != null)
            {
                _code.Append($" = ");
                Transpile(stmt.Initializer);
            }

            // Todo: Should this always end with a ;?
            _code.Append(";\n");

            //Define(stmt.Name);

            return null;

        }

        /// <summary>
        /// Transpile a variable expression
        /// </summary>
        /// <param name="expr">The expression</param>
        public object Visit(Expr.Variable expr)
        {
            if (!_scopes.IsEmpty() && _scopes.Peek().Get(expr.Name.Lexeme) == false)
            {
                _error_handler.Error(expr.Name, "Cannot read local variable in its own initializer.");
            }
            TranspileLocal(expr, expr.Name);

            return null;

        }

        /// <summary>
        /// Transpile a assignement
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Assign expr)
        {
            _code.Append($"{expr.Name.Lexeme} = ");

            Transpile(expr.value);

            _code.Append(";\n");

            //TranspileLocal(expr, expr.Name);
            return null;

        }

        /// <summary>
        /// Transpile a function declaration
        /// </summary>
        /// <param name="stmt">The statement</param>
        public object Visit(Stmt.Function stmt)
        {
            // Declare and define the function name
            Declare(stmt.Name);
            Define(stmt.Name);

            if (stmt.ReturnType != null)
            {
                // Todo: Figure out how to record the return type in the function's scope
            }

            TranspileFunction(stmt, FunctionType.FUNCTION);
            return null;
        }

        /// <summary>
        /// Transpile a statement expression
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.ExpressionStatement stmt)
        {
            Transpile(stmt.Expression);
            return null;
        }

        /// <summary>
        /// Transpile an if statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.If stmt)
        {
            _code.Append("if (");
            Transpile(stmt.Condition);
            _code.Append(")\n");

            Transpile(stmt.ThenBranch);
            if (stmt.ElseBranch != null)
            {
                _code.Append("else ");
                Transpile(stmt.ElseBranch);
            }

            return null;
        }

        /// <summary>
        /// Transpile a print statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.Print stmt)
        {
            _code.Append("Console.WriteLine(");

            Transpile(stmt.Expression);

            _code.Append(");\n");

            return null;
        }

        /// <summary>
        /// Transpile a return statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.Return stmt)
        {
            // Make sure we are in a function
            if (_current_function == FunctionType.NONE)
            {
                _error_handler.Error(stmt.Keyword, "Cannot return from top-level code.");
            }

            if (stmt.Value != null)
            {
                // Check this is not in a initializer
                if (_current_function == FunctionType.INITIALIZER)
                {
                    _error_handler.Error(stmt.Keyword, "Cannot return from an initializer.");
                }

                Transpile(stmt.Value);
            }
            return null;
        }

        /// <summary>
        /// Transpile a while statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.While stmt)
        {
            _code.Append("while (");
            Transpile(stmt.Condition);
            _code.Append(")\n");
            Transpile(stmt.Body);

            return null;
        }

        /// <summary>
        /// Transpile a continue statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.Continue stmt)
        {
            return null;
        }

        /// <summary>
        /// Transpile a break statement
        /// </summary>
        /// <param name="stmt"></param>
        /// <returns></returns>
        public object Visit(Stmt.Break stmt)
        {

            return null;
        }

        /// <summary>
        /// Transpile a binary expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Binary expr)
        {
            Transpile(expr.Left);
            _code.Append($" {expr.Operator.Lexeme} ");
            Transpile(expr.Right);

            return null;
        }

        /// <summary>
        /// Transpile a call expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Call expr)
        {
            Transpile(expr.Callee);
            _code.Append("(");

            bool first = true;
            foreach (var kv in expr.Arguments)
            {
                if (!first)
                {
                    _code.Append(", ");
                }

                Expr arg = kv.Value;
                var argName = kv.Key;
                _code.Append($"{argName.Lexeme}:");
                Transpile(arg);

                first = false;
            }

            // Todo: What if there's another statement to the right?
            _code.Append(");\n");

            return null;
        }

        /// <summary>
        /// Transpile a property get
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Get expr)
        {
            Transpile(expr.Object);
            return null;
        }

        /// <summary>
        /// Transpile a property set
        /// </summary>
        /// <param name="expr"></param>
        /// <returns></returns>
        public object Visit(Expr.Set expr)
        {
            Transpile(expr.Value);
            Transpile(expr.Object);
            return null;
        }

        /// <summary>
        /// Transpile a superclass access
        /// </summary>
        /// <param name="expr">The expression</param>
        public object Visit(Expr.Super expr)
        {
            if (_current_class == ClassType.NONE)
            {
                _error_handler.Error(expr.keyword, "Cannot use 'super' outside of a class.");
            }
            else if (_current_class != ClassType.SUBCLASS)
            {
                _error_handler.Error(expr.keyword, "Cannot use 'super' in a class with no superclass.");
            }



            TranspileLocal(expr, expr.keyword);
            return null;
        }

        /// <summary>
        /// Transpile a grouping expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Grouping expr)
        {
            _code.Append("(");
            Transpile(expr.Expression);
            _code.Append(")");

            return null;
        }

        /// <summary>
        /// Transpile a literal expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Literal expr)
        {
            switch (expr.Value)
            {
                case string s:
                    _code.Append($"\"");
                    _code.Append(s);
                    _code.Append($"\"");
                    break;
                case double d:
                    _code.Append(d.ToString());
                    break;
                case bool b:
                    _code.Append(b ? "true" : "false");
                    break;

            }

            return null;
        }

        /// <summary>
        /// Transpile a logical expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Logical expr)
        {
            Transpile(expr.Left);
            Transpile(expr.Right);

            return null;
        }

        /// <summary>
        /// Transpile an unary expression
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.Unary expr)
        {
            Transpile(expr.Right);

            return null;
        }

        /// <summary>
        /// Transpile a class declaration
        /// </summary>
        /// <param name="stmt">The statement</param>
        /// <returns></returns>
        public object Visit(Stmt.Class stmt)
        {
            Declare(stmt.Name);
            Define(stmt.Name);

            ClassType enclosing_class = _current_class;
            _current_class = ClassType.CLASS;

            // Superclass
            if (stmt.Superclass != null)
            {
                _current_class = ClassType.SUBCLASS;
                Transpile(stmt.Superclass);

                // Create a new super class scope
                BeginScope();
                _scopes.Peek().Put("super", true);


            }


            // This
            BeginScope();
            _scopes.Peek().Put("this", true);

            // Methods
            foreach (Stmt.Function method in stmt.Methods)
            {
                FunctionType declaration = FunctionType.METHOD;

                // Check if this is the initalizer
                if (method.Name.Lexeme.Equals("init"))
                {
                    declaration = FunctionType.INITIALIZER;
                }

                TranspileFunction(method, declaration);
            }

            EndScope();

            // If we have a superclass, we need to end that scope too
            if (stmt.Superclass != null)
            {
                EndScope();
            }

            _current_class = enclosing_class;

            return null;
        }

        /// <summary>
        /// Transpile this
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <returns></returns>
        public object Visit(Expr.This expr)
        {
            // Make sure we are in a class
            if (_current_class == ClassType.NONE)
            {
                _error_handler.Error(expr.Keyword, "Cannot use 'this' outside of a class.");
            }

            TranspileLocal(expr, expr.Keyword);
            return null;
        }


        /// <summary>
        /// Transpile scope for a list of statements
        /// </summary>
        /// <param name="statements">The statements</param>
        public void Transpile(IEnumerable<Stmt> statements)
        {
            foreach (Stmt statement in statements)
            {
                Indent();
                Transpile(statement);
            }
        }

        /// <summary>
        /// Transpile the scope for a statement
        /// </summary>
        /// <param name="stmt">The statement</param>
        private void Transpile(Stmt stmt)
        {
            stmt.Accept(this);
        }

        private void Indent()
        {
            for (int i = 0; i < _indent; i++)
            {
                _code.Append("    ");
            }
        }

        /// <summary>
        /// Transpile a expression
        /// </summary>
        /// <param name="expr">The expression</param>
        private void Transpile(Expr expr)
        {
            expr.Accept(this);
        }

        /// <summary>
        /// Transpile a local variable
        /// </summary>
        /// <param name="expr">The expression</param>
        /// <param name="name">The name token</param>
        private void TranspileLocal(Expr expr, Token name)
        {
            _code.Append(name.Lexeme);

            // Look down the stack
            for (int i = _scopes.Count() - 1; i >= 0; i--)
            {
                if (_scopes[i].ContainsKey(name.Lexeme))
                {
                    //_interpeter.Transpile(expr, _scopes.Count() - 1 - i);
                }
            }

            // Global?
        }


        /// <summary>
        /// Transpile a function, creating a scope and its parameters
        /// </summary>
        /// <param name="function">The function</param>
        private void TranspileFunction(Stmt.Function function, FunctionType type)
        {
            // Keep track of functions
            FunctionType enclosing_function = _current_function;
            _current_function = type;

            BeginScope();
            foreach (Token param in function.Parameters)
            {
                Declare(param);
                Define(param);
            }

            // Todo: Store current return type globally here and then copy it in return

            Transpile(function.Body);
            EndScope();

            _current_function = enclosing_function;

        }


        /// <summary>
        /// Begin a new scope
        /// </summary>
        private void BeginScope()
        {
            _scopes.Push(new HashMap<string, bool?>());
        }

        /// <summary>
        /// End a scope
        /// </summary>
        private void EndScope()
        {
            _scopes.Pop();
        }

        /// <summary>
        /// Declare a variable into the current scope
        /// </summary>
        /// <param name="name">The variable name token</param>
        private void Declare(Token name)
        {
            // Make sure there is a active scope
            if (!_scopes.Any()) return;

            HashMap<string, bool?> scope = _scopes.Peek();

            // Make sure we hav't already declared this variable
            if (scope.ContainsKey(name.Lexeme))
            {
                _error_handler.Error(name, "Variable with this name already in this scope.");
            }


            scope.Put(name.Lexeme, false);
        }

        /// <summary>
        /// Define a varibale into the current scope
        /// </summary>
        /// <param name="name">The variable name token</param>
        private void Define(Token name)
        {
            if (!_scopes.Any()) return;

            _scopes.Peek().Put(name.Lexeme, true);

        }


        private enum FunctionType
        {
            NONE,
            FUNCTION,
            INITIALIZER,
            METHOD
        }

        private enum ClassType
        {
            NONE,
            CLASS,
            SUBCLASS
        }


    }
}
