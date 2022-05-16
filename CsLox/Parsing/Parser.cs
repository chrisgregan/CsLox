using CsLox.ErrorHandlers;
using CsLox.SyntaxTree;
using CsLox.Tokens;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CsLox.Parsing
{
    class Parser
    {
        private readonly IErrorHandler _error_handler;
        private readonly List<Token> _tokens;
        private int _current = 0;
        private int _loop_depth = 0;

        /// <summary>
        /// Create a new Parser instance
        /// </summary>
        /// <param name="tokens">The input tokens</param>
        public Parser(List<Token> tokens, IErrorHandler error_handler)
        {
            this._tokens = tokens;
            this._error_handler = error_handler;
        }

        /// <summary>
        /// Parse the tokens
        /// </summary>
        /// <returns>The parsed statements</returns>
        public List<Stmt> Parse()
        {
            List<Stmt> statements = new List<Stmt>();

            while (!IsAtEnd())
            {
                statements.Add(Declaration());
            }

            return statements;

        }

        /// <summary>
        /// Parse a declaration
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt Declaration()
        {
            try
            {
                if (Match(TokenType.CLASS)) return ClassDeclaration();
                if (Match(TokenType.FUNCTION)) return Function("function");
                if (Match(TokenType.NUMBER_TYPE) ||
                    Match(TokenType.STRING_TYPE) ||
                    Match(TokenType.BOOLEAN_TYPE))
                {
                    return VarDeclaration();
                }

                return Statement(false);
            }
            catch (ParseErrorException)
            {
                // Try and resync if there is an error
                Synchronise();
                return null;
            }
        }

        /// <summary>
        /// Parse a class declaration
        /// </summary>
        private Stmt ClassDeclaration()
        {

            Token name = Consume(TokenType.IDENTIFIER, "Expect class name");

            // Superclass
            Expr.Variable superclass = null;
            if (Match(TokenType.LESS))
            {
                Consume(TokenType.IDENTIFIER, "Expect superclass name.");
                superclass = new Expr.Variable(Previous());
            }


            // Body
            // Consume(TokenType.BEGIN, "Expect '' before class body");
            List<Stmt.Function> methods = new List<Stmt.Function>();
            while (!Check(TokenType.END) && !IsAtEnd())
            {
                Consume(TokenType.FUNCTION, "Expect 'Function' in class body.");
                methods.Add((Stmt.Function)Function("method"));
            }

            Consume(TokenType.END, "Expect 'End' after class body.");

            return new Stmt.Class(name, superclass, methods);
        }


        /// <summary>
        /// Parse a function declaration
        /// </summary>
        /// <param name="kind"></param>
        /// <returns></returns>
        private Stmt.Function Function(string kind)
        {
            Token name = Consume(TokenType.IDENTIFIER, $"Expect {kind} name.");

            // Parameters
            Consume(TokenType.LEFT_PAREN, $"Expect '(' after {kind} name.");
            List<Token> parameters = new List<Token>();
            List<Token> parameterTypes = new List<Token>();
            Token returnType = null;
            if (!Check(TokenType.RIGHT_PAREN))
            {
                do
                {
                    if (parameters.Count() >= 8)
                    {
                        Error(Peek(), "Cannot have more than 8 parameters");
                    }

                    if (Match(TokenType.NUMBER_TYPE, TokenType.STRING_TYPE, TokenType.BOOLEAN_TYPE))
                    {
                        parameterTypes.Add(Previous());
                    }
                    parameters.Add(Consume(TokenType.IDENTIFIER, "Expect parameter name."));
                }
                while (Match(TokenType.COMMA));
            }
            Consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");

            if (Match(TokenType.COLON))
            {
                if (Match(TokenType.NUMBER_TYPE, TokenType.STRING_TYPE, TokenType.BOOLEAN_TYPE))
                {
                    returnType = Previous();
                }
            }

            // Body
            // Consume(TokenType.BEGIN, $"Expect 'Begin' before {kind} body.");
            List<Stmt> body = Block();

            return new Stmt.Function(name, parameterTypes, parameters, returnType, body);
        }

        /// <summary>
        /// Parse a variable declaration
        /// </summary>
        /// <returns></returns>
        private Stmt VarDeclaration()
        {
            var varToken = Previous();
            if (!Enum.TryParse(varToken.Lexeme, out VarType varType))
            {
                Error(varToken, $"Var declaration type '{varType}' is not valid.");
            }

            Token name = Consume(TokenType.IDENTIFIER, "Expect variable name.");

            // If there is a equals, the variable is initalized
            Expr initializer = null;
            if (Match(TokenType.EQUAL))
            {
                initializer = Expression();
            }

            Consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
            return new Stmt.VarDeclaration(name, initializer, varType);
        }

        /// <summary>
        /// Parse a statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt Statement(bool beginBlock)
        {
            if (beginBlock)
                return new Stmt.Block(Block());

            if (Match(TokenType.BREAK)) return BreakStatement();
            if (Match(TokenType.CONTINUE)) return ContinueStatement();
            if (Match(TokenType.DO)) return DoStatement();
            if (Match(TokenType.IF)) return IfStatement();
            if (Match(TokenType.PRINT)) return PrintStatement();
            if (Match(TokenType.RETURN)) return ReturnStatement();
            if (Match(TokenType.WHILE)) return WhileStatement();
            if (Match(TokenType.BEGIN)) return new Stmt.Block(Block());
            if (Match(TokenType.SET)) return SetStatement();
            if (Match(TokenType.CALL)) return ExpressionStatement();

            return ExpressionStatement();
        }

        private Stmt ContinueStatement()
        {
            if (_loop_depth == 0)
            {
                Error(Previous(), "Cannot use 'continue' outside of a loop.");
            }

            Consume(TokenType.SEMICOLON, "Expect ';' after continue.");

            return new Stmt.Continue();
        }

        private Stmt BreakStatement()
        {
            if (_loop_depth == 0)
            {
                Error(Previous(), "Cannot use 'continue' outside of a loop.");
            }

            Consume(TokenType.SEMICOLON, "Expect ';' after break.");

            return new Stmt.Break();
        }

        /// <summary>
        /// Parse a return statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt ReturnStatement()
        {
            Token keyword = Previous();

            // If not return value is set, make it nil
            Expr value = null;
            if (!Check(TokenType.SEMICOLON))
            {
                value = Expression();
            }

            Consume(TokenType.SEMICOLON, "Expect ';' after return value.");
            return new Stmt.Return(keyword, value);
        }

        private Stmt DoStatement()
        {
            _loop_depth++;

            try
            {

                // Body must be a block
                //Consume(TokenType.BEGIN, "Expect 'Begin' after do.");
                List<Stmt> body = DoBlock();

                // While
                Consume(TokenType.WHILE, "Expect 'While' after do loop body.");

                // Condition
                Consume(TokenType.LEFT_PAREN, "Expect '(' after 'while.");
                Expr condition = Expression();
                Consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.");
                // Consume(TokenType.SEMICOLON, "Expect ';' after while condition.");


                // Convert to while loop
                // while (true) {
                //      ...body...
                //      if (condition) {
                //      } else {
                //          break;
                //      }
                // } 

                body.Add(new Stmt.If(condition, new Stmt.Block(new Stmt[]{ }), new Stmt.Break()));

                return new Stmt.While(new Expr.Literal(true), new Stmt.Block(body));
            }
            finally
            {
                _loop_depth--;
            }
        }

        /// <summary>
        /// Parse while loop
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt WhileStatement()
        {
            _loop_depth++;

            try
            {
                Consume(TokenType.LEFT_PAREN, "Expect '(' after 'while.");
                Expr condition = Expression();
                Consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.");
                Stmt body = Statement(true);

                return new Stmt.While(condition, body);
            }
            finally
            {
                _loop_depth--;
            }
        }


        /// <summary>
        /// Parse if statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt IfStatement()
        {
            Consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
            Expr condition = Expression();
            Consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.");

            Stmt then_branch = Statement(true);
            Stmt else_branch = null;

            // Do we have an else?
            if (Match(TokenType.ELSE))
            {
                else_branch = Statement(true);
            }

            return new Stmt.If(condition, then_branch, else_branch);
        }

        /// <summary>
        /// Parse a block of statementts
        /// </summary>
        /// <returns></returns>
        private List<Stmt> Block()
        {
            List<Stmt> statements = new List<Stmt>();

            while (!Check(TokenType.END) && !IsAtEnd())
            {
                statements.Add(Declaration());
            }

            Consume(TokenType.END, "Expect 'End' after block.");
            return statements;
        }

        private List<Stmt> DoBlock()
        {
            List<Stmt> statements = new List<Stmt>();

            while (!Check(TokenType.WHILE) && !IsAtEnd())
            {
                statements.Add(Declaration());
            }

            if (!Check(TokenType.WHILE))
            {
                Error(Peek(), "Expect 'While' after 'Do' block.");
            }
            return statements;
        }


        /// <summary>
        /// Parse a print statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt PrintStatement()
        {
            Expr value = Expression();
            Consume(TokenType.SEMICOLON, "Expect ';' after value");
            return new Stmt.Print(value);
        }

        /// <summary>
        /// Parse an expression statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt ExpressionStatement()
        {
            Expr expr = Expression();
            Consume(TokenType.SEMICOLON, "Expect ';' after expression.");
            return new Stmt.ExpressionStatement(expr);
        }

        private Stmt SetStatement()
        {
            Expr expr = Expression();
            Consume(TokenType.SEMICOLON, "Expect ';' after expression.");
            return new Stmt.ExpressionStatement(expr);
        }

        /// <summary>
        /// Parse an expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Expression()
        {
            return Assignment();
        }

        /// <summary>
        /// Parse an assignment
        /// </summary>
        /// <returns></returns>
        private Expr Assignment()
        {
            Expr expr = Or();

            if (Match(TokenType.EQUAL))
            {
                Token equals = Previous();
                Expr value = Assignment();

                // We have found a assignment target
                // Make sure its a variable
                if (expr is Expr.Variable)
                {
                    Token name = ((Expr.Variable)expr).Name;
                    return new Expr.Assign(name, value);
                }
                else if (expr is Expr.Get)
                {
                    Expr.Get get = (Expr.Get)expr;
                    return new Expr.Set(get.Object, get.Name, value);
                }

                Error(equals, "Invalid assignment target.");
            }

            return expr;

        }

        /// <summary>
        /// Parse an OR expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Or()
        {
            Expr expr = And();

            while (Match(TokenType.OR))
            {
                Token op = Previous();
                Expr right = And();
                expr = new Expr.Logical(expr, op, right);
            }

            return expr;
        }

        /// <summary>
        /// Parse an AND expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr And()
        {
            Expr expr = Equality();

            while (Match(TokenType.AND))
            {
                Token op = Previous();
                Expr right = Equality();
                expr = new Expr.Logical(expr, op, right);
            }

            return expr;
        }

        /// <summary>
        /// Parse a equality expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Equality()
        {
            Expr expr = Comparison();

            while (Match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))
            {
                Token op = Previous();
                Expr right = Comparison();
                expr = new Expr.Binary(expr, op, right);
            }

            return expr;

        }

        /// <summary>
        /// Parse a comparison expression
        /// </summary>
        /// <returns>The epxression</returns>
        private Expr Comparison()
        {
            Expr expr = Addition();

            while (Match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL))
            {
                Token op = Previous();
                Expr right = Addition();
                expr = new Expr.Binary(expr, op, right);
            }

            return expr;
        }

        /// <summary>
        /// Parse an addition/subtraction expression
        /// </summary>
        /// <returns></returns>
        private Expr Addition()
        {
            Expr expr = Multiplication();

            while (Match(TokenType.MINUS, TokenType.PLUS))
            {
                Token op = Previous();
                Expr right = Multiplication();
                expr = new Expr.Binary(expr, op, right);
            }

            return expr;

        }

        /// <summary>
        /// Parse a mulitplication/division expression
        /// </summary>
        /// <returns></returns>
        private Expr Multiplication()
        {
            Expr expr = Unary();

            while (Match(TokenType.SLASH, TokenType.STAR))
            {
                Token op = Previous();
                Expr right = Unary();
                expr = new Expr.Binary(expr, op, right);
            }

            return expr;

        }

        /// <summary>
        /// Parse a unary expression
        /// </summary>
        /// <returns></returns>
        private Expr Unary()
        {
            if (Match(TokenType.BANG, TokenType.MINUS))
            {
                Token op = Previous();
                Expr right = Unary();
                return new Expr.Unary(op, right);
            }

            return Call();

        }

        /// <summary>
        /// Parse a function call
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Call()
        {
            Expr expr = Primary();

            while (true)
            {
                if (Match(TokenType.LEFT_PAREN))
                {
                    // Function call
                    expr = FinishCall(expr);
                }
                else if (Match(TokenType.DOT))
                {
                    // Property
                    Token name = Consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
                    expr = new Expr.Get(expr, name);
                }
                else
                {
                    break;
                }
            }

            return expr;
        }

        /// <summary>
        /// Helper for calling functions
        /// </summary>
        /// <param name="callee">The callee</param>
        /// <returns>The expression</returns>
        private Expr FinishCall(Expr callee)
        {
            Dictionary<Token, Expr> arguments = new Dictionary<Token, Expr>();

            // Find any arguments
            if (!Check(TokenType.RIGHT_PAREN))
            {
                do
                {
                    if (arguments.Count() >= 8)
                    {
                        Error(Peek(), "Cannot have more than 8 arguments");
                    }

                    var argumentName = Consume(TokenType.IDENTIFIER, "Expect argument name");
                    Consume(TokenType.COLON, "Expect ':' after argument name");

                    // Todo: Handle name conflicts
                    arguments.Add(argumentName, Expression());
                }
                while (Match(TokenType.COMMA));
            }

            Token paren = Consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");

            return new Expr.Call(callee, paren, arguments);

        }

        /// <summary>
        /// Parse the primary/literal expression
        /// </summary>
        /// <returns>The literal expression</returns>
        private Expr Primary()
        {
            if (Match(TokenType.FALSE)) return new Expr.Literal(false);
            if (Match(TokenType.TRUE)) return new Expr.Literal(true);
            if (Match(TokenType.NIL)) return new Expr.Literal(null);

            if (Match(TokenType.NUMBER, TokenType.STRING))
            {
                return new Expr.Literal(Previous().Literal);
            }

            if (Match(TokenType.SUPER))
            {
                Token keyword = Previous();
                Consume(TokenType.DOT, "Expect '.' after 'super'.");
                Token method = Consume(TokenType.IDENTIFIER, "Expect superclass method name.");

                return new Expr.Super(keyword, method);

            }


            if (Match(TokenType.THIS)) return new Expr.This(Previous());

            if (Match(TokenType.IDENTIFIER))
            {
                return new Expr.Variable(Previous());
            }

            if (Match(TokenType.LEFT_PAREN))
            {
                Expr expr = Expression();
                Consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
                return new Expr.Grouping(expr);
            }

            // Unknown token
            throw Error(Peek(), "Expect expression.");
        }

        /// <summary>
        /// Consume a token checking it is of the correct type.  Throw an error if not
        /// </summary>
        /// <param name="type">The expected token type</param>
        /// <param name="message">The error message</param>
        /// <returns>The token</returns>
        private Token Consume(TokenType type, string message)
        {
            if (Check(type))
            {
                return Advance();
            }

            throw Error(Peek(), message);
        }


        /// <summary>
        /// Create a new error exception, and log
        /// </summary>
        /// <param name="token">The token</param>
        /// <param name="message">The error message</param>
        private ParseErrorException Error(Token token, string message)
        {
            _error_handler.Error(token, message);
            return new ParseErrorException();
        }

        /// <summary>
        /// Check if the current token matches one of a set of types, and comsume it if it does
        /// </summary>
        /// <param name="types">The tpyes to match</param>
        /// <returns>True if matched</returns>
        private bool Match(params TokenType[] types)
        {
            foreach (TokenType type in types)
            {
                if (Check(type))
                {
                    Advance();
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Check if the current token matches a type
        /// </summary>
        /// <param name="type">The type to check</param>
        /// <returns>True if the token is the given type</returns>
        private bool Check(TokenType type)
        {
            if (IsAtEnd())
            {
                return false;
            }
            return Peek().Type == type;
        }

        /// <summary>
        /// Consume and return the next token
        /// </summary>
        /// <returns>The current token</returns>
        private Token Advance()
        {
            if (!IsAtEnd())
            {
                _current++;
            }

            return Previous();
        }

        /// <summary>
        /// Check if we have reached the EOF token
        /// </summary>
        /// <returns>True if th next token is EOF</returns>
        private bool IsAtEnd()
        {
            return Peek().Type == TokenType.EOF;
        }

        /// <summary>
        /// Peek the next token without consuming it
        /// </summary>
        /// <returns>The next token</returns>
        private Token Peek()
        {
            return _tokens[_current];
        }

        /// <summary>
        /// The last consumed token
        /// </summary>
        /// <returns>The current token</returns>
        private Token Previous()
        {
            return _tokens[_current - 1];
        }

        /// <summary>
        /// Resyncronise the parser state after a syntax error
        /// </summary>
        private void Synchronise()
        {
            Advance();

            while (!IsAtEnd())
            {

                // We can resync if we are at a semicolon
                if (Previous().Type == TokenType.SEMICOLON) return;

                // Or the next token starts a statement or declaration
                switch (Peek().Type)
                {
                    case TokenType.CLASS:
                    case TokenType.FUNCTION:
                    case TokenType.NUMBER_TYPE:
                    case TokenType.STRING_TYPE:
                    case TokenType.BOOLEAN_TYPE:
                    case TokenType.IF:
                    case TokenType.WHILE:
                    case TokenType.PRINT:
                    case TokenType.RETURN:
                        return;

                }

                Advance();
            }


        }



        private class ParseErrorException : Exception { }

        private enum FunctionKind
        {
            FUNCTION
        }
    }
}
