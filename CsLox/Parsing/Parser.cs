﻿using CsLox.SyntaxTree;
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
        private readonly List<Token> _tokens;
        private int _current = 0;

        /// <summary>
        /// Create a new Parser instance
        /// </summary>
        /// <param name="tokens">The input tokens</param>
        public Parser(List<Token> tokens)
        {
            this._tokens = tokens;
        }

        /// <summary>
        /// Parse the tokens
        /// </summary>
        /// <returns>The parsed statements</returns>
        public List<Stmt> Parse()
        {
            List<Stmt> statements = new List<Stmt>();

            while(!IsAtEnd())
            {
                statements.Add(Statement());
            }

            return statements;

        }

        /// <summary>
        /// Parse a statement
        /// </summary>
        /// <returns>The statement</returns>
        private Stmt Statement()
        {
            if (Match(TokenType.PRINT)) return PrintStatement();

            return ExpressionStatement();
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


        /// <summary>
        /// Parse an expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Expression()
        {
            return Equality();
        }

        /// <summary>
        /// Parse a equality expression
        /// </summary>
        /// <returns>The expression</returns>
        private Expr Equality()
        {
            Expr expr = Comparison();

            while (Match (TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))
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

            return Primary();

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
            CsLox.ParseError(token, message);
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
        /// Consume and return the current token
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
        /// Peek the next token wihout consuming it
        /// </summary>
        /// <returns>The next token</returns>
        private Token Peek()
        {
            return _tokens[_current];
        }

        /// <summary>
        /// Look at the current token
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
                    case TokenType.FUN:
                    case TokenType.VAR:
                    case TokenType.FOR:
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

    }
}