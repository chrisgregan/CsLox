using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CsLox.Tokens
{
    public enum TokenType
    {
        LEFT_PAREN, RIGHT_PAREN, BEGIN, END,
        COMMA, DOT, MINUS, PLUS, COLON, SEMICOLON, SLASH, STAR,

        // One or two character tokens.
        BANG, BANG_EQUAL,
        EQUAL, EQUAL_EQUAL,
        GREATER, GREATER_EQUAL,
        LESS, LESS_EQUAL,

        // Literals.
        IDENTIFIER, STRING, NUMBER,

        // Keywords.
        AND, BREAK, CLASS, CONTINUE, DO, ELSE, FALSE, FUNCTION, IF, NIL, OR,
        PRINT, RETURN, SUPER, THIS, TRUE, WHILE, SET, CALL,

        // Types
        NUMBER_TYPE, STRING_TYPE, BOOLEAN_TYPE,

        EOF
    }

    public enum VarType
    {
        Number,
        String,
        Boolean,
        Object,
        Function,
        Var
    }
}
