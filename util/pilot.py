# PILOT LANGUAGE INTERPRETER
# A simple interpreter for PILOT (Programmed Instruction, Learning, Or
# Teaching), a simple programming language developed in 1968 for creating
# interactive classroom learning activities.
#
# Copyright (C) 2019 Andrew Smith
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from pyparsing import (CaselessLiteral, Combine, Forward, Group, Keyword,
    Literal, OneOrMore, Optional, Word, ZeroOrMore, alphas, alphanums,
    infixNotation, opAssoc)
from typing import Any, List

################################################################################
# ATOM DEFINITIONS
################################################################################
# General numeric information and forward declarations
NUMBER_SIGN = Word('+-', exact=1)
ARITHMETIC_EXPRESSION = Forward()

# Integer types
def integer_type(sigil: str, digits: Word, base: int) -> Combine:
    """
    Create a new integer type that denotes an integer in the specified base
    beginning with the specified sigil and containing only the specified digits.

    :param sigil: The sigil with which this integer type should begin.
    :param digits: A Word representing the digits permitted in integer literals
                   of this type.
    :param base: The base as which this integer type should be interpreted.
    :returns: A ParserElement that converts integer literals of this type to
              Python ints.
    """
    return Combine(CaselessLiteral(sigil) + digits).setParseAction(
        # Strip the sigil and underscores from the integer literal
        lambda toks: int(toks[0][len(sigil):].replace('_', ''), base)
    )

# Binary (base-2)
BINARY_DIGITS = Word('01_')
BINARY_INTEGER = integer_type('&b', BINARY_DIGITS, 2)
# Octal (base-8)
OCTAL_DIGITS = Word('01234567_')
OCTAL_INTEGER = integer_type('&o', OCTAL_DIGITS, 8)
# Decimal (base-10)
DECIMAL_INTEGER = Combine(Word('123456789', '0123456789_')).setParseAction(
    # Strip underscores from integer literal
    lambda toks: toks[0].replace('_', '')
)
# Hexadecimal (base-16)
HEXADECIMAL_DIGITS = Word('0123456789abcdefABCDEF_')
HEXADECIMAL_INTEGER = integer_type('&h', HEXADECIMAL_DIGITS, 16)
# Integers as a whole
INTEGER_TYPE = (BINARY_INTEGER | OCTAL_INTEGER | DECIMAL_INTEGER
    | HEXADECIMAL_INTEGER)
INTEGER_LITERAL = Combine(Optional(NUMBER_SIGN) + INTEGER_TYPE).setParseAction(
    lambda toks: int(toks[0])
)

# Floating-point
FLOAT_DIGITS = Word('0123456789_')
FLOAT_LITERAL = Combine(Optional(NUMBER_SIGN) + FLOAT_DIGITS + '.'
    + Optional(FLOAT_DIGITS)).setParseAction(
        # Remove underscores from float literal
        lambda toks: float(toks[0].replace('_', ''))
    )

NUMERIC_LITERAL = FLOAT_LITERAL | INTEGER_LITERAL

# Identifiers
INTEGER_SIGIL = Literal('#')
FLOAT_SIGIL = Literal('%')
STRING_SIGIL = Literal('$')
IDENTIFIER_SIGIL = INTEGER_SIGIL | FLOAT_SIGIL | STRING_SIGIL
IDENTIFIER_NAME = Word(alphas, alphanums + '_')

IDENTIFIER = (Combine(OneOrMore(IDENTIFIER_SIGIL) + IDENTIFIER_NAME)
    .setParseAction(lambda toks: {
        'operation': 'LOAD',
        'id': toks[0]
    }))

# Arrays
ARRAY_IDENTIFIER = Forward()
ARRAY_IDENTIFIER <<= Group(IDENTIFIER + '(' + ARITHMETIC_EXPRESSION
    + ZeroOrMore(',' + ARITHMETIC_EXPRESSION) + ')').setParseAction(
        # Separate variable and indices
        lambda toks: {
            'operation': 'ARRAY_LOAD',
            'array_id': toks[0][0],
            'indices': toks[0][2::2]
        })

# Arithmetic operators
def process_unary_operator(values: List[List[Any]]) -> dict:
    """
    Parse the specified unary operator, the result of a ParseExpression, and
    return an operation corresponding to its function.
    """
    operator, operand = values[0]
    # Ignore unary plus
    if operator == '+':
        return operand
    # Negate result for unary minus
    return {
        'operation': 'NEGATE',
        'value': operand
    }

OP_UNARY_PLUS = Literal('+')
OP_UNARY_MINUS = Literal('-')
OP_UNARY_PRECEDENCE = OP_UNARY_PLUS | OP_UNARY_MINUS

OPERATOR_NAMES = {
    '+': 'ADD',
    '-': 'SUBTRACT',
    '*': 'MULTIPLY',
    '/': 'DIVIDE',
    '\\': 'MODULO',
    '^': 'EXPONENT'
}

def process_binary_operator(values: List[List[Any]]) -> dict:
    """
    Parse the specified binary operator, the result of a ParseExpression, and
    return an operation corresponding to its function.
    """
    # Separate operations and validate
    left_operand, operator, right_operand = values[0]
    if operator not in OPERATOR_NAMES:
        # TODO Create syntax error class
        raise ValueError(f'Invalid binary operator in expression: {operator}')

    return {
        'operation': OPERATOR_NAMES[operator],
        'left': left_operand,
        'right': right_operand
    }

OP_ADD = Literal('+')
OP_SUBTRACT = Literal('-')
OP_ADDITIVE_PRECEDENCE = OP_ADD | OP_SUBTRACT

OP_MULTIPLY = Literal('*')
OP_DIVIDE = Literal('/')
OP_MODULO = Literal('\\')
OP_MULTIPLICATIVE_PRECEDENCE = OP_MULTIPLY | OP_DIVIDE | OP_MODULO

OP_EXPONENT = Literal('^')

# Arithmetic expressions
ARITHMETIC_ATOM = NUMERIC_LITERAL | ARRAY_IDENTIFIER | IDENTIFIER
ARITHMETIC_EXPRESSION <<= infixNotation(ARITHMETIC_ATOM, [
    (OP_UNARY_PRECEDENCE, 1, opAssoc.RIGHT, process_unary_operator),
    (OP_EXPONENT, 2, opAssoc.LEFT, process_binary_operator),
    (OP_MULTIPLICATIVE_PRECEDENCE, 2, opAssoc.LEFT, process_binary_operator),
    (OP_ADDITIVE_PRECEDENCE, 2, opAssoc.LEFT, process_binary_operator)
])
