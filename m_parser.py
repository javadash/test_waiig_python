from enum import Enum, unique
from typing import List, Callable, Dict, Optional, Union
import m_ast as ast
from lexer import Lexer, Token, TokenType


class MParser:
    def __init__(self, lexer: Lexer) -> None:
        self.errors: List[str] = []
        self._lexer = lexer

        # Acts like _position and _peek_char within the lexer, but instead of
        # pointing to characters in the source they point to current and next
        # tokens. We need _current_token, the current token under examination,
        # to decide what to do next, and we need _peekToken to guide the
        # decision in case _current_token doesn't provide us with enough
        # information, e.g., with source "5;", _current_token is Int and we
        # require _peek_token to decide if we're at the end of the line or at
        # the start of an arithmetic expression. This implements a parser with
        # one token lookahead.
        #
        # We initialize both with a dummy token rather than define their type as
        # Optional[Token] which would cause lots of unnecessary changes to
        # satisfy the type checker. In practice, these tokens would only be None
        # between declaration and until the calls to _next_token().
        self._current_token: Token = Token(TokenType.ILLEGAL, "")
        self._peek_token: Token = Token(TokenType.ILLEGAL, "")

        # # Functions based on token type called as part of Pratt parsing.
        # self._prefix_parse_fns: Dict[TokenType, PrefixParseFn] = {}
        # self._infix_parse_fns: Dict[TokenType, InfixParseFn] = {}

        # Read two tokens so _current_token and _peekToken tokens are both set.
        self._next_token()
        self._next_token()

    def _next_token(self) -> None:
        self._current_token = self._peek_token
        self._peek_token = self._lexer.next_token()

    def parse_program(self) -> ast.Program:
        stmts = []
        while not self._current_token_is(TokenType.EOF):
            stmt = self._parse_statement()

            # Many _parse_x() return null which bubbles up the call stack to
            # here where null enables continued parsing on error. The
            # alternative would be for _parse_x() to immediately stop parsing
            # and signal a parser error. But that wouldn't allow collecting
            # multiple errors to improve the user experience. Instead parsing
            # would halt on the first error. As an example, _expect_peek()
            # returns null if the expected token isn't found _and_ adds an error
            # message to the _errors list. That null bubbles up to
            # _parse_statement() which skips adding the statement to the AST and
            # moves on the next statement. Evaluation of the AST shouldn't be
            # attempted in case of parser errors as the faulting parts of the
            # program is missing from the AST, likely resulting in evaluation
            # errors.
            if stmt is not None:
                stmts.append(stmt)
            self._next_token()
        return ast.Program(stmts)

    def _current_token_is(self, type_: TokenType) -> bool:
        if self._current_token is None:
            return False
        return self._current_token.type_ == type_
    
    # def _parse_statement(self) -> Optional[Union[ast.Statement, ast.Expression]]:
    #     if self._current_token.type_ == TokenType.LET:
    #         return self._parse_let_statement()
    #     if self._current_token.type_ == TokenType.RETURN:
    #         return self._parse_return_statement()

    #     # The only two real statement types in Monkey are let and return. If
    #     # none of those got matched, try to parse source as a pseudo
    #     # ExpressionStatement.
    #     return self._parse_expression_statement()
    
    def _parse_statement(self) -> Optional[Union[ast.Statement, ast.Expression]]:
        if self._current_token.type_ == TokenType.LET:
            return self._parse_let_statement()
        return None
    

    def _parse_let_statement(self) -> Optional[ast.LetStatement]:
        token = self._current_token
        if not self._expect_peek(TokenType.IDENT):
            return None
        
        name = ast.Identifier(self._current_token, self._current_token.literal)

        if not self._expect_peek(TokenType.ASSIGN):
            return None
        
        # self._next_token()
        # value = self._parse_expression(PrecedenceLevel.LOWEST)

        # # Type checker would be satisfied if we added "assert value is not None"
        # # following a call to a parser retuning Optional. assert, however,
        # # terminates the program on None when what we want, whenever a
        # # sub-parser such as _parse_expression() returns None, is to bubble up
        # # None to _parse_program() so it can skip adding the statement to the
        # # AST.
        # if value is None:
        #     return None
        
        # if self._peek_token_is(TokenType.SEMICOLON):
        #     self._next_token()

        while not self._current_token.type_ != TokenType.SEMICOLON:
            self._next_token()

        # return ast.LetStatement(token, name, value)
        return ast.LetStatement(token, name, None)

    def _expect_peek(self, type_: TokenType) -> bool:
        if self._peek_token_is(type_):
            self._next_token()
            return True
        self._peek_error(type_)
        return False

    def _peek_token_is(self, type_: TokenType) -> bool:
        if self._peek_token is None:
            return False
        return self._peek_token.type_ == type_
