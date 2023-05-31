from typing import cast, Union, Any
from collections import namedtuple
import m_ast as ast
import unittest
from m_parser import MParser
from lexer import Lexer


class ParserTests(unittest.TestCase):
    def _setup_program(self, source: str) -> ast.Program:
        lexer = Lexer(source)
        parser = MParser(lexer)
        program = parser.parse_program()
        self._check_parser_errors(parser)
        return program
    
    def _check_parser_errors(self, parser: MParser) -> None:
        if len(parser.errors) == 0:
            return
        for message in parser.errors:
            print(message)
        self.fail("See stdout")

    def test_let_statements(self) -> None:
        Case = namedtuple(
            "Case", ["source", "expected_identifier", "expected_value"])
        tests = [
            Case("let x = 5;", "x", 5),
            Case("let y = true;", "y", True),
            Case("let foobar = y", "foobar", "y")]
    
        for test in tests:
            program = self._setup_program(test.source)
            self.assertEqual(len(program.statements), 1)
            stmt = cast(ast.LetStatement, program.statements[0])
            self._test_let_statement(stmt, test.expected_identifier)
            self.assertIsInstance(stmt, ast.LetStatement)
            value = stmt.value
            self._test_literal_expression(value, test.expected_value)

    def _test_let_statement(self, stmt: ast.Statement, name: str) -> None:
        self.assertEqual(stmt.token_literal(), "let")
        # Downside of type checking being done by a seperate program is that we
        # must assert the same thing twice: first to satisfy the runtime unit
        # test and second to satisfy mypy.
        self.assertIsInstance(stmt, ast.LetStatement)
        let_stmt = cast(ast.LetStatement, stmt)
        self.assertEqual(let_stmt.name.value, name)
        self.assertEqual(let_stmt.name.token_literal(), name)

    def _test_literal_expression(self, expr: ast.Expression,
                                 expected: Union[int, str, bool]) -> None:
        identifier = cast(ast.Identifier, expr)
        # self.assertEqual(identifier.value, expected)
        # self.assertEqual(identifier.token.literal, expected)

    # def _test_literal_expression(self, expr: ast.Expression,
    #                              expected: Union[int, str, bool]) -> None:
    #     # bool check must preceed int check or bool is matched as int.
    #     if isinstance(expected, bool):
    #         self._test_boolean_literal(expr, cast(bool, expected))
    #     elif isinstance(expected, int):
    #         self._test_integer_literal(expr, cast(int, expected))
    #     elif isinstance(expected, str):
    #         self._test_identifier(expr, cast(str, expected))
    #     else:
    #         self.fail(f"type of expr not handled. Got {type(expected)}")

    def _test_identifier(self, expr: ast.Expression, value: str) -> None:
        self.assertIsInstance(expr, ast.Identifier)
        identifier = cast(ast.Identifier, expr)
        self.assertEqual(identifier.value, value)
        self.assertEqual(identifier.token.literal, value)