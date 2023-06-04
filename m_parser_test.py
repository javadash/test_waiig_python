from typing import cast, Union, Any
from collections import namedtuple
import m_ast as ast
import unittest
from m_parser import MParser
from lexer import Lexer, Token, TokenType

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
            Case("let foobar = y;", "foobar", "y")
            #Case("let foobar = y", "foobar", "y")
            ]
    
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
        # bool check must preceed int check or bool is matched as int.
        if isinstance(expected, bool):
            self._test_boolean_literal(expr, cast(bool, expected))
        elif isinstance(expected, int):
            self._test_integer_literal(expr, cast(int, expected))
        elif isinstance(expected, str):
            self._test_identifier(expr, cast(str, expected))
        else:
            self.fail(f"type of expr not handled. Got {type(expected)}")

    def _test_identifier(self, expr: ast.Expression, value: str) -> None:
        self.assertIsInstance(expr, ast.Identifier)
        identifier = cast(ast.Identifier, expr)
        self.assertEqual(identifier.value, value)
        self.assertEqual(identifier.token.literal, value)


    def test_return_statements(self) -> None:
        Case = namedtuple("Case", ["source", "expected_value"])
        tests = [
            Case("return 5;", 5),
            Case("return 10;", 10),
            Case("return 993322;", 993322),
            Case("return 5;", 5),
            Case("return true;", True),
            Case("return foobar;", "foobar")
            ]

        for test in tests:
            program = self._setup_program(test.source)
            self.assertEqual(len(program.statements), 1)
            return_stmt = cast(ast.ReturnStatement, program.statements[0])
            self.assertIsInstance(return_stmt, ast.ReturnStatement)
            self.assertEqual(return_stmt.token_literal(), "return")
            self._test_literal_expression(
                return_stmt.return_value, test.expected_value)

    def test_program_tostring(self) -> None:
        stmt = ast.LetStatement(
        Token(TokenType.LET, "let"), 
        ast.Identifier(Token(TokenType.IDENT, "myVar"), "myVar"), 
        ast.Identifier(Token(TokenType.IDENT, "anotherVar"), "anotherVar")
        )
        aProg = ast.Program([stmt])

        self.assertEqual(aProg.string(), "let myVar = anotherVar;" )
    
    def test_identifier_expression(self) -> None:
        source = "foobar"
        program = self._setup_program(source)
        self.assertEqual(len(program.statements), 1)
        stmt = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        expr_stmt = cast(ast.ExpressionStatement, stmt)
        self.assertIsInstance(expr_stmt.expression, ast.Identifier)
        ident = cast(ast.Identifier, expr_stmt.expression)
        self.assertEqual(ident.value, "foobar")
        self.assertEqual(ident.token.literal, "foobar")
    
    def test_integer_literal_expression(self) -> None:
        source = "5"
        program = self._setup_program(source)
        self.assertEqual(len(program.statements), 1)
        stmt = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        expr_stmt = cast(ast.ExpressionStatement, stmt)
        self.assertIsInstance(expr_stmt.expression, ast.IntegerLiteral)
        literal = cast(ast.IntegerLiteral, expr_stmt.expression)
        self.assertEqual(literal.value, 5)
        self.assertEqual(literal.token.literal, "5")

    def test_parsing_prefix_expressions(self) -> None:
        Case = namedtuple("Case", ["source", "operator", "value"])
        tests = [Case("!5;", "!", 5),
                 Case("-15;", "-", 15),]
        
        for test in tests:
            program = self._setup_program(test.source)
            self.assertEqual(len(program.statements), 1)
            stmt = program.statements[0]
            self.assertIsInstance(stmt, ast.ExpressionStatement)
            expr_stmt = cast(ast.ExpressionStatement, stmt)
            self.assertIsInstance(expr_stmt.expression, ast.PrefixExpression)
            expr = cast(ast.PrefixExpression, expr_stmt.expression)
            self.assertEqual(expr.operator, test.operator)
            
            #self._test_literal_expression(expr.right, test.value)

            self._test_integer_literal(expr.right, test.value)

    def _test_integer_literal(self, expr: ast.Expression, value: int) -> None:
        self.assertIsInstance(expr, ast.IntegerLiteral)
        integer = cast(ast.IntegerLiteral, expr)
        self.assertEqual(integer.value, value)
        self.assertEqual(integer.token.literal, str(value))


    
    def test_parsing_infix_expressions(self) -> None:
        Case = namedtuple(
            "Case", ["source", "left_value", "operator", "right_value"])
        tests = [
            Case("5 + 5;", 5, "+", 5),
            Case("5 - 5;", 5, "-", 5),
            Case("5 * 5;", 5, "*", 5),
            Case("5 / 5;", 5, "/", 5),
            Case("5 > 5;", 5, ">", 5),
            Case("5 < 5;", 5, "<", 5),
            Case("5 == 5;", 5, "==", 5),
            Case("5 != 5;", 5, "!=", 5)
            # Case("true == true", True, "==", True),
            # Case("true != false", True, "!=", False),
            # Case("false == false", False, "==", False)
            ]

        for test in tests:
            program = self._setup_program(test.source)
            self.assertEqual(len(program.statements), 1)
            stmt = program.statements[0]
            self.assertIsInstance(stmt, ast.ExpressionStatement)
            expr_stmt = cast(ast.ExpressionStatement, program.statements[0])
            expr = expr_stmt.expression
            self._test_infix_expression(
                expr, test.left_value, test.operator, test.right_value)
        
    def _test_infix_expression(self, expr: ast.Expression, left: Any,
                            operator: str, right: Any) -> None:
        self.assertIsInstance(expr, ast.InfixExpression)
        self._test_literal_expression(
            cast(ast.InfixExpression, expr).left, left)
        self.assertEqual(cast(ast.InfixExpression, expr).operator, operator)
        self._test_literal_expression(
            cast(ast.InfixExpression, expr).right, right)
    
    def test_operator_precedence_parsing(self) -> None:
        Case = namedtuple("Case", ["source", "expected"])
        tests = [
            Case("-a * b", "((-a) * b)"),
            Case("!-a", "(!(-a))"),
            Case("a + b + c", "((a + b) + c)"),
            Case("a + b - c", "((a + b) - c)"),
            Case("a * b * c", "((a * b) * c)"),
            Case("a * b / c", "((a * b) / c)"),
            Case("a + b / c", "(a + (b / c))"),
            Case("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            Case("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            Case("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            Case("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            Case("3 + 4 * 5 == 3 * 1 + 4 * 5",
                 "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            Case("3 + 4 * 5 == 3 * 1 + 4 * 5",
                 "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            Case("true", "true"),
            Case("false", "false"),
            Case("3 > 5 == false", "((3 > 5) == false)"),
            Case("3 < 5 == true", "((3 < 5) == true)"),
            # Case("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            # Case("(5 + 5) * 2", "((5 + 5) * 2)"),
            # Case("2 / (5 + 5)", "(2 / (5 + 5))"),
            # Case("-(5 + 5)", "(-(5 + 5))"),
            # Case("!(true == true)", "(!(true == true))"),
            # Case("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            # Case("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            #      "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            # Case("add(a + b + c * d / f + g)",
            #      "add((((a + b) + ((c * d) / f)) + g))"),
            # Case("a * [1, 2, 3, 4][b * c] * d",
            #      "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            # Case("add(a * b[2], b[1], 2 * [1, 2][1])",
            #      "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))")
                 
                 ]

        for test in tests:
            program = self._setup_program(test.source)
            # cannot assert statement eq 1 as one test
            # ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)") consists of two statements.
            # self.assertEqual(len(program.statements), 1)
            actual = program.string()
            self.assertEqual(actual, test.expected)

    def _test_boolean_literal(self, expr: ast.Expression, value: bool) -> None:
        self.assertIsInstance(expr, ast.Boolean)
        boolean = cast(ast.Boolean, expr)
        self.assertEqual(boolean.value, value)
        # Monkey and Python boolean literals differ
        if value:
            self.assertEqual(expr.token_literal(), "true")
        else:
            self.assertEqual(expr.token_literal(), "false")
    
    def _test_identifier(self, expr: ast.Expression, value: str) -> None:
        self.assertIsInstance(expr, ast.Identifier)
        identifier = cast(ast.Identifier, expr)
        self.assertEqual(identifier.value, value)
        self.assertEqual(identifier.token.literal, value)