import unittest
from collections import namedtuple
from typing import Dict, cast
from m_parser import MParser
from lexer import Lexer
import monkey_object
import environment
from evaluator import Evaluator

Case = namedtuple("Case", ["source", "expected"])


class EvaluatorTest(unittest.TestCase):
    def test_eval_integer_expression(self) -> None:
        tests = [
            Case("5", 5),
            Case("10", 10),
            Case("-5", -5),
            Case("-10", -10),
            Case("5 + 5 + 5 + 5 - 10", 10),
            Case("2 * 2 * 2 * 2 * 2", 32),
            Case("-50 + 100 + -50", 0),
            Case("5 * 2 + 10", 20),
            Case("5 + 2 * 10", 25),
            Case("20 + 2 * -10", 0),
            Case("50 / 2 * 2 + 10", 60),
            Case("2 * (5 + 10)", 30),
            Case("3 * 3 * 3 + 10", 37),
            Case("3 * (3 * 3) + 10", 37),
            Case("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
        ]
        for test in tests:
            evaluated = self._test_eval(test.source)
            self._test_integer_object(evaluated, test.expected)

    def _test_eval(self, source: str) -> monkey_object.MonkeyObject:
        lexer = Lexer(source)
        parser = MParser(lexer)
        evaluator = Evaluator()
        program = parser.parse_program()
        env = environment.Environment()
        evaluated = evaluator.eval(program, env)
        assert evaluated is not None
        return evaluated

    def _test_integer_object(self, obj: monkey_object.MonkeyObject, expected: int) -> None:
        self.assertIsInstance(obj, monkey_object.Integer)
        self.assertEqual(cast(monkey_object.Integer, obj).value, expected)

    def test_eval_boolean_expression(self) -> None:
        tests = [
            Case("true", True),
            Case("false", False),
            Case("1 < 2", True),
            Case("1 > 2", False),
            Case("1 < 1", False),
            Case("1 > 1", False),
            Case("1 == 1", True),
            Case("1 != 1", False),
            Case("1 == 2", False),
            Case("1 != 2", True),
            Case("true == true", True),
            Case("false == false", True),
            Case("true == false", False),
            Case("true != false", True),
            Case("false != true", True),
            Case("(1 < 2) == true", True),
            Case("(1 < 2) == false", False),
            Case("(1 > 2) == true", False),
            Case("(1 > 2) == false", True)
        ]
        for test in tests:
            evaluated = cast(monkey_object.Boolean,
                             self._test_eval(test.source))
            self._test_boolean_object(evaluated, test.expected)

    def _test_boolean_object(self, obj: monkey_object.Boolean, expected: bool) -> None:
        self.assertIsInstance(obj, monkey_object.Boolean)
        self.assertEqual(obj.value, expected)

    def test_bang_operator(self) -> None:
        tests = [
            Case("!true", False),
            Case("!false", True),
            Case("!5", False),
            Case("!!true", True),
            Case("!!false", False),
            Case("!!5", True)]
        for test in tests:
            evaluated = cast(monkey_object.Boolean,
                             self._test_eval(test.source))
            self._test_boolean_object(evaluated, test.expected)

    def test_if_else_expression(self) -> None:
        tests = [
            Case("if (true) { 10 }", 10),
            Case("if (false) { 10 }", None),
            Case("if (1) { 10 }", 10),
            Case("if (1 < 2) { 10 }", 10),
            Case("if (1 > 2) { 10 }", None)]
        for test in tests:
            evaluated = self._test_eval(test.source)
            if isinstance(evaluated, monkey_object.Integer):
                self._test_integer_object(evaluated, test.expected)
            else:
                self._test_null_object(evaluated)

    def _test_null_object(self, obj: monkey_object.MonkeyObject) -> None:
        self.assertEqual(obj, Evaluator.null)

    def test_return_statement(self) -> None:
        tests = [
            Case("return 10;", 10),
            Case("return 10; 9;", 10),
            Case("return 2 * 5; 9;", 10),
            Case("9; return 2 * 5; 9;", 10),
            Case("""if (10 > 1) {
                      if (10 > 1) {
                        return 10;
                      }
                      return 1;
                    }""", 10),
            Case("""if (10 > 1) {
                     if (10 > 1) {
                       return 10;
                     }
                     return 1;
                   }""", 10),
            # Case("""let f = fn(x) {
            #          return x;
            #          x + 10;
            #        };
            #        f(10);""", 10),
            # Case("""let f = fn(x) {
            #          let result = x + 10;
            #          return result;
            #          return 10;
            #        };
            #        f(10);""", 20)
        ]
        for test in tests:
            evaluated = self._test_eval(test.source)
            self._test_integer_object(evaluated, test.expected)

    def test_error_handling(self) -> None:
        tests = [
            Case("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            Case("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            Case("-true", "unknown operator: -BOOLEAN"),
            Case("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            Case("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            Case("if (10 > 1) { true + false; }",
                 "unknown operator: BOOLEAN + BOOLEAN"),
            Case("""if (10 > 1) {
                      if (10 > 1) {
                        return true + false;
                      }
                      return 1;
                    }""", "unknown operator: BOOLEAN + BOOLEAN"),
            Case("foobar", "identifier not found: foobar"),
            # Case('"Hello" - "World"', "unknown operator: STRING - STRING"),
            # Case('{"name": "Monkey"}[fn(x) { x }];', "unusable as hash key: FUNCTION")
            ]
        for test in tests:
            evaluated = cast(monkey_object.Error, self._test_eval(test.source))
            self.assertIsInstance(evaluated, monkey_object.Error)
            self.assertEqual(evaluated.message, test.expected)
        
    def test_let_statements(self) -> None:
        tests = [
            Case("let a = 5; a;", 5),
            Case("let a = 5 * 5; a;", 25),
            Case("let a = 5; let b = a; b;", 5),
            Case("let a = 5; let b = a; let c = a + b + 5; c;", 15)]
        for test in tests:
            self._test_integer_object(
                self._test_eval(test.source), test.expected)
            
    def test_function_object(self) -> None:
        source = "fn(x) { x + 2; };"
        evaluated = cast(monkey_object.Function, self._test_eval(source))
        self.assertIsInstance(evaluated, monkey_object.Function)
        self.assertEqual(len(evaluated.parameters), 1)
        self.assertEqual(evaluated.parameters[0].string(), "x")
        expected_body = "(x + 2)"
        self.assertEqual(evaluated.body.string(), expected_body)

    def test_function_application(self) -> None:
        tests = [
            Case("let identity = fn(x) { x; }; identity(5);", 5),
            Case("let identity = fn(x) { return x; }; identity(5);", 5),
            Case("let double = fn(x) { x * 2; }; double(5);", 10),
            Case("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            Case("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            Case("fn(x) { x; }(5)", 5)]
        for test in tests:
            self._test_integer_object(
                self._test_eval(test.source), test.expected)
