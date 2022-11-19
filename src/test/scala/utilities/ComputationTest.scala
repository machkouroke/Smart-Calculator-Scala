package utilities
import org.scalatest.funsuite.AnyFunSuite
import utilities.Computation
class ComputationTest extends AnyFunSuite :
    test("postfixToInfix") {
        val answer = Map("3 + 2 * 4" -> "3 2 4 * +",
            "2 * (3 + 4) + 1" -> "2 3 4 + * 1 +",
            "8 * 3 + 12 * (4 - 2)" -> "8 3 * 12 4 2 - * +",
            "2 * 2 ^ 3" -> "2 2 3 ^ *")
        for {
            key <- answer.keys
        } {
            val computer = new Computation(key, Map())
            computer.init()
            val postfix = computer.infixToPostfix(computer.operand)
            assert(postfix == answer(key).split("\\s+").toList)
        }

    }
    test("postfixToAnswer") {
        val answer = Map(
            "8 * 3 + 12 * (4 - 2)" -> "48",
            "2 * 2 ^ 3" -> "16")
        for {
            key <- answer.keys
        } {
            val computer = new Computation(key, Map())
            computer.init()
            val postfix = computer.infixToPostfix(computer.operand)
            val result = computer.eval(postfix)
            assert(result == answer(key).toInt)
        }

    }