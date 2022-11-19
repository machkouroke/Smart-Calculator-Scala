import org.scalatest.funsuite.AnyFunSuite


class MainTest extends AnyFunSuite :
    private val UNKNOWN_VARIABLE = "Unknown variable"
    private val INVALID_IDENTIFIER = "Invalid identifier"
    private val INVALID_ASSIGNMENT = "Invalid assignment"

    test("Simple Operation") {
        assert(Main.evaluator("4 + 4") == "8")
        assert(Main.evaluator("+4") == "4")
        assert(Main.evaluator("-5") == "-5")
        assert(Main.evaluator("") == "")
        assert(Main.evaluator("8 + 7 - 4") == "11")
        assert(Main.evaluator("4 ++ 4") == "8")
        assert(Main.evaluator("10 -- 4") == "14")
        assert(Main.evaluator("10 --- 4") == "6")


    }
    test("Invalid Expression") {
        assert(Main.evaluator("123+") == "Invalid expression")
        assert(Main.evaluator("18 22") == "Invalid expression")
        assert(Main.evaluator("18 22") == "Invalid expression")
        assert(Main.evaluator("4 * (2 + 3") == "Invalid expression")
        assert(Main.evaluator("3 *** 5") == "Invalid expression")
        assert(Main.evaluator("4+3)") == "Invalid expression")
    }
    test("Command") {
        assert(Main.evaluator("/help") == "The program calculates the sum of numbers")
        assert(Main.evaluator("/exit") == "Bye!")
        assert(Main.evaluator("/bad") == "Unknown Command")
    }
    test("Affectation") {
        val finalVariable = Map("n" -> 9, "m" -> 4, "a" -> 5, "b" -> 5, "v" -> 7, "count" -> 10, "c" -> 5)
        Main.variables = Map.empty[String, Int]
        assert(Main.evaluator("n = 3") == "")
        assert(Main.evaluator("m=4") == "")
        assert(Main.evaluator("a  =   5") == "")
        assert(Main.evaluator("b = a") == "")
        assert(Main.evaluator("v=   7") == "")
        assert(Main.evaluator("n =9") == "")
        assert(Main.evaluator("c=  a ") == "")
        assert(Main.evaluator("count = 10") == "")
        assert(Main.variables == finalVariable)
    }

    test("Invalid Affectation") {
        Main.variables = Map.empty[String, Int]
        assert(Main.evaluator("b = c") == UNKNOWN_VARIABLE)
        assert(Main.evaluator("b = c") == UNKNOWN_VARIABLE)
        assert(Main.evaluator("e") == UNKNOWN_VARIABLE)
        assert(Main.evaluator("Variable") == UNKNOWN_VARIABLE)

        assert(Main.evaluator("a1 = 8") == INVALID_IDENTIFIER)
        assert(Main.evaluator("n1 = a2a") == INVALID_IDENTIFIER)
        assert(Main.evaluator("n = a2a") == INVALID_ASSIGNMENT)
        assert(Main.evaluator("a = 7 = 8") == INVALID_ASSIGNMENT)
    }

   
    