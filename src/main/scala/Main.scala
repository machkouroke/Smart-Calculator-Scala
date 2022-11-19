import exceptions.{SyntaxException, UndefinedException}
import sun.tools.jstat.Expression
import utilities.{Affectation, Command, Computation}

import scala.annotation.{tailrec, targetName}
import scala.io.StdIn.*
import java.lang.Exception
import scala.util.matching.Regex


object Main {
    var variables: Map[String, Int] = Map()

    private def typeDetector(expression: String): String =
        expression match
            case x if x.isEmpty => "empty"
            case x if x.startsWith("/") => "command"
            case x if x.contains("=") => "affectation"
            case _ => "computation"


    def evaluator(inputUser: String): String =
        val trimmed_input = inputUser.trim
        val typeOperation: String = typeDetector(trimmed_input)
        try
            typeOperation match
                case "empty" => ""
                case "command" =>
                    val operation = new Command(trimmed_input)
                    operation.init()
                    operation.execute()

                case "affectation" =>
                    val operation = new Affectation(trimmed_input, this.variables)
                    operation.init()
                    this.variables = operation.execute()
                    ""

                case "computation" =>
                    val operation = new Computation(trimmed_input, this.variables)
                    operation.init()
                    operation.execute().toString


        catch
            case ex: SyntaxException =>
                ex.getMessage
            case ex: UndefinedException =>
                ex.getMessage


    @tailrec
    private def calculator(): Unit =
        val inputUser: String = readLine()
        val answer: String = evaluator(inputUser)
        answer match
            case x if x == "Bye!" =>
                println(x)
                sys.exit()

            case x =>
                if (x.nonEmpty)
                    println(x)
                calculator()


    def main(args: Array[String]): Unit =
        calculator()
}