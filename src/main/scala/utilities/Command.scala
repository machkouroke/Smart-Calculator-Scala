package utilities

class Command(var expression: String) extends Operation {
    private var command: String = ""

    def init(): Unit =
        this.checker()
        this.command = this.expression.tail

    def execute(): String =
        this.expression match
            case "/exit" =>
                "Bye!"
            case "/help" =>
                "The program calculates the sum of numbers"

            case _ =>
                "Unknown Command"


    def checker(): Unit =
        if (!this.expression.matches("^\\/[\\w]+$"))
            throw new Exception(INVALID_EXPRESSION)

}