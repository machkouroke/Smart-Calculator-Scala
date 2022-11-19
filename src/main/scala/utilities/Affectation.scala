package utilities

class Affectation(var expression: String, var variableSpace: Map[String, Int]) extends Operation {
    private var variableName: String = ""
    private var variableValue: Int = 0

    def init(): Unit =
        this.checker()
        val operand = expression.split("=", 2)
        this.variableName = operand.head
        val computer = new Computation(operand.last.trim, this.variableSpace)
        computer.init()
        this.variableValue = computer.execute()

    def execute(): Map[String, Int] =
        this.variableSpace + (this.variableName.trim -> this.variableValue)

    def checker(): Unit =
        val operand = this.expression.split("=", 2)
        if (!operand.head.trim.matches(s"^$VALID_IDENTIFIER$$"))
            throw new Exception(INVALID_IDENTIFIER)
        else if (!operand.last.trim.matches(s"^$VALID_OPERATION$$"))
            throw new Exception(INVALID_ASSIGNMENT)
        else if (!this.expression.matches(s"^$VALID_IDENTIFIER[\\s]*=[\\s]*$VALID_OPERATION$$"))
            throw new Exception(INVALID_EXPRESSION)
}