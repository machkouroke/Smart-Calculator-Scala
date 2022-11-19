package utilities

import exceptions.{SyntaxException, UndefinedException}

class Computation(var expression: String, var variableSpace: Map[String, Int]) extends Operation {
    var operand: List[String] = Nil

    def init(): Unit =
        this.checker()
        this.operand = this.expression.split("\\s+").toList
        this.operand = this.operand.flatMap(x => if (x.matches(".*(\\(|\\)).*")) splitParenthesis(x) else List(x))


    /**
     * Permet de séparer un élément suivant la parenthèse et le nombre
     * Par exemple (10 = List((, 10)
     */
    private def splitParenthesis(x: String): List[String] =
        var expanded: List[String] = List()
        var digitStack: List[String] = List()
        for (char: Char <- x.toList)
            char match
                case x if List('(', ')').contains(x) =>
                    if (digitStack.nonEmpty)
                        expanded :+= digitStack.mkString
                        digitStack = List()

                    expanded :+= char.toString

                case _ =>
                    digitStack :+= char.toString


        if (digitStack.nonEmpty)
            expanded :+= digitStack.mkString
            digitStack = List()
        expanded

    def execute(): Int =
        this.operand |>
          this.variableEval |>
          this.signEval |>
          this.infixToPostfix |>
          this.eval

    private def signEval(expression: List[String]): List[String] =
        val answer: List[String] = for element: String <- expression
            yield element match
                case x if x.matches("^(\\+|-)+$") =>
                    val sign = x.toList.map(letter => s"$letter".toInt).product
                    if (sign > 0) "+" else "-"

                case _ => element

        answer

    private def variableEval(expression: List[String]): List[String] =
        try
            expression.map(x => if (x.matches(VALID_IDENTIFIER.toString())) this.variableSpace(x.trim).toString else x)
        catch
            case _: NoSuchElementException => throw new UndefinedException("Unknown variable")


    def eval(postfixExpression: List[String]): Int =
        var scanner: List[String] = List()
        var answer = 0
        val operation = Map("+" -> ((a: Int, b: Int) => a + b),
            "-" -> ((a: Int, b: Int) => a - b),
            "*" -> ((a: Int, b: Int) => a * b),
            "/" -> ((a: Int, b: Int) => a / b),
            "^" -> ((a: Int, b: Int) => math.pow(a, b).toInt))
        for (value <- postfixExpression)
            value match
                case x if !operation.keys.toSet.contains(x) =>
                    scanner :+= x

                case _ =>
                    val a = scanner.init.last.toInt
                    val b = scanner.last.toInt
                    scanner = scanner.dropRight(2)
                    scanner :+= operation(value)(a, b).toString


        answer = scanner.last.toInt
        answer


    def infixToPostfix(infixExpression: List[String]): List[String] =
        var scanner: List[String] = List()
        var postfixExpression: String = ""
        val priority: Map[String, Int] = Map("+" -> 1, "-" -> 1, "*" -> 2, "/" -> 2, "^" -> 3, "(" -> 0, ")" -> 0)
        for (char: String <- infixExpression)
            char match
                /* Si le charactère est une opérande on l'ajoute à l'expression postfix */
                case x if !priority.keys.toSet.contains(x) =>
                    postfixExpression += s"$x "


                /**
                 * Si la pile est vide, contient un '(', x == '(' ou la priorité de l'élément scanné est supérieur à
                 * la priorité du dernier élément de la pile, on ajoute le charactère à la pile
                 */
                case x if scanner.isEmpty
                  || scanner.last == "("
                  || x == "("
                  || priority(char) > priority(scanner.last) =>
                    scanner :+= x


                /**
                 * Si le charactère scanné est ')', on ajoute tout les éléments de la pile
                 * jusqu'a croiser une parenthèse '('
                 */
                case x if x == ")" =>
                    var lastElement = scanner.last
                    while (lastElement != "(")
                        scanner = scanner.init
                        postfixExpression += s"$lastElement "
                        lastElement = scanner.last

                    scanner = scanner.init


                /**
                 * Si la priorité de la pile est inférieur à celle du dernier élément de la pile,
                 * on retire tous les élément de celle ci jusqu'a avoir un élément de priorité strictement inférieur
                 */
                case x if priority(x) <= priority(scanner.last) =>
                    var lastElement = scanner.last
                    var continue = true
                    while (continue && (priority(lastElement) >= priority(x)))
                        postfixExpression += s"$lastElement "
                        scanner = scanner.init
                        if (scanner.nonEmpty)
                            lastElement = scanner.last
                        else
                            continue = false


                    scanner :+= x


        postfixExpression += scanner.reverse.mkString(" ")
        postfixExpression.trim.split("\\s+").toList


    def checker(): Unit =
        if (!this.expression.matches(s"^$VALID_OPERATION$$"))
            throw new SyntaxException(INVALID_EXPRESSION)
        var stackBracket: List[String] = List()
        for (ch <- this.expression)
            ch.toString match
                case x: String if x == "(" => stackBracket :+= x
                case x: String if x == ")" =>
                    if (stackBracket.isEmpty)
                        throw new SyntaxException(INVALID_EXPRESSION)

                    stackBracket = stackBracket.init

                case _: String =>

        if (stackBracket.nonEmpty) throw new SyntaxException(INVALID_EXPRESSION)
}