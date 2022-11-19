package utilities

import scala.util.matching.Regex

abstract class Operation {
    val INVALID_EXPRESSION = "Invalid expression"
    val INVALID_IDENTIFIER = "Invalid identifier"
    val INVALID_ASSIGNMENT = "Invalid assignment"
    private val VALID_IDENTIFIER_IN_OP = raw"([A-z()]*|\d*)".r
    val VALID_OPERATION: Regex = raw"((\(*$VALID_IDENTIFIER_IN_OP\s*([+-]+|[*/^])\s*([A-z()]+|\d+)\)*)+|$VALID_IDENTIFIER_IN_OP)".r
    val VALID_IDENTIFIER: Regex = raw"[A-z]+".r

    def init(): Unit

    def checker(): Unit

    def execute(): Any
}