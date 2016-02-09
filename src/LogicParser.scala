/**
  * Created by Jakub Martin on 2/8/2016.
  */

import scala.actors.Actor
import scala.util.parsing.combinator._

sealed trait LogicalOperator {
  def eval(): Boolean = this match {
    case True => true
    case False => false
    case And(xs) => xs.map(_ eval).reduce(_ && _)
    case Or(xs) => xs.map(_ eval).reduce(_ || _)
    case Xor(xs) => xs.map(_ eval).reduce((x, y) => (x || y) && !(x && y))
  }

  def evalWithActors(): Boolean = {
    case class LogicalOperatorEvaluator() extends Actor {

      def go(left: Int, f: (Boolean, Boolean) => Boolean): Boolean = {
        if (left > 0) {
          receive {
            case x: Boolean => f(x, go(left - 1, f))
          }
        } else
          receive {
            case x: Boolean => x
          }
      }

      def act(): Unit = {
        react {
            case True => {
              reply(true)
              exit()
            }
            case False => {
              reply(false)
              exit()
            }
            case And(xs) => {
              xs.map(new LogicalOperatorEvaluator().start() ! _)
              reply(go(xs.size - 1, _ && _))
              exit()
            }
            case Or(xs) => {
              xs.map(new LogicalOperatorEvaluator().start() ! _)
              reply(go(xs.size - 1, _ || _))
              exit()
            }
            case Xor(xs) => {
              xs.map(new LogicalOperatorEvaluator().start() ! _)
              reply(go(xs.size - 1, (x, y) => (x || y) && !(x && y)))
              exit()
            }
          }
        }
    }
    ((new LogicalOperatorEvaluator()).start() !! this)() match {
      case x: Boolean => x
    }
  }
}

case class And(operands: List[LogicalOperator]) extends LogicalOperator

case class Or(operands: List[LogicalOperator]) extends LogicalOperator

case class Xor(operands: List[LogicalOperator]) extends LogicalOperator

case object True extends LogicalOperator

case object False extends LogicalOperator

object LogicalOperator {

  object LogicParser extends RegexParsers {
    def obj: Parser[LogicalOperator] = and | or | xor | tru | fal

    def and: Parser[And] = "And" ~ "(" ~ ls ~ ")" ^^ ((x) => And(x._1._2))

    def or: Parser[Or] = "Or" ~ "(" ~ ls ~ ")" ^^ ((x) => Or(x._1._2))

    def xor: Parser[Xor] = "Xor" ~ "(" ~ ls ~ ")" ^^ ((x) => Xor(x._1._2))

    def tru: Parser[True.type] = "true" ^^ (_ => True)

    def fal: Parser[False.type] = "false" ^^ (_ => False)

    def ls: Parser[List[LogicalOperator]] = repsep(obj, ",")
  }

}