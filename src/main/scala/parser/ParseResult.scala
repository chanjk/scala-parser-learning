package parser

import cats.{Functor, Show}
import cats.implicits._

sealed trait ParseResult[+A] {
  override def toString(): String = this.show
}
case object UnexpectedEof extends ParseResult[Nothing]
case class ExpectedEof(unexpected: String) extends ParseResult[Nothing]
case class UnexpectedChar(char: Char) extends ParseResult[Nothing]
case class UnexpectedString(string: String) extends ParseResult[Nothing]
case class Result[A](nextInput: String, parsed: A) extends ParseResult[A]

object ParseResult {
  implicit def show[A]: Show[ParseResult[A]] = Show.show {
    case UnexpectedEof => "Unexpected end of stream"
    case ExpectedEof(unexpected) => s"Expected end of stream, but got >$unexpected<"
    case UnexpectedChar(char) => s"Unexpected character: $char"
    case UnexpectedString(string) => s"Unexpected string: $string"
    case Result(nextInput, parsed) => s"Result >$nextInput< $parsed"
  }

  implicit val functor: Functor[ParseResult] = new Functor[ParseResult] {
    def map[A, B](pr: ParseResult[A])(f: A => B): ParseResult[B] = pr match {
      case UnexpectedEof => UnexpectedEof
      case ExpectedEof(unexpected) => ExpectedEof(unexpected)
      case UnexpectedChar(char) => UnexpectedChar(char)
      case UnexpectedString(string) => UnexpectedString(string)
      case Result(nextInput, parsed) => Result(nextInput, f(parsed))
    }
  }

  /**
    * Function to determine whether this [[ParseResult]] is an error
    */
  def isErrorResult[A](pr: ParseResult[A]): Boolean = pr match {
    case UnexpectedEof => true
    case ExpectedEof(_) => true
    case UnexpectedChar(_) => true
    case UnexpectedString(_) => true
    case Result(_, _) => false
  }

  /**
    * Runs the given function on a successful parse result
    *
    * Otherwise, return the same failing parse result
    */
  def onResult[A, B](pr: ParseResult[A])(f: String => A => ParseResult[B]): ParseResult[B] = pr match {
    case UnexpectedEof => UnexpectedEof
    case ExpectedEof(unexpected) => ExpectedEof(unexpected)
    case UnexpectedChar(char) => UnexpectedChar(char)
    case UnexpectedString(string) => UnexpectedString(string)
    case Result(nextInput, parsed) => f(nextInput)(parsed)
  }
}
