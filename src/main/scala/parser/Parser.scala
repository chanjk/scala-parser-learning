package parser

import ParseResult._

import Function.const
import cats.Monad
import cats.implicits._
import scala.annotation.tailrec

trait Parser[A] { def apply(input: String): ParseResult[A] }

object Parser {
  /**
    * Convenience function to build a parser from a parsing function
    */
  def apply[A](f: String => ParseResult[A]): Parser[A] = f(_)

  /**
    * Produces a parser that always fails with [[UnexpectedChar]] using the given character
    */
  def unexpectedCharParser[A](char: Char): Parser[A] = Parser(const(UnexpectedChar(char)))

  /**
    * Return a parser that always returns the given parse result
    *
    * scala> isErrorResult(constantParser(UnexpectedEof)("abc"))
    * res0: Boolean = true
    */
  def constantParser[A](pr: ParseResult[A]): Parser[A] = Parser(const(pr))

  /**
    * Return a parser that succeeds with a character off the input or fails with an error if the input is empty
    *
    * scala> character("abc")
    * res0: parser.ParseResult[Char] = Result >bc< a
    *
    * scala> isErrorResult(character(""))
    * res1: Boolean = true
    */
  def character: Parser[Char] = Parser(input => (input.headOption, input.tail) match {
    case (None, _) => UnexpectedEof
    case (Some(char), nextInput) => Result(nextInput, char)
  })

  /**
    * Parsers can map
    *
    * scala> map(character)(_.toUpper)("amz")
    * res0: parser.ParseResult[Char] = Result >mz< A
    */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = Parser(p(_) map f)

  /**
    * Return a parser that always succeeds with the given value and consumes no input
    *
    * scala> valueParser(3)("abc")
    * res0: parser.ParseResult[Int] = Result >abc< 3
    */
  def valueParser[A](value: A): Parser[A] = Parser(Result(_, value))

  /**
    * Return a parser that tries the first parser for a successful value
    *
    *   - If the first parser succeeds, then use this parser
    *
    *   - If the first parser fails, try the second parser
    *
    * scala> or(character)(valueParser('v'))("")
    * res0: parser.ParseResult[Char] = Result >< v
    *
    * scala> or(constantParser(UnexpectedEof))(valueParser('v'))("")
    * res1: parser.ParseResult[Char] = Result >< v
    *
    * scala> or(character)(valueParser('v'))("abc")
    * res2: parser.ParseResult[Char] = Result >bc< a
    *
    * scala> or(constantParser(UnexpectedEof))(valueParser('v'))("abc")
    * res3: parser.ParseResult[Char] = Result >abc< v
    */
  def or[A](p: Parser[A])(fallback: => Parser[A]): Parser[A] = Parser(input => {
    val pr = p(input)

    if (isErrorResult(pr)) fallback(input) else pr
  })

  /**
    * Parsers can flatMap
    * Return a parser that puts its input into the given parser and
    *
    *   - If that parser succeeds with a value (a), put that value into the given function
    *     then put in the remaining input in the resulting parser
    *
    *   - If that parser fails with an error, the returned parser fails with that error
    *
    * scala> flatMap(character)(c => if (c == 'x') character else valueParser('v'))("abc")
    * res0: parser.ParseResult[Char] = Result >bc< v
    *
    * scala> flatMap(character)(c => if (c == 'x') character else valueParser('v'))("a")
    * res1: parser.ParseResult[Char] = Result >< v
    *
    * scala> flatMap(character)(c => if (c == 'x') character else valueParser('v'))("xabc")
    * res2: parser.ParseResult[Char] = Result >bc< a
    *
    * scala> isErrorResult(flatMap(character)(c => if (c == 'x') character else valueParser('v'))(""))
    * res3: Boolean = true
    *
    * scala> isErrorResult(flatMap(character)(c => if (c == 'x') character else valueParser('v'))("x"))
    * res4: Boolean = true
    */
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(input => onResult(p(input))(s => a => f(a)(s)))

  /**
    * Parsers can pure
    */
  def pure[A](a: A): Parser[A] = valueParser(a)

  /**
    * Parsers can ap
    *
    * scala> ap(pure[Char => Char](_.toUpper))(valueParser('a'))("xyz")
    * res0: parser.ParseResult[Char] = Result >xyz< A
    *
    * scala> ap(map[Char, Char => List[Char]](character)(a => b => List(a, b)))(character)("abxyz")
    * res1: parser.ParseResult[List[Char]] = Result >xyz< List(a, b)
    *
    * Tip: Use [[flatMap]]
    */
  def ap[A, B](pf: Parser[A => B])(p: Parser[A]): Parser[B] = {
    def solution1: Parser[B] = flatMap(pf)(map(p))

    // Using the infix [[flatMap]] from the predefined Monad instance
    def solution2: Parser[B] = for (f <- pf; a <- p) yield f(a)

    solution1
  }

  /**
    * Return a parser that produces a character but fails if
    *
    *   - The input is empty
    *
    *   - The character does not satisfy the given predicate
    *
    * Tip: The [[flatMap]], [[unexpectedCharParser]] and [[character]] functions will be helpful here
    *
    * scala> satisfy(_.isUpper)("Abc")
    * res0: parser.ParseResult[Char] = Result >bc< A
    *
    * scala> isErrorResult(satisfy(_.isUpper)("abc"))
    * res1: Boolean = true
    */
  def satisfy(pred: Char => Boolean): Parser[Char] = {
    def solution1: Parser[Char] = flatMap(character)(c => if (pred(c)) pure(c) else unexpectedCharParser(c))

    // Since [[Char => *]] is a Monad, we can use [[cats.Monad#ifM]]
    def solution2: Parser[Char] = flatMap(character)(pred.ifM(pure, unexpectedCharParser))

    solution1
  }

  /**
    * Return a parser that produces the given character but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not equal to the given character
    *
    * Tip: Use the [[satisfy]] function
    */
  def is(char: Char): Parser[Char] = satisfy(_ == char)

  /**
    * Return a parser that produces a character between '0' and '9' but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not a digit
    *
    * Tip: Use the [[satisfy]] and [[scala.Char#isDigit]] functions
    *
    * scala> digit("9")
    * res0: parser.ParseResult[Char] = Result >< 9
    *
    * scala> digit("123")
    * res1: parser.ParseResult[Char] = Result >23< 1
    *
    * scala> isErrorResult(digit(""))
    * res2: Boolean = true
    *
    * scala> isErrorResult(digit("hello"))
    * res3: Boolean = true
    */
  def digit: Parser[Char] = satisfy(_.isDigit)

  /**
    * Return a parser that produces a space character but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not a space
    *
    * Tip: Use the [[satisfy]] and [[scala.Char#isWhitespace]] functions
    *
    * scala> space(" ")
    * res0: parser.ParseResult[Char] = Result ><
    *
    * scala> space("\n z")
    * res1: parser.ParseResult[Char] =
    * Result > z<
    *
    * scala> isErrorResult(space(""))
    * res2: Boolean = true
    *
    * scala> isErrorResult(space("a"))
    * res3: Boolean = true
    */
  def space: Parser[Char] = satisfy(_.isWhitespace)

  /**
    * Return a parser that conses the result of the first parser onto the result of
    * the second
    *
    * Tip: Use [[cats.Applicative#map2]], made available through the predefined [[cats.Monad]] instance
    *
    * scala> cons(character)(valueParser(Nil))("abc")
    * res0: parser.ParseResult[List[Char]] = Result >bc< List(a)
    *
    * scala> cons(digit)(valueParser(List('h', 'e', 'l', 'l', 'o')))("321")
    * res1: parser.ParseResult[List[Char]] = Result >21< List(3, h, e, l, l, o)
    */
  def cons[A](p: Parser[A])(pList: Parser[List[A]]): Parser[List[A]] = (p map2 pList)(_ :: _)

  /**
    * Return a parser that continues producing a list of values from the given parser
    *
    * Tip: Use [[list1]], [[pure]] and [[or]]
    *
    * scala> list(character)("")
    * res0: parser.ParseResult[List[Char]] = Result >< List()
    *
    * scala> list(digit)("123abc")
    * res1: parser.ParseResult[List[Char]] = Result >abc< List(1, 2, 3)
    *
    * scala> list(digit)("abc")
    * res2: parser.ParseResult[List[Char]] = Result >abc< List()
    *
    * scala> list(character)("abc")
    * res3: parser.ParseResult[List[Char]] = Result >< List(a, b, c)
    *
    * scala> list(character productR valueParser('v'))("abc")
    * res4: parser.ParseResult[List[Char]] = Result >< List(v, v, v)
    *
    * scala> list(character productR valueParser('v'))("")
    * res5: parser.ParseResult[List[Char]] = Result >< List()
    */
  def list[A](p: Parser[A]): Parser[List[A]] = {
    def solution1: Parser[List[A]] = or(list1(p))(pure(Nil))

    // Using [[cats.Monad#tailRecM]]
    def solution2: Parser[List[A]] = {
      def continue(p: Parser[A])(as: List[A]): Parser[Either[List[A], List[A]]] = map(p)(a => Left(a :: as))
      def stop(as: List[A]): Parser[Either[List[A], List[A]]] = pure(Right(as.reverse))

      List.empty[A] tailRecM (as => or(continue(p)(as))(stop(as)))
    }

    solution1
  }

  /**
    * Return a parser that produces at least one value from the given parser then
    * continues producing a list of values from the given parser (to ultimately produce a non-empty list)
    *
    * Tip: Use [[flatMap]] and [[list]]
    *
    * Challenge:
    *   - Use [[cats.Monad#tailRecM]] instead of [[list1]] to implement [[list]]
    *   - Then, use [[cons]] instead of [[flatMap]] to implement [[list1]]
    *
    * scala> list1(character)("abc")
    * res0: parser.ParseResult[List[Char]] = Result >< List(a, b, c)
    *
    * scala> list1(character productR valueParser('v'))("abc")
    * res1: parser.ParseResult[List[Char]] = Result >< List(v, v, v)
    *
    * scala> isErrorResult(list1(character productR valueParser('v'))(""))
    * res2: Boolean = true
    */
  def list1[A](p: Parser[A]): Parser[List[A]] = {
    def solution1: Parser[List[A]] = flatMap(p)(a => map(list(p))(a :: _))

    // Using the infix [[flatMap]] from the predefined Monad instance
    def solution2: Parser[List[A]] = for (a <- p; as <- list(p)) yield a :: as

    // Using [[cons]]
    def solution3: Parser[List[A]] = cons(p)(list(p))

    // Since [[Parser[A] => *]] is an Applicative, we can use [[cats.Applicative#ap]]
    def solution4: Parser[List[A]] = (cons[A] _) ap list apply p

    solution1
  }

  /**
    * Return a parser that produces one or more space characters
    * (consuming until the first non-space) but fails if
    *
    *   - The input is empty
    *
    *   - The first produced character is not a space
    *
    * Tip: Use the [[list1]] and [[space]] functions
    */
  def spaces1: Parser[List[Char]] = list1(space)

  /**
    * Return a parser that produces a lower-case character but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not lower-case
    *
    * Tip: Use the [[satisfy]] and [[scala.Char#isLower]] functions
    */
  def lower: Parser[Char] = satisfy(_.isLower)

  /**
    * Return a parser that produces an upper-case character but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not upper-case
    *
    * Tip: Use the [[satisfy]] and [[scala.Char#isUpper]] functions
    */
  def upper: Parser[Char] = satisfy(_.isUpper)

  /**
    * Return a parser that produces an alpha character but fails if
    *
    *   - The input is empty
    *
    *   - The produced character is not alpha
    *
    * Tip: Use the [[satisfy]] and [[scala.Char#isLetter]] functions
    */
  def alpha: Parser[Char] = satisfy(_.isLetter)

  /**
    * Return a parser that sequences the given list of parsers by producing all their results
    * but fails on the first failing parser of the list
    *
    * Tip: Use [[cons]] and [[pure]]
    *
    * Tip: Optionally use [[scala.collection.immutable.List#foldRight]]; if not, an explicit recursive call
    *
    * scala> sequenceParser(List(character, is('x'), upper))("axCdef")
    * res0: parser.ParseResult[List[Char]] = Result >def< List(a, x, C)
    *
    * scala> isErrorResult(sequenceParser(List(character, is('x'), upper))("abCdef"))
    * res1: Boolean = true
    */
  def sequenceParser[A](ps: List[Parser[A]]): Parser[List[A]]= {
    def solution1: Parser[List[A]] = ps.foldRight(pure(List.empty[A]))(Function.uncurried(cons))

    // Using an explicit recursive call
    def solution2: Parser[List[A]] = ps match {
      case Nil => pure(Nil)
      case q :: qs => cons(q)(sequenceParser(qs))
    }

    // Using [[cats.Traverse#sequence]]
    def solution3: Parser[List[A]] = ps.sequence

    // Using [[cats.Monad#tailRecM]]
    def solution4: Parser[List[A]] = (ps, List.empty[A]) tailRecM {
      case (Nil, as) => pure(Right(as.reverse))
      case (q :: qs, as) => map(q)(a => Left((qs, a :: as)))
    }

    solution1
  }

  /**
    * Return a parser that produces the given number of values off the given parser
    * This parser fails if the given parser fails in the attempt to produce the given number of values
    *
    * Tip: Use [[sequenceParser]] and [[scala.collection.immutable.List.fill]]
    *
    * scala> thisMany(4)(upper)("ABCDef")
    * res0: parser.ParseResult[List[Char]] = Result >ef< List(A, B, C, D)
    *
    * scala> isErrorResult(thisMany(4)(upper)("ABcDef"))
    * res1: Boolean = true
    */
  def thisMany[A](n: Int)(p: Parser[A]): Parser[List[A]] = sequenceParser(List.fill(n)(p))

  implicit val monad: Monad[Parser] = new Monad[Parser] {
    override def map[A, B](p: Parser[A])(f: A => B): Parser[B] = Parser.map(p)(f)

    def pure[A](a: A): Parser[A] = Parser.pure(a)

    override def ap[A, B](pf: Parser[A => B])(p: Parser[A]): Parser[B] = Parser.ap(pf)(p)

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = Parser.flatMap(p)(f)

    def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = Parser(input => {
      @tailrec
      def loop(thisA: A, thisInput: String): ParseResult[B] = f(thisA)(thisInput) match {
        case UnexpectedEof => UnexpectedEof
        case ExpectedEof(unexpected) => ExpectedEof(unexpected)
        case UnexpectedChar(char) => UnexpectedChar(char)
        case UnexpectedString(string) => UnexpectedString(string)
        case Result(nextInput, aOrB) => aOrB match {
          case Left(nextA) => loop(nextA, nextInput)
          case Right(parsed) => Result(nextInput, parsed)
        }
      }

      loop(a, input)
    })
  }
}
