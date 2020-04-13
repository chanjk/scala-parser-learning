package parser

import cats.Eq
import cats.implicits._
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws

class ParserLawsSpec extends AnyFreeSpec {
  "monad laws" - {
    checkAll(MonadTests[Parser].monad[Int, Int, String])
  }

  implicit def arbParser[A](implicit ev: Arbitrary[ParseResult[A]]): Arbitrary[Parser[A]] =
    Arbitrary(Arbitrary.arbitrary[String => ParseResult[A]] map Parser[A])

  implicit def arbParseResult[A : Arbitrary]: Arbitrary[ParseResult[A]] = Arbitrary(Gen.oneOf(
    Gen.const(UnexpectedEof),
    Arbitrary.arbitrary[String] map ExpectedEof,
    Arbitrary.arbitrary[Char] map UnexpectedChar,
    Arbitrary.arbitrary[String] map UnexpectedString,
    Gen.zip(Arbitrary.arbitrary[String], Arbitrary.arbitrary[A]) map (Result[A] _).tupled
  ))

  // https://github.com/typelevel/cats/blob/b99a8545af87af62c19d46f06d693829bddfdd3c/laws/src/main/scala/cats/laws/discipline/Eq.scala#L131
  implicit def eqParser[A](implicit eqPRA: Eq[ParseResult[A]]): Eq[Parser[A]] =
    new Eq[Parser[A]] {
      val sampleCnt: Int = 50

      def eqv(f: Parser[A], g: Parser[A]): Boolean = {
        val samples = List.fill(sampleCnt)(Arbitrary.arbitrary[String].sample).collect {
          case Some(a) => a
          case None    => sys.error("Could not generate arbitrary values to compare two parsers")
        }
        samples.forall(s => eqPRA.eqv(f(s), g(s)))
      }
    }

  implicit def eqParseResult[A]: Eq[ParseResult[A]] = Eq.fromUniversalEquals

  def checkAll(ruleSet: Laws#RuleSet): Unit =
    for ((name, prop) <- ruleSet.all.properties) name in { Checkers.check(prop) }
}
