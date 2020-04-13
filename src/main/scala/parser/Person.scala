package parser

import Parser._

import cats.implicits._

/**
  * Suppose we have a data structure to represent a person. The person data structure has these attributes:
  *   - Age: positive integer
  *   - First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters
  *   - Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
  *   - Smoker: character that must be 'y' or 'n' that maps to a boolean
  *   - Phone: string of digits, dots or hyphens but must start with a digit and end with a hash (#)
  */
case class Person(age: Int, firstName: String, surname: String, smoker: Boolean, phone: String)

object Person {
  /**
    * Convenience function to convert a parser that produces a list of characters into a parser that produces a string
    * This parser fails if the given parser fails in the attempt to produce a list of characters
    */
  private def charsToString(p: Parser[List[Char]]): Parser[String] = p map (_.mkString)

  /**
    * This one is done for you
    *
    * ''Age: positive integer''
    *
    * scala> ageParser("120")
    * res0: parser.ParseResult[Int] = Result >< 120
    *
    * scala> isErrorResult(ageParser("abc"))
    * res1: Boolean = true
    *
    * scala> isErrorResult(ageParser("-120"))
    * res2: Boolean = true
    */
  def ageParser: Parser[Int] = charsToString(list1(digit)) flatMap (s => s.toIntOption match {
    case None => constantParser(UnexpectedString(s))
    case Some(age) => age.pure[Parser]
  })

  /**
    * Write a parser for Person.firstName
    *
    * ''First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters''
    *
    * Tip: Use [[cons]], [[upper]], [[list]] and [[lower]]
    *
    * scala> firstNameParser("Abc")
    * res0: parser.ParseResult[String] = Result >< Abc
    *
    * scala> isErrorResult(firstNameParser("abc"))
    * res1: Boolean = true
    */
  def firstNameParser: Parser[String] = ???

  /**
    * Write a parser for Person.surname
    *
    * ''Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters''
    *
    * Tip: Use [[flatMap]], [[upper]], [[thisMany]], [[lower]] and [[list]]
    *
    * scala> surnameParser("Abcdef")
    * res0: parser.ParseResult[String] = Result >< Abcdef
    *
    * scala> surnameParser("Abcdefghijklmnopqrstuvwxyz")
    * res1: parser.ParseResult[String] = Result >< Abcdefghijklmnopqrstuvwxyz
    *
    * scala> isErrorResult(surnameParser("Abc"))
    * res2: Boolean = true
    *
    * scala> isErrorResult(surnameParser("abc"))
    * res3: Boolean = true
    */
  def surnameParser: Parser[String] = ???

  /**
    * Write a parser for Person.smoker
    *
    * ''Smoker: character that must be 'y' or 'n'''
    *
    * Tip: Use [[is]] and [[or]]
    *
    * scala> smokerParser("yabc")
    * res0: parser.ParseResult[Boolean] = Result >abc< true
    *
    * scala> smokerParser("nabc")
    * res1: parser.ParseResult[Boolean] = Result >abc< false
    *
    * scala> isErrorResult(smokerParser("abc"))
    * res2: Boolean = true
    */
  def smokerParser: Parser[Boolean] = ???

  /**
    * Write part of a parser for Person.phone
    * This parser will only produce a string of digits, dots or hyphens
    * It will ignore the overall requirement of a phone number to start with a digit and end with a hash (#)
    *
    * ''Phone: string of digits, dots or hyphens ...''
    *
    * Tip: Use [[list]], [[digit]], [[or]] and [[is]]
    *
    * scala> phoneBodyParser("123-456")
    * res0: parser.ParseResult[String] = Result >< 123-456
    *
    * scala> phoneBodyParser("123-4a56")
    * res1: parser.ParseResult[String] = Result >a56< 123-4
    *
    * scala> phoneBodyParser("a123-456")
    * res2: parser.ParseResult[String] = Result >a123-456<
    */
  def phoneBodyParser: Parser[String] = ???

  /**
    * Write a parser for Person.phone
    *
    * ''Phone: ... but must start with a digit and end with a hash (#)''
    *
    * Tip: Use [[flatMap]], [[digit]], [[phoneBodyParser]] and [[is]]
    *
    * scala> phoneParser("123-456#")
    * res0: parser.ParseResult[String] = Result >< 123-456
    *
    * scala> phoneParser("123-456#abc")
    * res1: parser.ParseResult[String] = Result >abc< 123-456
    *
    * scala> isErrorResult(phoneParser("123-456"))
    * res2: Boolean = true
    *
    * scala> isErrorResult(phoneParser("a123-456"))
    * res3: Boolean = true
    */
  def phoneParser: Parser[String] = ???

  /**
    * Write a parser for Person
    *
    * Tip: Use  [[flatMap]],
    *           [[cats.Apply#productR]],
    *           [[spaces1]],
    *           [[ageParser]],
    *           [[firstNameParser]],
    *           [[surnameParser]],
    *           [[smokerParser]],
    *           [[phoneParser]]
    *
    * Tip: Follow-on exercise: Use [[ap]] and [[pure]] instead of [[flatMap]]
    *
    * Tip: Follow-on exercise: Use [[apGobble]] (defined below) instead of [[ap]] and [[cats.Apply#productR]]
    *
    * scala> isErrorResult(personParser(""))
    * res0: Boolean = true
    *
    * scala> isErrorResult(personParser("12x Fred Clarkson y 123-456.789#"))
    * res1: Boolean = true
    *
    * scala> isErrorResult(personParser("123 fred Clarkson y 123-456.789#"))
    * res2: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred Cla y 123-456.789#"))
    * res3: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred clarkson y 123-456.789#"))
    * res4: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred Clarkson x 123-456.789#"))
    * res5: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred Clarkson y 1x3-456.789#"))
    * res6: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred Clarkson y -123-456.789#"))
    * res7: Boolean = true
    *
    * scala> isErrorResult(personParser("123 Fred Clarkson y 123-456.789"))
    * res8: Boolean = true
    *
    * scala> personParser("123 Fred Clarkson y 123-456.789#")
    * res9: parser.ParseResult[parser.Person] = Result >< Person(123,Fred,Clarkson,true,123-456.789)
    *
    * scala> personParser("123 Fred Clarkson y 123-456.789# rest")
    * res10: parser.ParseResult[parser.Person] = Result > rest< Person(123,Fred,Clarkson,true,123-456.789)
    *
    * scala> personParser("123  Fred   Clarkson    y     123-456.789#")
    * res11: parser.ParseResult[parser.Person] = Result >< Person(123,Fred,Clarkson,true,123-456.789)
    */
  def personParser: Parser[Person] = ???

  /**
    * Did you repeat yourself in [[personParser]]? This might help:
    */
  private def flatMapGobble[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = (p productL spaces1) flatMap f

  /**
    * or maybe this
    */
  private def apGobble[A, B](pf: Parser[A => B])(p: Parser[A]): Parser[B] = pf ap (spaces1 productR p)

  /**
    * or if you prefer infix notation
    */
  private implicit class ParserOps[A](pa: Parser[A]) {
    def flatMapGobble[B](f: A => Parser[B]): Parser[B] = (pa productL spaces1) flatMap f

    def apGobble[B, C](pb: Parser[B])(implicit ev: A <:< (B => C)): Parser[C] = pa ap (spaces1 productR pb)
  }
}
