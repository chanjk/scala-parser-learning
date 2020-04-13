package parser

import ParseResult._
import Parser._

import cats.implicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalactic.TypeCheckedTripleEquals

class ParserSpec extends AnyFreeSpec with Matchers with TypeCheckedTripleEquals {
  "constantParser" - {
    "can return error result" in {
      constantParser(UnexpectedEof)("abc") must === (UnexpectedEof)
    }
    "can return ParseResult" in {
      constantParser(Result("xyz", 4))("abc") must === (Result("xyz", 4))
    }
  }

  "character" - {
    "parses single character from non-empty string" in {
      character("abc") must === (Result("bc", 'a'))
    }
    "parsing empty string is an error" in {
      isErrorResult(character("")) must === (true)
    }
  }

  "map" - {
    "toUpper" in {
      map(character)(_.toUpper)("amz") must === (Result("mz", 'A'))
    }
  }

  "valueParser" - {
    "succeeds with given value" in {
      valueParser(3)("abc") must === (Result("abc", 3))
    }
  }

  "or" - {
    "first fails, second succeeds with no input" in {
      or(character)(valueParser('v'))("") must === (Result("", 'v'))
    }
    "first always fails, second succeeds with no input" in {
      or(constantParser(UnexpectedEof))(valueParser('v'))("") must === (Result("", 'v'))
    }
    "first always fails, second succeeds with input" in {
      or(constantParser(UnexpectedEof))(valueParser('v'))("abc") must === (Result("abc", 'v'))
    }
    "takes first parse result when it succeeds" in {
      or(character)(valueParser('v'))("abc") must === (Result("bc", 'a'))
    }
  }

  "flatMap" - {
    "first parse fails" in {
      isErrorResult(flatMap(character)(c => if (c == 'x') character else valueParser('v'))("")) must === (true)
    }
    "second parse fails" in {
      isErrorResult(flatMap(character)(c => if (c == 'x') character else valueParser('v'))("x")) must === (true)
    }
    "flatMap to valueParser" in {
      flatMap(character)(c => if (c == 'x') character else valueParser('v'))("abc") must === (Result("bc", 'v'))
    }
    "flatMap to valueParser with no more input" in {
      flatMap(character)(c => if (c == 'x') character else valueParser('v'))("a") must === (Result("", 'v'))
    }
    "flatMap to character parser with remaining input" in {
      flatMap(character)(c => if (c == 'x') character else valueParser('v'))("xabc") must === (Result("bc", 'a'))
    }
  }

  "pure" - {
    "succeeds with given value" in {
      pure('a')("xyz") must === (Result("xyz", 'a'))
    }
    "pure an Option value" in {
      pure(Some(5))("xyz") must === (Result("xyz", Some(5)))
    }
  }

  "ap" - {
    "ap pure toUpper" in {
      ap(pure[Char => Char](_.toUpper))(valueParser('a'))("xyz") must === (Result("xyz", 'A'))
    }
    "ap pure show" in {
      ap(pure[Int => String](_.show))(valueParser(599))("xyz") must === (Result("xyz", "599"))
    }
    "ap append character" in {
      ap(map[Char, Char => List[Char]](character)(a => b => List(a, b)))(character)("abxyz") must === (Result("xyz", List('a', 'b')))
    }
  }

  "satisfy" - {
    "when character satisfies predicate" in {
      satisfy(_.isUpper)("Abc") must === (Result("bc", 'A'))
    }
    "is error when predicate not satisfied" in {
      isErrorResult(satisfy(_.isUpper)("abc")) must === (true)
    }
  }

  "is" - {
    "when character matches" in {
      is('a')("abc") must === (Result("bc", 'a'))
    }
    "is error when character does not match" in {
      isErrorResult(is('x')("abc")) must === (true)
    }
    "is error when parsing empty string" in {
      isErrorResult(is('a')("")) must === (true)
    }
  }

  "digit" - {
    "is error when input empty" in {
      isErrorResult(digit("")) must === (true)
    }
    "is error when character not digit" in {
      isErrorResult(digit("ABC")) must === (true)
    }
    "succeeds when character is a digit" in {
      digit("1BC") must === (Result("BC", '1'))
    }
  }

  "space" - {
    "fails when input empty" in {
      isErrorResult(space("")) must === (true)
    }
    "fails when character not space" in {
      isErrorResult(space("ABC")) must === (true)
    }
    "succeeds when first character is a space" in {
      space(" abc") must === (Result("abc", ' '))
    }
  }

  "cons" - {
    "conses onto an empty list" in {
      cons(character)(valueParser(Nil))("abc") must === (Result("bc", List('a')))
    }
    "conses onto a non-empty list" in {
      cons(digit)(valueParser(List('h', 'e', 'l', 'l', 'o')))("321") must === (Result("21", List('3', 'h', 'e', 'l', 'l', 'o')))
    }
    "conses the result of the first parser onto the result of the second" in {
      cons(character)(cons(character)(valueParser(Nil)))("abc") must === (Result("c", List('a', 'b')))
    }
  }

  "list" - {
    "succeeds on empty input" in {
      list(character)("") must === (Result("", List()))
    }
    "parses for as long as characters match" in {
      list(digit)("123abc") must === (Result("abc", List('1', '2', '3')))
    }
    "parses empty value when no matching characters" in {
      list(digit)("abc") must === (Result("abc", List()))
    }
    "parses entire input if matches" in {
      list(character)("abc") must === (Result("", List('a', 'b', 'c')))
    }
    "parses for as long as characters match with value parser" in {
      list(character productR valueParser('v'))("abc") must === (Result("", List('v', 'v', 'v')))
    }
    "succeeds on empty input with value parser" in {
      list(character productR valueParser('v'))("") must === (Result("", List()))
    }
  }

  "list1" - {
    "succeeds when at least one character matches" in {
      list1(character)("abc") must === (Result("", List('a', 'b', 'c')))
    }
    "succeeds when at least one character matches with value parser" in {
      list1(character productR valueParser('v'))("abc") must === (Result("", List('v', 'v', 'v')))
    }
    "no matching chars fails" in {
      isErrorResult(list1(character productR valueParser('v'))("")) must === (true)
    }
  }

  "spaces1" - {
    "fails on empty string" in {
      isErrorResult(spaces1("")) must === (true)
    }
    "consumes single space" in {
      spaces1(" ") must === (Result("", List(' ')))
    }
    "consumes multiple spaces" in {
      spaces1("    abc") must === (Result("abc", List(' ', ' ', ' ', ' ')))
    }
  }

  "lower" - {
    "fails on empty string" in {
      isErrorResult(lower("")) must === (true)
    }
    "fails if character is not lowercase" in {
      isErrorResult(lower("Abc")) must === (true)
    }
    "produces lowercase character" in {
      lower("aBC") must === (Result("BC", 'a'))
    }
  }

  "upper" - {
    "fails on empty string" in {
      isErrorResult(upper("")) must === (true)
    }
    "fails if character is not uppercase" in {
      isErrorResult(upper("aBC")) must === (true)
    }
    "produces uppercase character" in {
      upper("Abc") must === (Result("bc", 'A'))
    }
  }

  "alpha" - {
    "fails on empty string" in {
      isErrorResult(alpha("")) must === (true)
    }
    "fails if character is not alpha" in {
      isErrorResult(alpha("5BC")) must === (true)
    }
    "produces alpha character" in {
      alpha("A45") must === (Result("45", 'A'))
    }
  }

  "sequenceParser" - {
    "fails on first failing parser" in {
      isErrorResult(sequenceParser(List(character, is('x'), upper))("abCdef")) must === (true)
    }
    "sequences list of successful parsers" in {
      sequenceParser(List(character, is('x'), upper))("axCdef") must === (Result("def", List('a', 'x', 'C')))
    }
  }

  "thisMany" - {
    "fails when not enough matches" in {
      isErrorResult(thisMany(4)(upper)("ABcDef")) must === (true)
    }
    "produces n values when matched" in {
      thisMany(4)(upper)("ABCDef") must === (Result("ef", List('A', 'B', 'C', 'D')))
    }
  }
}
