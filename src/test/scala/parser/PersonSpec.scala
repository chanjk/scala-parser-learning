package parser

import ParseResult._
import Person._

import cats.implicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalactic.TypeCheckedTripleEquals

class PersonSpec extends AnyFreeSpec with Matchers with TypeCheckedTripleEquals {
  "ageParser" - {
    "fails on invalid age (all letters)" in {
      isErrorResult(ageParser("abc")) must === (true)
    }
    "fails on invalid age (leading '-')" in {
      isErrorResult(ageParser("-120")) must === (true)
    }
    "parses valid age" in {
      ageParser("120") must === (Result("", 120))
    }
  }

  "firstNameParser" - {
    "fails on first name that doesn't start with a capital" in {
      isErrorResult(firstNameParser("abc")) must === (true)
    }
    "parses valid first name" in {
      firstNameParser("Abc") must === (Result("", "Abc"))
    }
  }

  "surnameParser" - {
    "fails on short surname" in {
      isErrorResult(surnameParser("Abc")) must === (true)
    }
    "fails on short surname starting with a lower case letter" in {
      isErrorResult(surnameParser("abc")) must === (true)
    }
    "parses shortest valid surname" in {
      surnameParser("Abcdef") must === (Result("", "Abcdef"))
    }
    "parses long surname" in {
      surnameParser("Abcdefghijklmnopqrstuvwxyz") must === (Result("", "Abcdefghijklmnopqrstuvwxyz"))
    }
  }

  "smokerParser" - {
    "fails on non y/n value" in {
      isErrorResult(smokerParser("abc")) must === (true)
    }
    "parses y, leaving remaining input" in {
      smokerParser("yabc") must === (Result("abc", true))
    }
    "parses n, leaving remaining input" in {
      smokerParser("nabc") must === (Result("abc", false))
    }
  }

  "phoneBodyParser" - {
    "produces empty list when no characters match" in {
      phoneBodyParser("a123-456") must === (Result("a123-456", ""))
    }
    "parses valid phone body value" in {
      phoneBodyParser("123-456") must === (Result("", "123-456"))
    }
    "parses up to first letter" in {
      phoneBodyParser("123-a456") must === (Result("a456", "123-"))
    }
  }

  "phoneParser" - {
    "fails without trailing '#'" in {
      isErrorResult(phoneParser("123-456")) must === (true)
    }
    "fails when input starts with a letter" in {
      isErrorResult(phoneParser("a123-456")) must === (true)
    }
    "produces valid phone number" in {
      phoneParser("123-456#") must === (Result("", "123-456"))
    }
    "produces a valid phone number with remaining input" in {
      phoneParser("123-456#abc") must === (Result("abc", "123-456"))
    }
  }

  "personParser" - {
    "fails on empty string" in {
      isErrorResult(personParser("")) must === (true)
    }
    "fails on invalid age" in {
      isErrorResult(personParser("12x Fred Clarkson y 123-456.789#")) must === (true)
    }
    "fails on first name that doesn't start with capital" in {
      isErrorResult(personParser("123 fred Clarkson y 123-456.789#")) must === (true)
    }
    "fails on surname that is too short" in {
      isErrorResult(personParser("123 Fred Cla y 123-456.789#")) must === (true)
    }
    "fails on surname that doesn't start with a capital letter" in {
      isErrorResult(personParser("123 Fred clarkson y 123-456.789#")) must === (true)
    }
    "fails on invalid smoker value 'x'" in {
      isErrorResult(personParser("123 Fred Clarkson x 123-456.789#")) must === (true)
    }
    "fails on phone number containing an 'x'" in {
      isErrorResult(personParser("123 Fred Clarkson y 1x3-456.789#")) must === (true)
    }
    "fails on phone number starting with '-'" in {
      isErrorResult(personParser("123 Fred Clarkson y -123-456.789#")) must === (true)
    }
    "fails on phone number without a trailing '#'" in {
      isErrorResult(personParser("123 Fred Clarkson y 123-456.789")) must === (true)
    }
    "produces person for valid input" in {
      personParser("123 Fred Clarkson y 123-456.789#") must === (Result("", Person(123, "Fred", "Clarkson", true, "123-456.789")))
    }
    "produces person for valid input and keeps remaining input" in {
      personParser("123 Fred Clarkson y 123-456.789# rest") must === (Result(" rest", Person(123, "Fred", "Clarkson", true, "123-456.789")))
    }
    "produces person for valid input containing extra whitespace" in {
      personParser("123  Fred   Clarkson    y     123-456.789#") must === (Result("", Person(123, "Fred", "Clarkson", true, "123-456.789")))
    }
  }
}
