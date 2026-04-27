package ex1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.should

class ParserScalaTest extends AnyFlatSpec:

  "A BasicParser" should "accept valid sequences" in:
    val parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabc".toList) should be (true)

  "A BasicParser" should "reject invalid sequences" in:
    val parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabcdc".toList) should be (false)

  "A BasicParser" should "accept empty sequences" in:
    val parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("".toList) should be (true)

  "A NotEmptyParser" should "accept valid non-empty sequences" in:
    val parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll("0101".toList) should be (true)

  "A NotEmptyParser" should "reject invalid non-empty sequences" in :
    val parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll("0123".toList) should be (false)

  "A NotEmptyParser" should "reject empty sequences" in :
    val parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll(List()) should be (false)

  "A NotTwoConsecutiveParser" should "accept valid sequences without two consecutive equal characters" in:
    val parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYZ".toList) should be (true)

  "A NotTwoConsecutiveParser" should "reject valid sequences with two consecutive equal characters" in :
    val parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYYZ".toList) should be (false)

  "A NotTwoConsecutiveParser" should "accept empty sequences" in :
    val parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll(List()) should be (true)

  "A NotEmptyAndNotTwoConsecutiveParser" should "accept valid sequences without two consecutive equal characters" in:
    val parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll("XYZ".toList) should be (true)

  "A NotEmptyAndNotTwoConsecutiveParser" should "reject valid sequences with two consecutive equal characters" in :
    val parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll("XYYZ".toList) should be (false)

  "A NotEmptyAndNotTwoConsecutiveParser" should "reject empty sequences" in :
    val parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll(List()) should be (false)
    
  "A string parser" should "accept valid strings" in:
    val sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("aabc".toList) should be (true)

  "A string parser" should "reject invalid strings" in :
    val sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("aabcdc".toList) should be (false)

  "A string parser" should "accept empty strings" in :
    val sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("".toList) should be (true)
    