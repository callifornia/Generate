package com.foo.app

import scala.io.Source
import scala.util.{Failure, Success, Try}

import Validation._

object Generator {

  // for testing. this should be rewritten
  var vocabulary = Source.fromURL("https://users.cs.duke.edu/~ola/ap/linuxwords").getLines().toSet

  def main(args: Array[String]): Unit = {
    val userInput = readFromKeyboard
    val result = action(userInput)
    print(result)
  }

  def readFromKeyboard = {
    println("Generating all possible words from number combination. Example: 2334-23")
    print("Enter numbers: ")
    scala.io.StdIn.readLine()
  }

  def action(input: String): String = {
    val result = for {
      parsedInput <- parseInput(input)
      numbers <- validateByNumbers(parsedInput)
      validNumbers <- validateByAcceptableNumbers(numbers)
    } yield process(validNumbers)

    result.fold(error => error, {
      case Nil => emptyResultMsg
      case xs => xs.sorted.mkString(", ")})
  }


  // process main logic
  def process(validNumbers: List[String]): List[String] = {

    // if word exist in vocabulary
    val wordExistInVocabulary: String => Boolean = word => {
      Try(vocabulary.contains(word)).getOrElse(false)
    }

    // split words combination by "-" if generated words combinations acceptable by there size
    val buildResult: List[String] => List[String] = tmpResult => {
      val isResultSizeCorrect: Boolean =
        tmpResult.flatten.size == validNumbers.size

      if (isResultSizeCorrect) List(tmpResult.mkString("-"))
      else List.empty[String]
    }

    // recursively generate words from numbers
    def generateWords(numbersToProcess: List[String])(numbersToCheck: List[String])(tmpResult: List[String]): List[String] = {
      numbersToProcess match {
        case Nil => {
          for {
            word <- generateWordsCombinationFor(numbersToCheck) withFilter wordExistInVocabulary
            result <- buildResult(tmpResult :+ word)
          } yield result
        }
        case x :: xs => {
          (for {
            word <- generateWordsCombinationFor(numbersToCheck) withFilter wordExistInVocabulary
            w2 <- generateWords(xs)(x :: Nil)(tmpResult :+ word) // generate new words started from next number in sequence
          } yield w2) ::: generateWords(xs)(numbersToCheck:+x)(tmpResult) // join both results
        }
      }
    }
    generateWords(validNumbers)(List.empty)(List.empty)
  }

  // Generate words combination from number representation.
  def generateWordsCombinationFor(numbers: List[String]): List[String] = {
    val lessThan: List[String] => Boolean = list =>
      list.size < numbers.size

    val generate: (List[String], List[String]) => List[String] =
      (list1, list2) => list1.flatMap(l1 => list2.map(l2 => l1 + l2))

    val generateWordsFromInputNumbers: List[String] => List[String] = numbers => {
      if (numbers.isEmpty) List.empty
      else numbers.map(el => numbersLettersMapping(el)).reduceLeft(generate)
    }

    numbers
      .inits
      .toList
      .reverse
      .dropWhile(lessThan)
      .flatMap(generateWordsFromInputNumbers)
  }
}

object Validation {

  val emptyResultMsg = "No words combinations found"

  val validateNumbersErrorMsg: Throwable => Either[String, Nothing]= ex =>
    Left(s"Invalid data ${ex.getMessage.toLowerCase}. Only [1,2,3,4,5,6,7,8] that can be spited by '-'")

  val validateAcceptableNumbersErrorMsg: List[String] => Either[String, Nothing] = list =>
    Left(s"Invalid input data. Number(s): [${list.mkString(",")}] not allowed. Only [1,2,3,4,5,6,7,8] that can be spited by '-'")

  val parseErrorMsg: Throwable => Either[String, Nothing] = ex =>
    Left(s"Invalid input. Can't parse input: ${ex.getMessage}")

  lazy val numbersLettersMapping = Map(
    "2" -> List("a", "b", "c"),
    "3" -> List("d", "e", "f"),
    "4" -> List("g", "h", "i"),
    "5" -> List("j", "k", "l"),
    "6" -> List("m", "n", "o"),
    "7" -> List("p", "q", "r", "s"),
    "8" -> List("t", "u", "v"),
    "9" -> List("w", "x", "y", "z")
  )

  // "12-34" -> List("1", "2", "3", "4")
  def parseInput(input: String): Either[String, List[String]] = {
    Try(input.split("-").toList.flatten.map(_.toString)) match {
      case Success(parsedInput) => Right(parsedInput)
      case Failure(ex) => parseErrorMsg(ex)
    }
  }

  // Validate user input. Accept only that numbers for which letters are represented.
  def validateByAcceptableNumbers(inputNumbers: List[String]): Either[String, List[String]] = {
    val accessibleNumbers: String => Boolean = number =>
      numbersLettersMapping.keys.exists(n => n == number)

    inputNumbers.filterNot(accessibleNumbers) match {
      case Nil => Right(inputNumbers)
      case xs => validateAcceptableNumbersErrorMsg(xs)
    }
  }


  // Validate user input. Accept numbers only
  def validateByNumbers(parsedInput: List[String]): Either[String, List[String]] = {
    Try(Integer.parseInt(parsedInput.mkString.trim)) match {
      case Success(_) => Right(parsedInput)
      case Failure(ex) => validateNumbersErrorMsg(ex)
    }
  }
}

