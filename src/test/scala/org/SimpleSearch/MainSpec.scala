package org.SimpleSearch

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MainSpec extends AnyFlatSpec {
  behavior of "Simple Search Program"

  object TestProgram extends SimpleSearchT with MockOutputT

  trait MockOutputT extends OutputT {
    var consoleMessages: Seq[String] = Seq()

    override def println(str: String): Unit =
      consoleMessages = consoleMessages :+ str
  }

  it should "gracefully reject a missing argument" in {
    val exception = TestProgram.readFile(Array())
    assert(exception == Left(TestProgram.MissingPathArg))
  }

  it should "gracefully reject an invalid file path" ignore {
    val result    = TestProgram.readFile(Array(null))
    val exception = new NullPointerException
    assert(result == Left(TestProgram.FileNotFound(exception)))
  }

  it should "read a valid directory path and return a valid path" in {
    assert(
      TestProgram.readFile(Array("./data")) ==
        Right(new java.io.File("./data"))
    )
  }

  it should "gracefully reject a file path argument" in {
    val exception = TestProgram.readFile(Array("./data/codegeek.txt"))
    assert(exception == Left(
      TestProgram.NotDirectory("Path [./data/codegeek.txt] is not a directory")))
  }

  it should "index directory files" in {
    val dir   = new java.io.File("./data")
    val index = TestProgram.Index(Map(
      'h' -> Map("hello" -> List("index_text.txt")),
      'd' -> Map("day" -> List("index_text.txt")),
      'r' -> Map("rockerchallenge" -> List("index_text.txt")),
      'w' -> Map("world" -> List("index_text.txt")),
      'o' -> Map("of" -> List("index_text.txt"), "one" -> List("index_text.txt"))
    ))

    assert(TestProgram.index(dir) == index)
  }

  it should "compute the inputs received" in {
    val testIndex = TestProgram.Index(
      Map(
        'h' -> Map("hello" -> List("index.txt", "sole.txt")),
        'w' -> Map("world" -> List("sole.txt"))
      )
    )
    TestProgram.compute(testIndex, "hello")

    TestProgram.consoleMessages should contain("index.txt : 100.0%")
    TestProgram.consoleMessages should contain("sole.txt : 100.0%")
  }

  it should "get occurrence num for result files" in {
    val resultFiles = List("index_text.txt", "index_text.txt", "intro.txt")
    assert(
      TestProgram.getOccurrenceNum(resultFiles) == Map("intro.txt" -> 1, "index_text.txt" -> 2)
    )
  }

  it should "rank search results correctly and return top 10 results" in {
    val searchResults = Map(
      "index_text.txt"  -> 2,
      "intro.txt"       -> 10,
      "sample.txt"      -> 3,
      "count.txt"       -> 4,
      "forza_spec.txt"  -> 12,
      "lorem_ipsum.txt" -> 5,
      "hello_word.txt"  -> 4,
      "sample2.txt"     -> 3,
      "count2.txt"      -> 7,
      "lists.txt"       -> 15,
      "grocery.txt"     -> 11,
      "wonderwall.txt"  -> 1
    )

    val rankedResults = List(
      TestProgram.Rank("lists.txt",62.5),
      TestProgram.Rank("forza_spec.txt",50.0),
      TestProgram.Rank("grocery.txt",45.9),
      TestProgram.Rank("intro.txt",41.7),
      TestProgram.Rank("count2.txt",29.2),
      TestProgram.Rank("lorem_ipsum.txt",20.9),
      TestProgram.Rank("hello_word.txt",16.7),
      TestProgram.Rank("count.txt",16.7),
      TestProgram.Rank("sample2.txt",12.5),
      TestProgram.Rank("sample.txt",12.5)
    )

    assert(TestProgram.rank(searchResults, 24) == rankedResults)
  }
}
