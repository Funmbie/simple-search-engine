package org.SimpleSearch

import java.io.File
import scala.util.Try

object Main extends App {
  Program
    .readFile(args)
    .fold(
      println,
      file => Program.iterate(Program.index(file))
    )
}

object Program extends SimpleSearchT

trait OutputT {
  def println(str: String): Unit = Console.println(str)
}


trait SimpleSearchT extends OutputT {
  import scala.io.StdIn.readLine

  case class Index(files: Map[String, List[String]])
  case class Rank(file: String, rank: Double)

  sealed trait ReadFileError

  case object MissingPathArg extends ReadFileError
  case class NotDirectory(error: String) extends ReadFileError
  case class FileNotFound(t: Throwable) extends ReadFileError

  def readFile(args: Array[String]): Either[ReadFileError, File] = {
    for {
      path <- args.headOption.toRight(MissingPathArg)
      file <- Try(new java.io.File(path))
        .fold(
          throwable => Left(FileNotFound(throwable)),
          file =>
            if (file.isDirectory) Right(file)
            else Left(NotDirectory(s"Path [$path] is not a directory"))
        )
    } yield file
  }

  def index(file: File): Index = {
    val fileIndex = file.listFiles(_.isFile)
      .foldLeft(Map.empty[String, List[String]]) { (map, f) =>
        println(f.getAbsolutePath)
        val source = scala.io.Source.fromFile(f)
        val lines  = try source.getLines mkString "\n" finally source.close()

        val result = for {
          res <- lines.split("\\s") map { word =>
            map get word match {
              case None        => (word.toLowerCase, List(f.getName))
              case Some(paths) => (word.toLowerCase, f.getName :: paths)
            }
          }
        } yield res

        result.foldLeft(map)(_ + _)
    }
    Index(fileIndex)
  }

  def iterate(indexedFiles: Index): Unit = {
    print(s"search> ")
    val searchString = readLine()

    if(searchString == ":quit") System exit 0

    compute(indexedFiles, searchString)
    iterate(indexedFiles)
  }

  def compute(indexedFiles: Index, searchString: String): Unit = {
    val query        = searchString.split("\\s")
    val unrankedRes  = query.foldLeft(List.empty[String])(
      (files, word) =>
        indexedFiles.files.get(word.toLowerCase)
          .fold(files)
          (x => x ::: files)
    )
    val rankedRes = rank(getOccurrenceNum(unrankedRes), query.length)
    if (rankedRes.isEmpty)
      println("No results: ")
    else {
      rankedRes foreach (r => println(s"${r.file} : ${r.rank}%"))
    }
  }

  def getOccurrenceNum(resultFiles: List[String]): Map[String, Int] =
    resultFiles
      .foldLeft(Map.empty[String, Int])(
        (map, index) =>
          if (map contains index) map
          else map.updated(
            index,
            resultFiles.count(_ == index)
          )
      )

  def rank(searchResults: Map[String, Int], queryCount: Int): List[Rank] =
    searchResults.toSeq.sortBy(_._2).reverse.take(10)
      .map{ case (name, occ) =>
        val rank = (occ.toDouble / queryCount.toDouble) * 100
        Rank(name, BigDecimal(rank).setScale(1, BigDecimal.RoundingMode.UP).toDouble)
      }.toList
}