package patcher

import java.io.FileWriter

import scala.io.Source

import io.circe.JsonObject

def readFile(path: String): String =
  Source.fromFile(path).getLines.mkString("\n")

def writeFile(path: String, content: JsonObject): Unit =
  val writer = FileWriter(path)
  try {
    writer.write(content.toString())
  } finally {
    writer.close()
  }
