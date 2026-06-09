package patcher

import java.io.FileWriter

import scala.io.Source

import io.circe.JsonObject
import io.circe.yaml.scalayaml.printer

def readFile(path: String): String =
  Source.fromFile(path).getLines.mkString("\n")

def writeFile(path: String, content: JsonObject): Unit =
  val writer = FileWriter(path)
  try {
    if path.endsWith("yaml") then {
      writer.write(printer.print(content.toJson))
    } else {
      writer.write(serialize(content))
    }
  } finally {
    writer.close()
  }
