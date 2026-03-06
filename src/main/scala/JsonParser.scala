package patcher

import io.circe.JsonObject
import io.circe.parser.parse

def parseJson(json: String): Option[JsonObject] =
  parse(json).toOption.flatMap(_.asObject)

// trait JsonLoader:
//   def load_json(path: String): Json
