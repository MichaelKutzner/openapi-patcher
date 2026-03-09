package patcher

import io.circe.JsonObject
import io.circe.{parser as json_parser}
import io.circe.yaml.{parser as yaml_parser}

def parseJson(json: String): Option[JsonObject] =
  json_parser.parse(json).toOption.flatMap(_.asObject)

def parseYaml(yaml: String): Option[JsonObject] =
  yaml_parser.parse(yaml).toOption.flatMap(_.asObject)
