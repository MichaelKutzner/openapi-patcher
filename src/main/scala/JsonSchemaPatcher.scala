package patcher

import io.circe.Json
import io.circe.JsonObject
import io.circe.parser.parse

import io.circe.optics.JsonOptics.*
import io.circe.optics.JsonPath.root

import monocle.function.Plated

case class JsonSchemaPatcher(json: JsonObject):
  def fixMaps: JsonSchemaPatcher =
    _modifyAll { j =>
      j match {
        case BadMap(BadMap(type_ref)) => FixedMap(type_ref).json
        case _                        => j
      }
    }

  def _modifyAll(f: Json => Json): JsonSchemaPatcher =
    JsonSchemaPatcher(
      Plated
        .transform[Json](f)(json.toJson)
        .asObject
        .get,
    )

object JsonSchemaPatcher:
  def fromString(json: String): Option[JsonSchemaPatcher] =
    parseJson(json).map(JsonSchemaPatcher.apply)

case class BadMap(type_ref: String)

object BadMap:
  def unapply(json: Json): Option[BadMap] =
    def createBadMap(ref: String) =
      JsonObject(
        ("type", Json.fromString("object")),
        (
          "properties",
          JsonObject(
            ("@type", createRef(s"#/${definition_path}/mobidp.common.String")),
            ("key", createRef(s"#/${definition_path}/mobidp.common.String")),
            ("value", createRef(ref)),
          ).toJson,
        ),
      ).toJson

    root.properties.value.`$ref`.string
      .getOption(json)
      .filter(_.startsWith(s"#/${definition_path}/"))
      .flatMap { ref =>
        if json == createBadMap(ref) then {
          Some(BadMap(ref))
        } else { None }
      }

case class FixedMap(type_ref: String):
  def json: Json =
    JsonObject(
      ("type", Json.fromString("object")),
      ("additionalProperties" -> JsonObject(
        (
          "type",
          JsonObject(("$ref", Json.fromString(type_ref))).toJson,
        ),
      ).toJson),
    ).toJson

def createRef(ref: String): Json = JsonObject(
  ("$ref", Json.fromString(ref)),
).toJson

val definition_path = "definitions"

// extension (json: Json) def toPatcher = json.asObject.get
// extension (json: Option[Json]) def toPatcher = json.flatMap(_.asObject).get
