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
    root.properties.value.`$ref`.string
      .getOption(json)
      .flatMap {
        case ref @ s"#/definitions/${t}" => Some(BadMap(ref))
        case _                           => None
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

// extension (json: Json) def toPatcher = json.asObject.get
// extension (json: Option[Json]) def toPatcher = json.flatMap(_.asObject).get
