package patcher

import scala.annotation.tailrec

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

  def dropEmptyOverrides: JsonSchemaPatcher =
    def isEmpty(o: Json): Boolean =
      o.asObject.map(_.isEmpty).getOrElse(false)
    JsonSchemaPatcher(
      _forEachDefinition(o =>
        val derivedProperties = _derivedProperties(o)
        _forEachProperty((key, value) =>
          if isEmpty(value) && derivedProperties.contains(key) then { None }
          else { Some(value) },
        )(o),
      )(json),
    )

  def _modifyAll(f: Json => Json): JsonSchemaPatcher =
    JsonSchemaPatcher(
      Plated
        .transform[Json](f)(json.toJson)
        .asObject
        .get,
    )

  def _forEachDefinition(f: JsonObject => JsonObject)(
      j: JsonObject,
  ): JsonObject =
    def modifyEachDefinition(o: JsonObject): Json =
      o.mapValues(o => f(o.asObject.get).toJson).toJson
    j.toJson.hcursor
      .downField(definition_path)
      .withFocus(_.withObject(modifyEachDefinition))
      .top
      .flatMap(_.asObject)
      .get

  def _forEachProperty(f: ((String, Json)) => Option[Json])(
      j: JsonObject,
  ): JsonObject =
    def modifyEachProperty(o: JsonObject): Json =
      JsonObject
        .fromIterable(
          o.toMap.flatMap((k, v) =>
            f(k -> v) match
              case Some(res) => Some(k -> res)
              case None      => None,
          ),
        )
        .toJson
    j.toJson.hcursor
      .downField("properties")
      .withFocus(_.withObject(modifyEachProperty))
      .top
      .flatMap(_.asObject)
      .get

  def _derivedProperties(o: JsonObject): List[String] =
    def getParents(o: Json): List[String] =
      root.allOf.each.`$ref`.as[String]
        .getAll(o)
        .map(getDefinitionName)
    def getProperties(definition: String) =
      _getDefinition(definition).asObject
        .flatMap(
          _("properties").flatMap(_.asObject),
        )
        .toList
        .flatMap(_.keys)
    @tailrec
    def collectProperties(
        definitions: List[String],
        properties: List[String],
    ): List[String] =
      definitions match
        case x :: xs =>
          collectProperties(
            getParents(_getDefinition(x)) ::: xs,
            getProperties(x) ::: properties,
          )
        case Nil => properties
    collectProperties(getParents(o.toJson), List())

  def _getDefinition(definition: String): Json =
    json.toJson.hcursor
      .downField(definition_path)
      .downField(definition)
      .focus
      .get

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
            ("@type", createRef(createDefinition("mobidp.common.String"))),
            ("key", createRef(createDefinition("mobidp.common.String"))),
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

def getDefinitionName(s: String): String =
  s.reverse.takeWhile(_ != '/').reverse

def createDefinition(definition: String): String =
  s"#/${definition_path}/${definition}"

val definition_path = "definitions"

// extension (json: Json) def toPatcher = json.asObject.get
// extension (json: Option[Json]) def toPatcher = json.flatMap(_.asObject).get
