package patcher

import scala.annotation.tailrec

import io.circe.{Json, JsonObject}
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
      _forEachDefinition((_, o) =>
        val derivedProperties = _derivedProperties(o)
        _forEachProperty((key, value) =>
          if isEmpty(value) && derivedProperties.contains(key) then { None }
          else { Some(value) },
        )(o),
      ),
    )

  def fixDuration: JsonSchemaPatcher =
    _modifyDefinition("mobidp.common.Duration")(
      _.+:(
        "type" -> Json.arr(Json.fromString("number"), Json.fromString("string")),
      ).+:(
        "pattern" -> Json.fromString(
          "^P([0-9]+D)?T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$",
        ),
      ),
    )

  def fillGeometry: JsonSchemaPatcher =
    val geometries =
      List("Point", "MultiPoint", "LineString", "Polygon", "MultiPolygon")
    _modifyDefinition("mobidp.common.Geometry")((o: JsonObject) =>
      def geometryOptions =
        Some(
          Json.arr(
            geometries
              .map(geometry =>
                createRef(createDefinition(s"mobidp.common.${geometry}")),
              )*,
          ),
        )
      JsonObject.fromMap(o.toMap.updatedWith("oneOf")(_ match
        case Some(value) =>
          if value.asArray.map(_.isEmpty).getOrElse(true) then {
            geometryOptions
          } else { Some(value) }
        case None => geometryOptions,
      )),
    )

  def dropRedundantNumberRef: JsonSchemaPatcher =
    /** Using a double specified number type with objects using 'oneOf' can
      * currently result in an unexpected class being generated. This might be a
      * bug of the generator.
      */
    def isNumber(o: JsonObject) =
      o("type").flatMap(_.asString).map(_ == "number").getOrElse(false)
    def isNumberRef(o: JsonObject) =
      val numberRef =
        Json.arr(createRef(createDefinition("mobidp.common.Decimal")))
      o("allOf").map(_ == numberRef).getOrElse(false)
    JsonSchemaPatcher(
      _forEachDefinition((key, value) =>
        if isNumber(value) && isNumberRef(value) then { value.remove("allOf") }
        else { value },
      ),
    )

  def definitions: List[(String, Json)] =
    json.toJson.hcursor
      .downField(definition_path)
      .focus
      .flatMap(_.asObject)
      .toList
      .flatMap(_.toList)

  def _modifyAll(f: Json => Json): JsonSchemaPatcher =
    JsonSchemaPatcher(
      Plated
        .transform[Json](f)(json.toJson)
        .asObject
        .get,
    )

  def _modifyDefinition(definition: String)(
      f: JsonObject => JsonObject,
  ): JsonSchemaPatcher =
    JsonSchemaPatcher(
      _forEachDefinition((key, value) =>
        key match
          case `definition` => f(value)
          case _            => value,
      ),
    )

  def _forEachDefinition(f: (String, JsonObject) => JsonObject): JsonObject =
    def modifyEachDefinition(o: JsonObject): Json =
      JsonObject
        .fromMap(o.toMap.map((k, v) => (k -> f(k, v.asObject.get).toJson)))
        .toJson
    json.toJson.hcursor
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
      .getOrElse(j)

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
      (
        "additionalProperties" ->
          JsonObject(("$ref", Json.fromString(type_ref))).toJson,
      ),
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
