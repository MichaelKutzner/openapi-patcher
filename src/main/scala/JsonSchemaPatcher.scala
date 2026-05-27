package patcher

import scala.annotation.tailrec

import io.circe.{Json, JsonObject}
import io.circe.parser.parse

import io.circe.optics.JsonOptics.*
import io.circe.optics.JsonPath.root

import monocle.function.Plated

case class JsonSchemaPatcher(json: JsonObject):

  def fixBrokenDefinitions: JsonSchemaPatcher =
    _modifyDefinition("mobidp.portal.Subscriber")(
      _.+:(
        "properties" -> Json.obj(
          "TODO" -> Json.obj(
            "description" -> Json.fromString(
              "TODO Placeholder propertery to compile",
            ),
            "type" -> Json.fromString("string"),
          ),
        ),
      ),
    )

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
      List(
        "Point",
        "MultiPoint",
        "LineString",
        "MultiLineString",
        "Polygon",
        "MultiPolygon",
      )
    val geometriesMapping =
      geometries.map(geometry => s"mobidp.common.${geometry}" -> geometry)
    // Add discriminator to Geometry
    val schemaWithGeometryDiscrimitator =
      _modifyDefinition("mobidp.common.Geometry")((o: JsonObject) =>
        def geometryOptions =
          Some(
            Json.arr(
              geometriesMapping
                .map((definition, geometry) =>
                  createRef(createDefinition(definition)),
                )
                .toSeq*,
            ),
          )
        JsonObject.fromMap(
          o.toMap
            .updatedWith("oneOf")(_ match
              case Some(value) =>
                if value.asArray.map(_.isEmpty).getOrElse(true) then {
                  geometryOptions
                } else { Some(value) }
              case None => geometryOptions,
            )
            .+(
              "discriminator" -> Json.obj(
                "propertyName" -> Json.fromString("type"),
                "mapping" -> Json.obj(
                  geometriesMapping
                    .map((definition, `type`) =>
                      `type` -> Json.fromString(createDefinition(definition)),
                    )
                    .toSeq*,
                ),
              ),
            ),
        ),
      )
    // Update each geometry
    val geometriesMap = geometriesMapping.toMap
    JsonSchemaPatcher(
      schemaWithGeometryDiscrimitator._forEachDefinition(
        // Add 'type' property with default for each geometry
        (ref, definition) =>
          if geometriesMap.isDefinedAt(ref) then {
            definition
              // Override property 'type' and set default
              .+:(
                "properties" -> definition
                  .apply("properties")
                  .flatMap(_.asObject)
                  .orElse(Some(JsonObject()))
                  .map(_.+:("type" -> createEnum(geometriesMap(ref))))
                  .get
                  .toJson,
              )
              // Remove 'type' from required properties
              .+:(
                "required" -> definition
                  .apply("required")
                  .orElse(Some(Json.arr()))
                  .map(
                    _.withArray(reqs =>
                      Json.arr(reqs.filterNot(_ == Json.fromString("type"))*),
                    ),
                  )
                  .get,
              )
          } else { definition },
      ),
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

  def createKStoreValueObject: JsonSchemaPatcher =
    val refs = _mapEachDefinition((definitionName, definition) =>
      if _getDerivedParents(definition).contains(value_object_name) then {
        Some(definitionName)
      } else { None },
    )
    // Create object with discriminator '@type' and explicit mapping
    val withKStoreSchema = json.toJson.hcursor
      .downField(definition_path)
      .withFocus(
        _.withObject(obj =>
          obj
            .+:(
              kstore_value_name -> Json.obj(
                "oneOf" -> Json.arr(refs.map(createRef)*),
                "discriminator" -> Json.obj(
                  "propertyName" -> Json.fromString("@type"),
                  "mapping" -> Json.obj(
                    refs
                      .map(ref =>
                        ref + "$Bean" -> Json.fromString(createDefinition(ref)),
                      )
                      .toSeq*,
                  ),
                ),
              ),
            )
            .toJson,
        ),
      )
      .top
      .flatMap(_.asObject)
      .get
    JsonSchemaPatcher(
      JsonSchemaPatcher(withKStoreSchema)._forEachDefinition(
        // Add '@type' property with default for each allowed discriminator object
        (ref, definition) =>
          if refs.contains(ref) then {
            definition
              .+:(
                "properties" -> definition
                  .apply("properties")
                  .flatMap(_.asObject)
                  .orElse(Some(JsonObject()))
                  .map(_.+:("@type" -> createEnum(ref + "$Bean")))
                  .get
                  .toJson,
              )
          } else { definition },
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

  def _mapEachDefinition[A](f: (String, JsonObject) => Option[A]): Seq[A] =
    json.toJson.hcursor
      .downField(definition_path)
      .focus
      .flatMap(_.asObject)
      .toSeq
      .flatMap(
        _.toMap
          .flatMap((key, value) => value.asObject.flatMap(o => f(key, o)))
          .toSeq,
      )

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
    _getDerivedParents(o).flatMap(_getProperties)

  def _getDerivedParents(o: JsonObject): List[String] =
    @tailrec
    def helper(candidates: List[Json], allParents: List[String]): List[String] =
      candidates match
        case head :: next =>
          val parentNames =
            root.allOf.each.`$ref`.as[String]
              .getAll(head)
              .map(getDefinitionName)
          val parentDefinitions = parentNames.map(_getDefinition)
          helper(parentDefinitions ::: next, parentNames ::: allParents)
        case scala.collection.immutable.Nil => allParents
    helper(List(o.toJson), List())

  def _getProperties(definition: String): List[String] =
    _getDefinition(definition).asObject
      .flatMap(
        _("properties").flatMap(_.asObject),
      )
      .toList
      .flatMap(_.keys)

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

def createEnum(definition: String): Json =
  val disc = Json.fromString(definition)
  Json.obj(
    "enum" -> Json.arr(disc),
    "default" -> disc,
  )

val definition_path = "definitions"
val value_object_name = "mobidp.persistence.ValueObject"
val kstore_value_name = "KStoreValue"
