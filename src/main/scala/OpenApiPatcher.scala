package patcher

import io.circe.{ACursor, HCursor}
import io.circe.{Json, JsonObject}

import io.circe.optics.JsonOptics.*
import io.circe.optics.JsonPath.root

import monocle.function.Plated

case class OpenApiPatcher(
    openApiSpec: JsonObject,
    schemaPatcher: JsonSchemaPatcher,
):

  def fixAll: OpenApiPatcher =
    fixMaps.dropEmptyOverrides.fixDuration.fillGeometry.dropRedundantNumberRef

  def fixMaps = OpenApiPatcher(openApiSpec, schemaPatcher.fixMaps)

  def dropEmptyOverrides =
    OpenApiPatcher(openApiSpec, schemaPatcher.dropEmptyOverrides)

  def fixDuration = OpenApiPatcher(openApiSpec, schemaPatcher.fixDuration)

  def fillGeometry = OpenApiPatcher(openApiSpec, schemaPatcher.fillGeometry)

  def dropRedundantNumberRef =
    OpenApiPatcher(openApiSpec, schemaPatcher.dropRedundantNumberRef)

  def mergedOpenApiSpec: JsonObject =
    val path = List("components", "schemas")
    val definitions = schemaPatcher.definitions
    // TODO Improve chaining
    _setOpenApiVersion("3.1.0")(
      _patchAllRefs(path)(
        _modifyOrCreate(path)(
          _.flatMap(_.asObject)
            .map(o => JsonObject.fromMap(o.toMap ++ definitions))
            .getOrElse(JsonObject.fromIterable(definitions))
            .toJson,
        ),
      ),
    )

  def schema: JsonObject = schemaPatcher.json

  def _setOpenApiVersion(version: String)(json: JsonObject): JsonObject =
    json.add("openapi", Json.fromString(version))

  def _patchAllRefs(path: List[String])(json: JsonObject): JsonObject =
    val newPath = path.reduceLeft(_ + '/' + _)
    Plated
      .transform[Json](
        _.withObject(o =>
          JsonObject
            .fromMap(
              o.toMap
                .map((k, v) =>
                  k -> Some(v)
                    .filter(_ => k == "$ref")
                    .flatMap(_.asString)
                    .map(s => s"#/${newPath}/${getDefinitionName(s)}")
                    .map(Json.fromString)
                    .getOrElse(v),
                ),
            )
            .toJson,
        ),
      )(json.toJson)
      .asObject
      .get

  def _modifyOrCreate(path: List[String])(
      f: Option[Json] => Json,
  ): JsonObject =
    def helper(path: List[String])(o: Option[Json]): Json =
      path match
        case head :: next =>
          JsonObject
            .fromMap(o.flatMap(_.asObject) match
              case Some(value) =>
                value.toMap.updatedWith(head)(j => Some(helper(next)(j)))
              case None => Map(head -> helper(next)(None)),
            )
            .toJson
        case Nil => f(o)

    helper(path)(Some(openApiSpec.toJson)).asObject.get

object OpenApiPatcher:
  def apply(
      openApiSpec: Option[JsonObject],
      schema: Option[JsonObject],
  ): OpenApiPatcher =
    OpenApiPatcher(openApiSpec.get, JsonSchemaPatcher(schema.get))
