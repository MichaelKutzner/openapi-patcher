package patcher

import io.circe.{ACursor, HCursor}
import io.circe.{Json, JsonObject}

import io.circe.optics.JsonOptics.*
import io.circe.optics.JsonPath.root

import monocle.function.Plated

case class OpenApiPatcher(
    openApiSpec: JsonObject,
    schemaPatcher: JsonSchemaPatcher,
    schemaWithFixedDuration: Boolean = false,
):

  def fixAll: OpenApiPatcher =
    dropProblematicEndpoints.fixBrokenDefinitions.fixMaps.dropEmptyOverrides.fixDuration.fillGeometry.dropRedundantNumberRef.updateValueObject

  def dropProblematicEndpoints =
    val newOpenApiSpec =
      root.paths.obj
        .modify(o =>
          o.filterKeys(key =>
            !List("/backup", "/restore", "/store/{table}/{identifier}")
              .contains(key),
          ),
        )(
          openApiSpec.toJson,
        )
        .asObject
        .get
    copy(openApiSpec = newOpenApiSpec)

  def fixBrokenDefinitions =
    copy(schemaPatcher = schemaPatcher.fixBrokenDefinitions)

  def fixMaps = copy(schemaPatcher = schemaPatcher.fixMaps)

  def dropEmptyOverrides =
    copy(schemaPatcher = schemaPatcher.dropEmptyOverrides)

  def fixDuration =
    this.copy(schemaWithFixedDuration = true)

  def fillGeometry = copy(schemaPatcher = schemaPatcher.fillGeometry)

  def dropRedundantNumberRef =
    copy(schemaPatcher = schemaPatcher.dropRedundantNumberRef)

  def updateValueObject =
    val suffix = "/" + value_object_name
    val patchedSpec = Plated
      .transform[Json](
        _.withObject(o =>
          Json.obj(
            o.toMap
              .map((key, value) =>
                key -> Some(value)
                  .filter(_ => key == "$ref")
                  .filter(_ =>
                    value.asString
                      .map(_.endsWith(suffix))
                      .getOrElse(false),
                  )
                  .map(
                    _.withString(s =>
                      Json.fromString(
                        s.dropRight(suffix.length) + "/" + kstore_value_name,
                      ),
                    ),
                  )
                  .getOrElse(value),
              )
              .toSeq*,
          ),
        ),
      )(openApiSpec.toJson)
      .asObject
      .get
    copy(schemaPatcher = schemaPatcher.createKStoreValueObject)

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

  def schema: JsonObject = _applySchemaFixes(schemaPatcher).json

  def _applySchemaFixes(patcher: JsonSchemaPatcher): JsonSchemaPatcher =
    if schemaWithFixedDuration then { patcher.fixDuration }
    else { patcher }

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
