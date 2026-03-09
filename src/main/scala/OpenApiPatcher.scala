package patcher

import io.circe.{ACursor, HCursor}
import io.circe.{Json, JsonObject}

import io.circe.optics.JsonOptics.*
import io.circe.optics.JsonPath.root
import scala.annotation.tailrec

case class OpenApiPatcher(
    openApiSpec: JsonObject,
    schemaPatcher: JsonSchemaPatcher,
):
  def fixDuration = OpenApiPatcher(openApiSpec, schemaPatcher.fixDuration)

  def mergedOpenApiSpec: JsonObject =
    _modifyOrCreate(List("components", "schemas"))(
      _.flatMap(_.asObject)
        .map(o => JsonObject.fromMap(o.toMap ++ schemaPatcher.definitions))
        .getOrElse(JsonObject.fromIterable(schemaPatcher.definitions))
        .toJson,
    )

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
