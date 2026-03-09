package patcher

import scala.annotation.tailrec

case class Options(
    openApiSpecPath: String,
    jsonSchemaPath: String,
    outputOpenApiSpecPath: String,
    outputJsonSchemaPath: String,
)

object Options:
  def apply(args: Seq[String]): Options =
    case class OptionBuilder(
        openApiSpecPath: Option[String] = None,
        jsonSchemaPath: Option[String] = None,
        outputPrefixPath: Option[String] = None,
    )
    @tailrec
    def helper(args: Seq[String], builder: OptionBuilder): OptionBuilder =
      args match
        case "--openapi-spec" :: path :: tail =>
          helper(tail, builder.copy(openApiSpecPath = Some(path)))
        case "-a" :: path :: tail =>
          helper(tail, builder.copy(openApiSpecPath = Some(path)))
        case "--json-schema" :: path :: tail =>
          helper(tail, builder.copy(jsonSchemaPath = Some(path)))
        case "-j" :: path :: tail =>
          helper(tail, builder.copy(jsonSchemaPath = Some(path)))
        case "--output" :: path :: tail =>
          helper(tail, builder.copy(outputPrefixPath = Some(path)))
        case "-o" :: path :: tail =>
          helper(tail, builder.copy(outputPrefixPath = Some(path)))
        case Nil => builder
        // case arg => throw RuntimeException(s"Invalid argument '${arg}'")
    val builder = helper(args.toList, OptionBuilder())
    Options(
      builder.openApiSpecPath.get,
      builder.jsonSchemaPath.get,
      builder.outputPrefixPath.get + "openapi.json",
      builder.outputPrefixPath.get + "schema.json",
    )
