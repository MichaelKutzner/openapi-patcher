package patcher

@main def patch(args: String*): Unit =
  val options = Options(
    List(
      "-a",
      "api.yaml",
      "-j",
      "schema.json",
      "-o",
      "patched-",
    ) ::: args.toList,
  )
  val openapiSpec = parseFile(options.openApiSpecPath)
  val schema = parseFile(options.jsonSchemaPath)
  if openapiSpec.isEmpty then {
    println(s"Failed to load OpenApiSpecification '${options.openApiSpecPath}'")
    return ()
  }
  if schema.isEmpty then {
    println(s"Failed to load JsonSchema '${options.jsonSchemaPath}'")
    return ()
  }
  val patcher = OpenApiPatcher(openapiSpec.get, JsonSchemaPatcher(schema.get))
  val patched = patcher.fixAll
  println(options)
  writeFile(options.outputOpenApiSpecPath, patched.mergedOpenApiSpec)
  writeFile(options.outputJsonSchemaPath, patched.schema)
