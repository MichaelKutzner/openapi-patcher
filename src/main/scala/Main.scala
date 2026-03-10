package patcher

@main def patch(args: String*): Unit =
  val options = Options(
    args.toList ::: List(
      "-a",
      "api.yaml",
      "-j",
      "schema.json",
      "-o",
      "patched-",
    ),
  )
  val openapiSpec = parseYaml(readFile(options.openApiSpecPath)).get
  val schema = parseJson(readFile(options.jsonSchemaPath)).get
  val patcher = OpenApiPatcher(openapiSpec, JsonSchemaPatcher(schema))
  val patched = patcher.fixAll
  println(options)
  writeFile(options.outputOpenApiSpecPath, patched.mergedOpenApiSpec)
  writeFile(options.outputJsonSchemaPath, patched.schema)
