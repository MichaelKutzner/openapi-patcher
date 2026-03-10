package patcher

class OpenApiPatcherSuite extends munit.FunSuite {
  test("mergedOpenApiSpec alreadyExistingSchemas extendSchemas") {
    val openApiSpec = """
    openapi: 3.0.3
    paths:
      /foo:
        get:
          responses:
            "200":
              content:
                application/json:
                schema:
                  $ref: "./schema.json#/definitions/mobidp.common.Duration"
    components:
      schemas:
        foo:
          type: number
    """
    val schema = """
    {
      "definitions": {
        "mobidp.common.Duration": {
          "type": "number"
        },
        "mobidp.common.String": {
          "type": "string"
        }
      }
    }
    """
    val patchedSpec = OpenApiPatcher(
      parseYaml(openApiSpec),
      parseJson(schema),
    ).fixDuration.mergedOpenApiSpec

    val expected = parseYaml("""
    openapi: 3.1.0
    paths:
      /foo:
        get:
          responses:
            "200":
              content:
                application/json:
                schema:
                  $ref: "#/components/schemas/mobidp.common.Duration"
    components:
      schemas:
        foo:
          type: number
        mobidp.common.Duration:
          type:
            - number
            - string
          pattern: "^P([0-9]+D)?T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$"
        mobidp.common.String:
          type: string
    """).get
    assertEquals(patchedSpec, expected)
  }

  test("mergedOpenApiSpec noExistingSchemas createSchemas") {
    val openApiSpec = """
    paths:
      /foo: {}
    """
    val schema = """
    {
      "definitions": {
        "mobidp.common.Duration": {
          "type": "number"
        },
        "mobidp.common.String": {
          "type": "string"
        }
      }
    }
    """
    val patchedSpec = OpenApiPatcher(
      parseYaml(openApiSpec),
      parseJson(schema),
    ).fixDuration.mergedOpenApiSpec

    val expected = parseYaml("""
    openapi: 3.1.0
    paths:
      /foo: {}
    components:
      schemas:
        mobidp.common.Duration:
          type:
            - number
            - string
          pattern: "^P([0-9]+D)?T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$"
        mobidp.common.String:
          type: string
    """).get
    assertEquals(patchedSpec, expected)
  }

  test("mergedOpenApiSpec withRefsUsingIncorrectPaths updatePaths") {
    val openApiSpec = """
    paths:
      /foo:
        get:
          responses:
            "200":
              content:
                schema:
                  $ref: "./schema.json#/definitions/mobidp.common.Duration"
    """
    val schema = """
    {
      "definitions": {
        "mobidp.common.Duration": {
          "type": "number"
        },
        "mobidp.common.String": {
          "type": "string"
        },
        "foo": {
          "properties": {
            "duration": {"$ref": "#/definitions/mobidp.common.Duration"},
            "text": {"$ref": "#/definitions/mobidp.common.String"}
          }
        },
        "bar": {"$ref": "#/definitions/mobidp.common.String"}
      }
    }
    """
    val patchedSpec = OpenApiPatcher(
      parseYaml(openApiSpec),
      parseJson(schema),
    ).mergedOpenApiSpec

    val expected = parseYaml("""
    openapi: 3.1.0
    paths:
      /foo:
        get:
          responses:
            "200":
              content:
                schema:
                  $ref: "#/components/schemas/mobidp.common.Duration"
    components:
      schemas:
        mobidp.common.Duration:
          type: number
        mobidp.common.String:
          type: string
        foo:
          properties:
            duration:
              $ref: "#/components/schemas/mobidp.common.Duration"
            text:
              $ref: "#/components/schemas/mobidp.common.String"
        bar:
          $ref: "#/components/schemas/mobidp.common.String"
    """).get
    assertEquals(patchedSpec, expected)
  }

  test("integrationTest patchFullExample getExpectedSpecification") {
    def loadFile(path: String): String =
      readFile("src/test/scala/" + path)
    val patcher = OpenApiPatcher(
      parseYaml(
        loadFile("./test_resources/full_example/openapi_specification.yaml"),
      ),
      parseJson(loadFile("./test_resources/full_example/json_schema.json")),
    )

    val fixed = patcher.fixAll

    val expectedSpecification = parseJson(
      loadFile("./test_resources/full_example/expected_specification.json"),
    ).get
    val expectedSchema =
      parseJson(
        loadFile("./test_resources/full_example/expected_schema.json"),
      ).get
    assertEquals(fixed.mergedOpenApiSpec, expectedSpecification)
    assertEquals(fixed.schema, expectedSchema)
  }
}
