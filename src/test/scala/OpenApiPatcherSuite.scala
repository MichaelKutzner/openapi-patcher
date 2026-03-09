package patcher

class OpenApiPatcherSuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("mergedOpenApiSpec alreadyExistingSchemas extendSchemas") {
    val openApiSpec = """
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
        mobidp.common.Duration:
          type:
            - number
            - string
          pattern: "^P([0-9]+D)?([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$"
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
    paths:
      /foo: {}
    components:
      schemas:
        mobidp.common.Duration:
          type:
            - number
            - string
          pattern: "^P([0-9]+D)?([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$"
        mobidp.common.String:
          type: string
    """).get
    assertEquals(patchedSpec, expected)
  }

  test("integrationTest patchFullExample getExpectedSpecification".ignore) {
    ???
  }
}
