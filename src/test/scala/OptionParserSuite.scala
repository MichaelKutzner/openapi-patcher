package patcher

class OptionParserSuite extends munit.FunSuite {
  test("parse longOptions getOptions") {
    val options = Options(
      List(
        "--openapi-spec",
        "my-openapi.yaml",
        "--json-schema",
        "my-schema.json",
        "--output",
        "patched-",
      ),
    )

    val expected = Options(
      "my-openapi.yaml",
      "my-schema.json",
      "patched-openapi.json",
      "patched-schema.json",
    )
    assertEquals(options, expected)
  }

  test("parse shortOptions getOptions") {
    val options = Options(
      List(
        "-a",
        "resources/openapi.yaml",
        "-j",
        "resources/schema.json",
        "-o",
        "out/",
      ),
    )

    val expected = Options(
      "resources/openapi.yaml",
      "resources/schema.json",
      "out/openapi.json",
      "out/schema.json",
    )
    assertEquals(options, expected)
  }
}
