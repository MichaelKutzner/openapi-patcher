package patcher

class ParserSuite extends munit.FunSuite {
  test("serialize serializeAndParseExampleObject getSameObject") {
    val example = parseJson("""
    {
      "text": "Hello, World!",
      "number": 1234.6789,
      "bool": true,
      "object": {
        "a": 1,
        "b": "9"
      },
      "array": [2, 3, 5, 7]
    }
    """).get

    val deserialized = parseJson(serialize(example))

    assert(deserialized.isDefined)
    assertEquals(deserialized.get, example)
  }
}
