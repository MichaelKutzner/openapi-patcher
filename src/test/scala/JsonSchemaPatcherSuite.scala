package patcher

import io.circe.parser.parse

class JsonSchemaPatcherSuite extends munit.FunSuite {
  val map_types = List("String", "Integer")

  map_types.map(`type` =>
    test(s"fixMaps incorrectMapForType`${`type`}` replaceWithCorrectMap") {
      val fixed = JsonSchemaPatcher
        .fromString(s"""
        {
          "ok": {
            "key": "K",
            "value": "V"
          },
          "broken": {
            "type": "object",
            "properties": {
              "@type": { "$$ref": "#/definitions/mobidp.common.String" },
              "key": { "$$ref": "#/definitions/mobidp.common.String" },
              "value": { "$$ref": "#/definitions/mobidp.common.${`type`}" }
            }
          }
        }
        """)
        .map(_.fixMaps)
        .map(_.json)
      val expected = parseJson(s"""
      {
        "ok": {
          "key": "K",
          "value": "V"
        },
        "broken": {
          "type": "object",
          "additionalProperties": {
            "type": { "$$ref": "#/definitions/mobidp.common.${`type`}" }
          }
        }
      }
      """)
      assert(expected.isDefined)
      assertEquals(fixed, expected)
    },
  )
}
