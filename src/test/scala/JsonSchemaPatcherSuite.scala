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
            "$$ref": "#/definitions/mobidp.common.${`type`}"
          }
        }
      }
      """)
      assert(expected.isDefined)
      assertEquals(fixed, expected)
    },
  )

  test("fixMaps minimalValueTypeExample keepUnmodified") {
    val json = """
    {
      "type": "object",
      "properties": {
        "value": { "$ref": "#/definitions/mobidp.common.String" }
      }
    }
    """

    val fixed = JsonSchemaPatcher.fromString(json).map(_.fixMaps).map(_.json)

    val expected = parseJson(json)
    assertEquals(fixed, expected)
  }

  test("dropEmptyOverrides withEmptyRedefinedProperties dropAll") {
    val json = """
    {
      "definitions": {
        "root": {
          "properties": {
            "a": { "schema": { "type": "number" }},
            "b": { "schema": { "type": "string" }},
            "c": { "schema": { "type": "string" }}
          }
        },
        "super": {
          "allOf": [{
            "$ref": "#/definitions/root"
          }],
          "properties": {
            "b": { "schema": { "tpe": "number" }},
            "c": { },
            "e": { "schema": { "type": "number" }}
          }
        },
        "derived": {
          "allOf": [{
            "$ref": "#/definitions/super"
          }],
          "properties": {
            "b": { },
            "c": { "schema": { "type": "number" }},
            "e": { },
            "f": { "schema": { "type": "number" }}
          }
        },
        "derived2": {
          "allOf": [{
            "$ref": "#/definitions/root"
          }],
          "properties": {
            "c": { },
            "g": { "schema": { "type": "string" }}
          }
        }
      }
    }
    """

    val fixed =
      JsonSchemaPatcher.fromString(json).map(_.dropEmptyOverrides).map(_.json)

    val expected = parseJson("""
    {
      "definitions": {
        "root": {
          "properties": {
            "a": { "schema": { "type": "number" }},
            "b": { "schema": { "type": "string" }},
            "c": { "schema": { "type": "string" }}
          }
        },
        "super": {
          "allOf": [{
            "$ref": "#/definitions/root"
          }],
          "properties": {
            "b": { "schema": { "tpe": "number" }},
            "e": { "schema": { "type": "number" }}
          }
        },
        "derived": {
          "allOf": [{
            "$ref": "#/definitions/super"
          }],
          "properties": {
            "c": { "schema": { "type": "number" }},
            "f": { "schema": { "type": "number" }}
          }
        },
        "derived2": {
          "allOf": [{
            "$ref": "#/definitions/root"
          }],
          "properties": {
            "g": { "schema": { "type": "string" }}
          }
        }
      }
    }
    """)
    assertEquals(fixed, expected)
  }

  test("fixDuration numberDuration allowISO8601DurationStrings") {
    val json = """
    {
      "definitions": {
        "mobidp.common.Duration": {
          "description": "Keep this",
          "type": "number"
        },
        "foo": { "type": "number" }
      }
    }
    """

    val fixed =
      JsonSchemaPatcher.fromString(json).map(_.fixDuration).map(_.json)

    val expected = parseJson("""
    {
      "definitions": {
        "mobidp.common.Duration": {
          "description": "Keep this",
          "type": ["number", "string"],
          "pattern": "^P([0-9]+D)?T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?$"
        },
        "foo": { "type": "number" }
      }
    }
    """)
    assertEquals(fixed, expected)
  }

  test("fillGeometry incompleteObject useCommonGeometries") {
    val json = """
    {
      "definitions": {
        "mobidp.common.Geometry": {
          "description": "Keep this",
          "oneOf": []
        },
        "foo": { "type": "number" }
      }
    }
    """

    val fixed =
      JsonSchemaPatcher.fromString(json).map(_.fillGeometry).map(_.json)

    val expected = parseJson("""
    {
      "definitions": {
        "mobidp.common.Geometry": {
          "description": "Keep this",
          "oneOf": [
            {"$ref": "#/definitions/mobidp.common.Point"},
            {"$ref": "#/definitions/mobidp.common.MultiPoint"},
            {"$ref": "#/definitions/mobidp.common.LineString"},
            {"$ref": "#/definitions/mobidp.common.Polygon"},
            {"$ref": "#/definitions/mobidp.common.MultiPolygon"}
          ]
        },
        "foo": { "type": "number" }
      }
    }
    """)
    assertEquals(fixed, expected)
  }
}
