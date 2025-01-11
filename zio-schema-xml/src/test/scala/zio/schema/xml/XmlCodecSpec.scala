package zio.schema.xml

import zio.test._
import zio.test.Assertion._
import zio.schema._

object XmlCodecSpec extends DefaultRunnableSpec {
  def spec = suite("XmlCodecSpec")(
    test("encode and decode string") {
      val schema = Schema.Primitive(StandardType.StringType)
      val value = "hello"
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    },
    test("encode and decode int") {
      val schema = Schema.Primitive(StandardType.IntType)
      val value = 42
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    },
    test("encode and decode boolean") {
      val schema = Schema.Primitive(StandardType.BooleanType)
      val value = true
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    },
    test("encode and decode double") {
      val schema = Schema.Primitive(StandardType.DoubleType)
      val value = 3.14
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    },
    test("encode and decode case class") {
      case class Person(name: String, age: Int)
      val schema = Schema.CaseClass2(
        field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
        field2 = Schema.Field("age", Schema.Primitive(StandardType.IntType)),
        construct = (name, age) => Person(name, age),
        extractField1 = _.name,
        extractField2 = _.age
      )
      val value = Person("John", 30)
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    },
    test("encode and decode sequence") {
      val schema = Schema.Sequence(Schema.Primitive(StandardType.IntType))
      val value = Seq(1, 2, 3)
      val xml = XmlCodec.encode(schema, value)
      assert(XmlCodec.decode(schema, xml))(isRight(equalTo(value)))
    }
  )
}