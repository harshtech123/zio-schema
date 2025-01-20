package zio.schema.xml

import zio.test._
import zio.test.Assertion._
import zio.schema._
import zio.ZIO

object XmlCodecSpec extends DefaultRunnableSpec {
  def spec = suite("XmlCodecSpec")(
    test("encode and decode string") {
      val schema = Schema.Primitive(StandardType.StringType)
      val value = "hello"
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode int") {
      val schema = Schema.Primitive(StandardType.IntType)
      val value = 42
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode boolean") {
      val schema = Schema.Primitive(StandardType.BooleanType)
      val value = true
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode double") {
      val schema = Schema.Primitive(StandardType.DoubleType)
      val value = 3.14
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
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
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode complex case class") {
      case class Address(street: String, city: String)
      case class Person(name: String, age: Int, address: Address)
      val addressSchema = Schema.CaseClass2(
        field1 = Schema.Field("street", Schema.Primitive(StandardType.StringType)),
        field2 = Schema.Field("city", Schema.Primitive(StandardType.StringType)),
        construct = (street, city) => Address(street, city),
        extractField1 = _.street,
        extractField2 = _.city
      )
      val personSchema = Schema.CaseClass3(
        field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
        field2 = Schema.Field("age", Schema.Primitive(StandardType.IntType)),
        field3 = Schema.Field("address", addressSchema),
        construct = (name, age, address) => Person(name, age, address),
        extractField1 = _.name,
        extractField2 = _.age,
        extractField3 = _.address
      )
      val value = Person("John", 30, Address("123 Main St", "Anytown"))
      val xml = XmlCodec.encode(personSchema, value)
      assertM(XmlCodec.decode(personSchema, xml))(equalTo(value))
    },
    test("encode and decode enum") {
      sealed trait Color
      case object Red extends Color
      case object Green extends Color
      case object Blue extends Color
      val schema = Schema.EnumN(
        List(
          Schema.Case("Red", Schema.singleton(Red), _.asInstanceOf[Color] == Red, _.asInstanceOf[Color]),
          Schema.Case("Green", Schema.singleton(Green), _.asInstanceOf[Color] == Green, _.asInstanceOf[Color]),
          Schema.Case("Blue", Schema.singleton(Blue), _.asInstanceOf[Color] == Blue, _.asInstanceOf[Color])
        )
      )
      val value = Red
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode optional field") {
      val schema = Schema.Optional(Schema.Primitive(StandardType.StringType))
      val value = Some("hello")
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode None optional field") {
      val schema = Schema.Optional(Schema.Primitive(StandardType.StringType))
      val value = None
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode map") {
      val schema = Schema.Map(Schema.Primitive(StandardType.StringType), Schema.Primitive(StandardType.IntType))
      val value = Map("one" -> 1, "two" -> 2)
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode sequence") {
      val schema = Schema.Sequence(Schema.Primitive(StandardType.IntType))
      val value = Seq(1, 2, 3)
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode empty sequence") {
      val schema = Schema.Sequence(Schema.Primitive(StandardType.IntType))
      val value = Seq.empty[Int]
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode non-empty sequence") {
      val schema = Schema.NonEmptySequence(Schema.Primitive(StandardType.IntType))
      val value = Seq(1, 2, 3)
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode set") {
      val schema = Schema.Set(Schema.Primitive(StandardType.IntType))
      val value = Set(1, 2, 3)
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode transform schema") {
      val schema = Schema.Transform(Schema.Primitive(StandardType.StringType), (s: String) => Right(s.toInt), (i: Int) => i.toString)
      val value = 42
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("encode and decode dynamic value") {
      val schema = Schema.DynamicValue(Schema.Primitive(StandardType.StringType))
      val value = "dynamic"
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    test("decode invalid XML") {
      val schema = Schema.Primitive(StandardType.StringType)
      val xml = <invalid></invalid>
      assertM(XmlCodec.decode(schema, xml).either)(isLeft)
    }
  )
}