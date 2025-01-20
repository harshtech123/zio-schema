package zio.schema.xml

import zio.schema._
import zio.{Runtime, Task, ZIO}

object XmlCodecApp extends App {
  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val runtime = Runtime.default

    // Test encoding and decoding a string
    val stringSchema = Schema.Primitive(StandardType.StringType)
    val stringValue = "hello"
    val stringXml = XmlCodec.encode(stringSchema, stringValue)
    println(s"Encoded string: $stringXml")
    val decodedString = runtime.unsafeRun(XmlCodec.decode(stringSchema, stringXml))
    println(s"Decoded string: $decodedString")

    // Test encoding and decoding an int
    val intSchema = Schema.Primitive(StandardType.IntType)
    val intValue = 42
    val intXml = XmlCodec.encode(intSchema, intValue)
    println(s"Encoded int: $intXml")
    val decodedInt = runtime.unsafeRun(XmlCodec.decode(intSchema, intXml))
    println(s"Decoded int: $decodedInt")

    // Test encoding and decoding a case class
    case class Person(name: String, age: Int)
    val personSchema = Schema.CaseClass2(
      field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
      field2 = Schema.Field("age", Schema.Primitive(StandardType.IntType)),
      construct = (name, age) => Person(name, age),
      extractField1 = _.name,
      extractField2 = _.age
    )
    val personValue = Person("John", 30)
    val personXml = XmlCodec.encode(personSchema, personValue)
    println(s"Encoded person: $personXml")
    val decodedPerson = runtime.unsafeRun(XmlCodec.decode(personSchema, personXml))
    println(s"Decoded person: $decodedPerson")

    // Test encoding and decoding an enum
    sealed trait Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
    val colorSchema = Schema.EnumN(
      List(
        Schema.Case("Red", Schema.singleton(Red), _.asInstanceOf[Color] == Red, _.asInstanceOf[Color]),
        Schema.Case("Green", Schema.singleton(Green), _.asInstanceOf[Color] == Green, _.asInstanceOf[Color]),
        Schema.Case("Blue", Schema.singleton(Blue), _.asInstanceOf[Color] == Blue, _.asInstanceOf[Color])
      )
    )
    val colorValue = Red
    val colorXml = XmlCodec.encode(colorSchema, colorValue)
    println(s"Encoded color: $colorXml")
    val decodedColor = runtime.unsafeRun(XmlCodec.decode(colorSchema, colorXml))
    println(s"Decoded color: $decodedColor")

    // Test encoding and decoding a map
    val mapSchema = Schema.Map(Schema.Primitive(StandardType.StringType), Schema.Primitive(StandardType.IntType))
    val mapValue = Map("one" -> 1, "two" -> 2)
    val mapXml = XmlCodec.encode(mapSchema, mapValue)
    println(s"Encoded map: $mapXml")
    val decodedMap = runtime.unsafeRun(XmlCodec.decode(mapSchema, mapXml))
    println(s"Decoded map: $decodedMap")

    ZIO.succeed(0)
  }
}
