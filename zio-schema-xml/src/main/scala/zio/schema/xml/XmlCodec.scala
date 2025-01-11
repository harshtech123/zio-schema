package zio.schema.xml

import scala.xml._
import zio.schema._

object XmlCodec {
  def encode[A](schema: Schema[A], value: A): Elem = {
    schema match {
      case Schema.Primitive(StandardType.StringType, _) => <string>{value.toString}</string>
      case Schema.Primitive(StandardType.IntType, _) => <int>{value.toString}</int>
      case Schema.Primitive(StandardType.BooleanType, _) => <boolean>{value.toString}</boolean>
      case Schema.Primitive(StandardType.DoubleType, _) => <double>{value.toString}</double>
      // Handle other primitive types...
      case Schema.CaseClass1(field, _, _) =>
        <caseClass>
          <field name={field.label}>{encode(field.schema, field.get(value))}</field>
        </caseClass>
      case Schema.Sequence(elementSchema, _, _) =>
        <sequence>
          {value.asInstanceOf[Seq[_]].map(elem => encode(elementSchema, elem))}
        </sequence>
      // Handle other schema types...
      case _ => throw new UnsupportedOperationException("Unsupported schema type")
    }
  }

  def decode[A](schema: Schema[A], xml: Elem): Either[String, A] = {
    schema match {
      case Schema.Primitive(StandardType.StringType, _) => Right(xml.text.asInstanceOf[A])
      case Schema.Primitive(StandardType.IntType, _) => Right(xml.text.toInt.asInstanceOf[A])
      case Schema.Primitive(StandardType.BooleanType, _) => Right(xml.text.toBoolean.asInstanceOf[A])
      case Schema.Primitive(StandardType.DoubleType, _) => Right(xml.text.toDouble.asInstanceOf[A])
      // Handle other primitive types...
      case Schema.CaseClass1(field, construct, _) =>
        val fieldValue = (xml \ "field").text
        decode(field.schema, XML.loadString(fieldValue)).map(construct)
      case Schema.Sequence(elementSchema, _, _) =>
        val elements = (xml \ "sequence" \ "_").map(node => decode(elementSchema, node.asInstanceOf[Elem]))
        if (elements.forall(_.isRight)) Right(elements.map(_.right.get).asInstanceOf[A])
        else Left("Error decoding sequence elements")
      // Handle other schema types...
      case _ => Left("Unsupported schema type")
    }
  }
}