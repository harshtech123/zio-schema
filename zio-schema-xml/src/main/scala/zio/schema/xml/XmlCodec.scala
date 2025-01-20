package zio.schema.xml

import scala.xml._
import zio.schema._
import zio.{ZIO, Task}

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
      case Schema.CaseClassN(fields, construct, _) =>
        <caseClass>
          {fields.map(field => <field name={field.label}>{encode(field.schema, field.get(value))}</field>)}
        </caseClass>
      case Schema.Enum1(case1, _) =>
        <enum>
          <case name={case1.id}>{encode(case1.schema, case1.get(value))}</case>
        </enum>
      case Schema.EnumN(cases, _) =>
        <enum>
          {cases.map(caseN => <case name={caseN.id}>{encode(caseN.schema, caseN.get(value))}</case>)}
        </enum>
      case Schema.Optional(elementSchema, _) =>
        value match {
          case Some(v) => encode(elementSchema, v)
          case None => <optional xsi:nil="true"/>
        }
      case Schema.Map(keySchema, valueSchema, _) =>
        <map>
          {value.asInstanceOf[Map[_, _]].map { case (k, v) =>
            <entry>
              <key>{encode(keySchema, k)}</key>
              <value>{encode(valueSchema, v)}</value>
            </entry>
          }}
        </map>
      case Schema.Sequence(elementSchema, _, _) =>
        <sequence>
          {value.asInstanceOf[Seq[_]].map(elem => encode(elementSchema, elem))}
        </sequence>
      case Schema.NonEmptySequence(elementSchema, _, _) =>
        <nonEmptySequence>
          {value.asInstanceOf[Seq[_]].map(elem => encode(elementSchema, elem))}
        </nonEmptySequence>
      case Schema.Set(elementSchema, _) =>
        <set>
          {value.asInstanceOf[Set[_]].map(elem => encode(elementSchema, elem))}
        </set>
      case Schema.Transform(schema, _, g, _) =>
        encode(schema, g(value))
      case Schema.DynamicValue(_) =>
        <dynamic>{value.toString}</dynamic>
      // Handle other schema types...
      case _ => throw new UnsupportedOperationException("Unsupported schema type")
    }
  }

  def decode[A](schema: Schema[A], xml: Elem): Task[A] = {
    schema match {
      case Schema.Primitive(StandardType.StringType, _) => ZIO.succeed(xml.text.asInstanceOf[A])
      case Schema.Primitive(StandardType.IntType, _) => ZIO.succeed(xml.text.toInt.asInstanceOf[A])
      case Schema.Primitive(StandardType.BooleanType, _) => ZIO.succeed(xml.text.toBoolean.asInstanceOf[A])
      case Schema.Primitive(StandardType.DoubleType, _) => ZIO.succeed(xml.text.toDouble.asInstanceOf[A])
      // Handle other primitive types...
      case Schema.CaseClass1(field, construct, _) =>
        val fieldValue = (xml \ "field").text
        decode(field.schema, XML.loadString(fieldValue)).map(construct)
      case Schema.CaseClassN(fields, construct, _) =>
        val fieldValues = fields.map { field =>
          val fieldValue = (xml \ "field").filter(node => (node \@ "name") == field.label).text
          decode(field.schema, XML.loadString(fieldValue))
        }
        ZIO.collectAll(fieldValues).map(values => construct(values: _*))
      case Schema.Enum1(case1, _) =>
        val caseValue = (xml \ "case").text
        decode(case1.schema, XML.loadString(caseValue)).map(case1.construct)
      case Schema.EnumN(cases, _) =>
        val caseNode = (xml \ "case").head
        val caseId = caseNode \@ "name"
        cases.find(_.id == caseId) match {
          case Some(caseN) => decode(caseN.schema, XML.loadString(caseNode.text)).map(caseN.construct)
          case None => ZIO.fail(new UnsupportedOperationException(s"Unknown case: $caseId"))
        }
      case Schema.Optional(elementSchema, _) =>
        if ((xml \@ "xsi:nil") == "true") ZIO.succeed(None.asInstanceOf[A])
        else decode(elementSchema, xml).map(Some(_).asInstanceOf[A])
      case Schema.Map(keySchema, valueSchema, _) =>
        val entries = (xml \ "entry").map { entry =>
          for {
            key <- decode(keySchema, (entry \ "key").head.asInstanceOf[Elem])
            value <- decode(valueSchema, (entry \ "value").head.asInstanceOf[Elem])
          } yield (key, value)
        }
        ZIO.collectAll(entries).map(_.toMap.asInstanceOf[A])
      case Schema.Sequence(elementSchema, _, _) =>
        val elements = (xml \ "sequence" \ "_").map(node => decode(elementSchema, node.asInstanceOf[Elem]))
        ZIO.collectAll(elements).map(_.asInstanceOf[A])
      case Schema.NonEmptySequence(elementSchema, _, _) =>
        val elements = (xml \ "nonEmptySequence" \ "_").map(node => decode(elementSchema, node.asInstanceOf[Elem]))
        ZIO.collectAll(elements).map(_.asInstanceOf[A])
      case Schema.Set(elementSchema, _) =>
        val elements = (xml \ "set" \ "_").map(node => decode(elementSchema, node.asInstanceOf[Elem]))
        ZIO.collectAll(elements).map(_.toSet.asInstanceOf[A])
      case Schema.Transform(schema, f, _, _) =>
        decode(schema, xml).flatMap(a => ZIO.fromEither(f(a)))
      case Schema.DynamicValue(_) =>
        ZIO.succeed(xml.text.asInstanceOf[A])
      // Handle other schema types...
      case _ => ZIO.fail(new UnsupportedOperationException("Unsupported schema type"))
    }
  }
}