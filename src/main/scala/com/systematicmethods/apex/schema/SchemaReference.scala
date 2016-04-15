package com.systematicmethods.apex.schema

import java.io.Reader
import org.apache.commons.csv._
import scala.reflect.runtime.{universe => ru}
import org.apache.avro.SchemaBuilder
import org.apache.avro.generic.GenericData
import org.apache.avro.generic.GenericRecord
import org.apache.avro.Schema.Type
import org.apache.avro.Schema
import scala.collection.JavaConverters._
import java.io.InputStream
import java.io.InputStreamReader

//case class Entity_Out_Relationship(entity_start:String, relationship:String, entity_end:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Defined_By_Group(entity:String, group:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Relationship_Defined_By_Group(relationship:String, group:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class String_Composed_Of_Property(group:String, property:String, data_type:String, optionality:Boolean, category:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Identified_By_Property(entity:String, property:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Indexed_By_Property(entity:String, property:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship

trait SchemaReference {
  def records:Map[String, GenericRecord]
  def size:Int = records.size
}

case class Entities(records:Map[String, GenericRecord]) extends SchemaReference 
object Entities {
  def typeName = "Entity"
  def primaryKey = List("Entity")
  def apply(stream:InputStream): Entities = Entities(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
}

case class Relationships(records:Map[String, GenericRecord]) extends SchemaReference 
object Relationships {
  def apply(stream:InputStream): Relationships = Relationships(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Relationship"
  def primaryKey = List("Relationship")
}

case class RelationshipsValid(records:Map[String, GenericRecord]) extends SchemaReference
object RelationshipsValid {
  def apply(stream:InputStream): RelationshipsValid = RelationshipsValid(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Relationships_Valid"
  def primaryKey = List("Relationship", "Entity_Start", "Entity_End")
}

case class RelationshipClasses(records:Map[String, GenericRecord]) extends SchemaReference
object RelationshipClasses {
  def apply(stream:InputStream): RelationshipClasses = RelationshipClasses(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Relationship_Class"
  def primaryKey = List("Relationship", "Class")
}

case class EntityClasses(records:Map[String, GenericRecord]) extends SchemaReference
object EntityClasses {
  def apply(stream:InputStream): EntityClasses = EntityClasses(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Entity_Class"
  def primaryKey = List("Entity", "Class")
}

case class Classes(records:Map[String, GenericRecord]) extends SchemaReference
object Classes {
  def apply(stream:InputStream): Classes = Classes(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Class"
  def primaryKey = List("Class")
}

case class Properties(records:Map[String, GenericRecord]) extends SchemaReference
object Properties {
  def apply(stream:InputStream): Properties = Properties(SchemaReference.importCSVAsAvro(new InputStreamReader(stream), typeName, primaryKey))
  def typeName = "Property"
  def primaryKey = List("Class", "Property")
}

//  def testImportAvro_Classes: Unit = {
//    val rd = this.getClass.getClassLoader.getResourceAsStream("Classes.csv")
//    val brdr = new InputStreamReader(rd)
//    val entities = SchemaReference.importCSVAsAvro(brdr, "Class", "Class")
//    assertEquals(64, entities.size)
//  }
//  def testImportAvro_Class_Properties: Unit = {
//    val rd = this.getClass.getClassLoader.getResourceAsStream("Class_Properties.csv")
//    val brdr = new InputStreamReader(rd)
//    val entities = SchemaReference.importCSVAsAvro(brdr, "Property", List("Class", "Property"))
//    assertEquals(462, entities.size)
//  }

object SchemaReference {
  
  def importCSVAsAvro(reader:Reader, schemaName:String, primaryKey:String): Map[String, GenericRecord] = {
    importCSVAsAvro(reader, schemaName, List(primaryKey))
  }
  
  def importCSVAsAvro(reader:Reader, schemaName:String, primaryKeys:List[String]): Map[String, GenericRecord] = {
    val parser = new CSVParser(reader, CSVFormat.EXCEL.withHeader())
    val csviter = parser.iterator().asScala
    val header = parser.getHeaderMap
    val bldr = SchemaBuilder.builder().record(schemaName).fields()
    val schema = header.keySet().asScala.foldLeft(bldr)((bldr, col) => {
      if (isColumnBookean(col)) {
        bldr.name(col).`type`().booleanType().noDefault()
      } else {
        bldr.name(col).`type`().nullable().stringType().noDefault()
      }
    }).endRecord()
    
    val keyrec = Map[String, GenericRecord]()
    csviter.foldLeft(keyrec)((keyrec, csvrec) => {
      val avrorec = new GenericData.Record(schema)
      var key:Option[String] = None
      header.keySet().asScala.foreach(col => {
        if (isBoolean(schema.getField(col).schema())) {
          avrorec.put(col, toBoolean(csvrec.get(col)))
        } else {
          avrorec.put(col, csvrec.get(col))
        }
        if (primaryKeys.contains(col)) {
          if (key.isEmpty) {
            key = Option(csvrec.get(col))
          } else {
            key = Option(key.get + "." + csvrec.get(col))
          }
        }
      })
      key match {
        case Some(pkey) => keyrec.+(pkey -> avrorec)
        case None => keyrec
      }
    })
  }
  
  private def isColumnBookean(col:String) = {
    col.toLowerCase.endsWith("flag")
  }

  private def isBoolean(schema:Schema):Boolean = {
    if (schema.getType == Type.BOOLEAN) {
      true
    } else if (schema.getType == Type.UNION) {
      schema.getTypes.asScala.exists(typ => typ.getType == Type.BOOLEAN)
    } else {
      false
    }
  }
  
  private def toBoolean(str:String) : Boolean = {
    if (str != null) str.toLowerCase match {
      case "true" => true
      case "yes" => true
      case _ => false
    } else {
      false
    }
  }
}