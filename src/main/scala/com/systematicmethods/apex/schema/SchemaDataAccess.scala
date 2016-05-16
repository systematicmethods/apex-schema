package com.systematicmethods.apex.schema

import java.io.Reader

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.JavaConverters.asScalaSetConverter

import org.apache.avro.Schema
import org.apache.avro.Schema.Type
import org.apache.avro.SchemaBuilder
import org.apache.avro.generic.GenericData
import org.apache.avro.generic.GenericRecord
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVParser

object SchemaDataAccess {
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

//  def testImportAvro_Classes: Unit = {
//    val rd = this.getClass.getClassLoader.getResourceAsStream("Classes.csv")
//    val brdr = new InputStreamReader(rd)
//    val entities = SchemaReferences.importCSVAsAvro(brdr, "Class", "Class")
//    assertEquals(64, entities.size)
//  }
//  def testImportAvro_Class_Properties: Unit = {
//    val rd = this.getClass.getClassLoader.getResourceAsStream("Class_Properties.csv")
//    val brdr = new InputStreamReader(rd)
//    val entities = SchemaReferences.importCSVAsAvro(brdr, "Property", List("Class", "Property"))
//    assertEquals(462, entities.size)
//  }
