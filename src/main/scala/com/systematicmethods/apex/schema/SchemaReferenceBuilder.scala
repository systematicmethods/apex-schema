package com.systematicmethods.apex.schema

import java.io.InputStreamReader
import org.apache.avro.generic.GenericRecord

object SchemaReferenceBuilder {
  
  def buildSchema = {
    val rd = this.getClass.getClassLoader.getResourceAsStream("Entities.csv")
    val brdr = new InputStreamReader(rd)
    val entities = SchemaReference.importCSVAsAvro(brdr, "Entity", "Entity")
    entities.foreach(ent => println(makeEntity(ent._2)))
  }
  
  private def makeEntity(rec:GenericRecord): String = {
    val template = """  {
    "name": "$Entity",
    "type": "record",
    "schematype": "entity",
    "namespace": "org.profile.customer.avro.public",
    "fields": []
  },"""
    val param = rec.get("Entity").asInstanceOf[String]
    template.replace("$Entity", param)
  }
}