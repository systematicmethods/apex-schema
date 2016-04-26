package com.systematicmethods.apex.schema

import java.io.InputStreamReader
import org.apache.avro.generic.GenericRecord
import java.io.StringWriter
import java.io.PrintWriter

object SchemaReferenceBuilder {

  val loader = this.getClass.getClassLoader
  val entities = Entities(loader.getResourceAsStream("Entities.csv"))
  val properties = Properties(loader.getResourceAsStream("Class_Properties.csv"))
  val classes = Properties(loader.getResourceAsStream("Classes.csv"))
  val valid = RelationshipsValid(loader.getResourceAsStream("Relationships_Valid.csv"))
  val relationshipClasses = RelationshipGroups(loader.getResourceAsStream("Relationship_Classes.csv"))
  val entityClasses = EntityGroups(loader.getResourceAsStream("Entity_Classes.csv"))
  val relationships = Relationships(loader.getResourceAsStream("Relationships.csv"))

  def buildSchema(namespace:String, out:PrintWriter):Unit = {
    out.print(begin)
    val ritr = relationships.active.iterator
    while (ritr.hasNext) {
      val rel = ritr.next()
      out.print(makeRelationship(rel._2, namespace))
      if (ritr.hasNext) {
        out.println(',')
      }
    }
    out.println(',')
    val eitr = entities.active.iterator
    while (eitr.hasNext) {
      val ent = eitr.next()
      out.print(makeEntity(ent._2, namespace))
      if (eitr.hasNext) {
        out.println(',')
      }
    }
    //entities.foreach(ent => println(makeEntity(ent._2)))
    out.println(end)
  }

  private def begin: String = {
    """[
"""
  }

  private def end: String = {
"""
]
"""
  }
  private def makeEntity(entity: SchemaItem, namespace:String): String = {
    val template = """  {
    "name": "${name}",
    "type": "record",
    "schematype": "entity",
    "namespace": "${namespace}.public",
    "fields": [
      ${propoerties}
    ]
  }"""
    val param = entity.name
    
    val classes = entityClasses.filter(param)
    val props = makeProperties(classes)
    template.replace("${name}", entity.recname).replace("${namespace}", namespace).replace("${propoerties}", props)
  }

  private def makeProperties(entityClasses:Map[String, GroupTypes]):String = {
    val eitr = entityClasses.iterator
    val str = new StringWriter
    val out = new PrintWriter(str)

    while (eitr.hasNext) {
      val entclass = eitr.next()._2.group
      // props for this entity that are active and valid for an entity type
      val props = properties.filter(entclass).filter(p => p._2.active)//.filter(p => p._2.dataType == typeName)
      val piter = props.iterator
      var count = 0
      while (piter.hasNext) {
        count = count + 1
        val prop = piter.next()
        out.print(makeProperty(prop._2))
        if (piter.hasNext) {
          out.println(',')
        }
      }
      if (count > 0) {
        out.println(',')
      }
    }
    str.toString()
  }
  
  private def makeProperty(rec:Property):String = {
    val primaryKeyTemplate = """
          "primarykey": "true", """
    val nullTemplate = """        {
          "name": "${name}",${primarykey}
          "type": [
            "null",
            "${type}"
          ]
        }"""
    val notNulltemplate = """        {
          "name": "${name}",${primarykey}
          "type": "${type}"
        }"""
    val pktemplate = if (rec.primaryKey.toLowerCase == "yes") primaryKeyTemplate else ""
    val template = if (rec.optional == "Optional") {
      nullTemplate
    } else {
      notNulltemplate
    }
    template.replace("${name}", rec.recname).replace("${type}", rec.dataType).replace("${primarykey}", pktemplate)
  }
  
  private def makeRelationship(rec: SchemaItem, namespace:String): String = {
    val template = """  {
    "name": "${name}",
    "type": "record",
    "schematype": "relationship",
    "namespace": "${namespace}.private",
    "fields": [
      ${propoerties}
    ]
  }"""
    val param = rec.name
    val classes = relationshipClasses.filter(param)
    val props = makeProperties(classes)
    template.replace("${name}", rec.recname).replace("${namespace}", namespace).replace("${propoerties}", props)
  }
}