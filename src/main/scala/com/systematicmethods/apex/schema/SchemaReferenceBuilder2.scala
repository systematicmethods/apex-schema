package com.systematicmethods.apex.schema

import java.io.InputStreamReader
import org.apache.avro.generic.GenericRecord
import java.io.StringWriter
import java.io.PrintWriter
import play.api.libs.json._

object SchemaReferenceBuilder2 {

  val loader = this.getClass.getClassLoader
  val entities = Entities(loader.getResourceAsStream("Entities.csv"))
  val properties = Properties(loader.getResourceAsStream("Class_Properties.csv"))
  val entityProperties = properties.entityProperties
  val relationshipProperties = properties.relationshipProperties
  val groups = Groups(loader.getResourceAsStream("Classes.csv"))
  val validRelationships = RelationshipsValid(loader.getResourceAsStream("Relationships_Valid.csv"))
  val relationshipGroups = RelationshipGroups(loader.getResourceAsStream("Relationship_Classes.csv"))
  val entityGroups = EntityGroups(loader.getResourceAsStream("Entity_Classes.csv"))
  val relationships = Relationships(loader.getResourceAsStream("Relationships.csv"))

  def buildSchema(namespace:String, out:PrintWriter):Unit = {
    val j1 = JsArray()
    val rels = relationships.active.map(ent => {
      makeRelationship(ent._2, namespace)
    })
    val j2 = rels.foldLeft(j1)((json, el) => json :+ el)
    val ents = entities.active.map(ent => {
      makeEntity(ent._2, namespace)
    })
    val j3 = ents.foldLeft(j2)((json, el) => json :+ el)
    out.println(Json.prettyPrint(j3))
  }

  private def makeEntity(entity: SchemaItem, namespace:String): JsValue = {
    val groups = entityGroups.filter(entity.name)
    val props = makeProperties(groups, entityProperties, true)

    val fields = Seq[(String, JsValue)](
        ("name" -> JsString(entity.recname)), 
        ("type" -> JsString("record")), 
        ("schematype" -> JsString("entity")), 
        ("namespace" -> JsString(s"${namespace}.public")),
        ("fields" -> props)
        )
    JsObject(fields)
  }

  /*
 {
          "name": "vertices",
          "type": {
            "type": "record",
            "name": "IS_RELATIONSHIP_MANAGER_TO_vertices",
            "fields": [
              {
                "name": "out_vertex",
                "type": "string"
              },
              {
                "name": "out_vertex_label",
                "type": {
                  "type": "enum",
                  "name": "IS_RELATIONSHIP_MANAGER_TO_OUT_ENUM",
                  "symbols": [
                    "Relationship_Manager"
                  ]
                }
              },
              {
                "name": "in_vertex",
                "type": "string"
              },
              {
                "name": "in_vertex_label",
                "type": {
                  "type": "enum",
                  "name": "IS_RELATIONSHIP_MANAGER_TO_IN_ENUM",
                  "symbols": [
                    "Organisation",
                    "Person"
                  ]
                }
              }
            ]
          }
        },
 
Subject_Area,Active_Flag,Entity_Start,Relationship,Relationship_Instance_Number,Entity_End,Description
Party,Yes,Relationship Manager,IS_RELATIONSHIP_MANAGER_TO,1,Organisation,A Relationship Manager manages an Organisation
Party,Yes,Relationship Manager,IS_RELATIONSHIP_MANAGER_TO,2,Person,A Relationship Manager manages a Person
 
 find starts 
 N rels for start
 N rels for end
 
   */
  private def makeRelationship(rel: SchemaItem, namespace:String): JsValue = {
    val groups = relationshipGroups.filter(rel.name)
    val validRels = validRelationships.active.filter(vrels => vrels._2.relationship == rel.name)
    val relationship = makeOutRelationship(rel, validRels)
    val props = makeProperties(groups, relationshipProperties, false)
    val fields = props.+:(relationship)
    
    val record = Seq[(String, JsValue)](
        ("name" -> JsString(rel.recname)), 
        ("type" -> JsString("record")), 
        ("schematype" -> JsString("relationship")), 
        ("namespace" -> JsString(s"${namespace}.private")),
        ("fields" -> fields)
        )
    JsObject(record)
  }

  // TODO: primary key for out and in
  private def makeOutRelationship(rel: SchemaItem, validRels: Map[String, RelationshipValid]): JsObject = {
    val outsyms = validRels.values.map(vrel => JsString(vrel.spaceTo_(vrel.entityStart))).toSet.toList
    val insyms = validRels.values.map(vrel => JsString(vrel.spaceTo_(vrel.entityEnd))).toSet.toList
    val fields = JsObject(Seq(
      ("name" -> JsString("vertices")), 
      ("type" -> JsObject(Seq(
        ("type" -> JsString("record")),
        ("name" -> JsString(rel.recname + "_vertices")),
        ("fields" -> JsArray(Seq(
          JsObject(Seq(
            ("name" -> JsString("out_vertex")),
            ("type" -> JsString("string")))),
          JsObject(Seq(
            ("name" -> JsString("out_vertex_label")),
            ("type" -> JsObject(Seq(
              ("type" -> JsString("enum")),
              ("name" -> JsString(rel.recname + "_OUT_ENUM")),
              ("symbols" -> JsArray(outsyms))
            ))))),
          JsObject(Seq(
            ("name" -> JsString("in_vertex")),
            ("type" -> JsString("string")))),
          JsObject(Seq(
            ("name" -> JsString("in_vertex_label")),
            ("type" -> JsObject(Seq(
              ("type" -> JsString("enum")),
              ("name" -> JsString(rel.recname + "_IN_ENUM")),
              ("symbols" -> JsArray(insyms))
            )))))
        )))
      ))
    )))
    fields  
  }
  
  private def makeProperties(groups:Map[String, GroupTypes], properties:Map[String, Property], usePrimaryKey:Boolean):JsArray = {
    // props for this entity that are active and valid for an entity type, sorted by prop name
    val props = groups.values.map(grp => {
      properties.values.filter(prop => prop.active && prop.group == grp.group).toList
    }).toList.flatten.sortBy(prop => prop.name)
    
    // convert to json
    val jprops = props.foldLeft(new JsArray)((arr, prop) => {
      val jprop = makeProperty(prop, usePrimaryKey)
      arr :+ jprop
    })
    //println("jarray =>" + Json.prettyPrint(jprops))
    jprops
  }
  
  private def makeProperty(rec:Property, usePrimaryKey:Boolean):JsValue = {
    val dt = if (rec.dataType == "timestamp") 
      JsObject(Seq(("type" -> JsString("long")), ("logicalType" -> JsString("timestamp-millis"))))  
    else 
      JsString(rec.dataType)
    val dtopt = if (rec.optional == "Optional") 
      JsArray(List(JsString("null"), dt))
    else 
      dt
    val fields = Seq[(String, JsValue)](
        ("name" -> JsString(rec.recname)), 
        ("type" -> dtopt))
        
    val fields2 = if (usePrimaryKey && rec.primaryKey.toLowerCase == "yes")
      fields.+:("primarykey" -> JsBoolean(true))
    else 
      fields
    
    JsObject(fields2)
  }
  
}