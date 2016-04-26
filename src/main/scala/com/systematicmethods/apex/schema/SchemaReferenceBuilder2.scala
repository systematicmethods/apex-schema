package com.systematicmethods.apex.schema

import java.io.InputStreamReader
import org.apache.avro.generic.GenericRecord
import java.io.StringWriter
import java.io.PrintWriter
import play.api.libs.json._

object SchemaReferenceBuilder2 {
/*
                    entities: {
                        header: [],
                        rows: []
                    },
                    entityProperties: {
                        header: [],
                        rows: []
                    },
                    relationships: {
                        header: [],
                        rows: []
                    },
                    relationshipsValid: {
                        header: [],
                        rows: []
                    },
                    relationshipProperties: {
                        header: [],
                        rows: []
                    },
                    components: {
                        header: [],
                        rows: []
                    },
                    componentProperties: {
                        header: [],
                        rows: []
                    }

 */
  val loader = this.getClass.getClassLoader
  val entities = Entities(loader.getResourceAsStream("Entities.csv"))
  val properties = Properties(loader.getResourceAsStream("Class_Properties.csv"))
  val entityProperties = properties.entityProperties
  val relationshipProperties = properties.relationshipProperties
  val groups = Groups(loader.getResourceAsStream("Classes.csv"))
  val valid = RelationshipsValid(loader.getResourceAsStream("Relationships_Valid.csv"))
  val relationshipGroups = RelationshipGroups(loader.getResourceAsStream("Relationship_Classes.csv"))
  val entityGroups = EntityGroups(loader.getResourceAsStream("Entity_Classes.csv"))
  val relationships = Relationships(loader.getResourceAsStream("Relationships.csv"))

  def buildSchema(namespace:String, out:PrintWriter):Unit = {
    val json = JsArray()
    val rels = relationships.active.map(ent => {
      makeRelationship(ent._2, namespace)
    })
    val j1 = rels.foldLeft(json)((json, el) => json.:+(el))
    val ents = entities.active.map(ent => {
      makeEntity(ent._2, namespace)
    })
    val j2 = ents.foldLeft(j1)((json, el) => json.:+(el))
    out.println(Json.prettyPrint(j2))
  }

  private def makeEntity(entity: SchemaItem, namespace:String): JsValue = {
    val groups = entityGroups.filter(entity.name)
    val props = makeProperties(groups, entityProperties)

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
    val props = makeProperties(groups, relationshipProperties)
    
    val fields = Seq[(String, JsValue)](
        ("name" -> JsString(rel.recname)), 
        ("type" -> JsString("record")), 
        ("schematype" -> JsString("relationship")), 
        ("namespace" -> JsString(s"${namespace}.private")),
        ("fields" -> props)
        )
    JsObject(fields)
  }

  private def makeProperties(classes:Map[String, GroupTypes], properties:Map[String, Property]):JsValue = {
    var array = new JsArray
    val eitr = classes.iterator
    while (eitr.hasNext) {
      val entGroup = eitr.next()._2.group
      // props for this entity that are active and valid for an entity type
      val piter = properties.filter(prop => prop._2.group == entGroup).iterator
      while (piter.hasNext) {
        val prop = piter.next()
        val jprop = makeProperty(prop._2)
        array = array.append(jprop)
      }
    }
    array
  }
  
  private def makeProperty(rec:Property):JsValue = {
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
        
    val fields2 = if (rec.primaryKey.toLowerCase == "yes")
      fields.+:("primarykey" -> JsBoolean(true))
    else 
      fields
    
    JsObject(fields2)
  }
  
}