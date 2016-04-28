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
 			,{
        "name" : "out_vertex_pk",
        "type" : {
          "type" : "array",
          "items" : "string"
        }
      },
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
    val outEntities = validRels.values.map(vrel => vrel.entityStart).toSet.toList
    val inEntities = validRels.values.map(vrel => vrel.entityEnd).toSet.toList
    val outpks = makePK(outEntities)
    val inpks = makePK(inEntities)
    val outsyms = outEntities.map(aname => JsString(SchemaItem.spaceTo_(aname)))
    val insyms = inEntities.map(aname => JsString(SchemaItem.spaceTo_(aname)))
    val fields = JsObject(Seq(
      ("name" -> JsString("vertices")), 
      ("type" -> JsObject(Seq(
        ("name" -> JsString(rel.recname + "_vertices")),
        ("type" -> JsString("record")),
        ("fields" -> JsArray(Seq(
          JsObject(Seq(
            ("name" -> JsString("out_vertex")),
            ("type" -> JsString("string")))),
          JsObject(Seq(
            ("name" -> JsString("out_vertex_label")),
            ("type" -> JsObject(Seq(
              ("type" -> JsString("enum")),
              ("name" -> JsString(rel.recname + "_OUT_ENUM")),
              ("symbols" -> JsArray(outsyms)),
              ("schematype" -> JsArray(outpks))
            ))
          ))),
          JsObject(Seq(
            ("name" -> JsString("in_vertex")),
            ("type" -> JsString("string")))),
          JsObject(Seq(
            ("name" -> JsString("in_vertex_label")),
            ("type" -> JsObject(Seq(
              ("type" -> JsString("enum")),
              ("name" -> JsString(rel.recname + "_IN_ENUM")),
              ("symbols" -> JsArray(insyms)),
              ("schematype" -> JsArray(inpks))
            ))
          )))
        )))
      ))
    )))
    fields  
  }
  
  private def makePK(entities: List[String]):Seq[JsObject] = {
    val props = for {
      entityName <- entities
      groups <- entityGroups.filter(entityName).values
      prop <- entityProperties.values.filter(ep => ep.isPrimaryKey)
      if prop.group == groups.group
    } yield {
      (entityName, prop.name)
    }
    props.map(ep => {
      JsObject(Seq(
          ("entity" -> JsString(SchemaItem.spaceTo_(ep._1))), 
          ("primarykey" -> JsString(ep._2))))  
    })
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
        
    val fields2 = if (usePrimaryKey && rec.isPrimaryKey)
      fields.+:("primarykey" -> JsBoolean(true))
    else 
      fields
    
    JsObject(fields2)
  }
  
}