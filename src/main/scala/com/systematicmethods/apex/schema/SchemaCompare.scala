package com.systematicmethods.apex.schema

import org.apache.avro.Schema
import org.apache.avro.Schema.{Parser, Type}

import scala.collection.JavaConverters._

object SchemaCompare {
  val RELATIONSHIPS = "relationships"
  val VERTICES = "vertices"

  def compareSchema(avsc1:String, avsc2:String):Option[List[String]] = {
    val schema1 = try {
      new Parser().parse(avsc1)
    } catch {
      case (ex: Exception) =>
        return Option(List(s"Parser error in avsc1 ${ex.getMessage}"))
    }
    val schema2 = try {
      new Parser().parse(avsc2)
    } catch {
      case (ex: Exception) =>
        return Option(List(s"Parser error in avsc2 ${ex.getMessage}"))
    }

    val result = compareTypes(schema1, schema2) ++
        compareTypeFields(schema1, schema2) ++
        compareEntityRelationships(schema1, schema2) ++
        compareRelationships(schema1, schema2)
    if (result.isEmpty) None else Option(result)
  }
  
  def compareTypes(schema1:Schema, schema2:Schema): List[String] = {
    val schemas1 = getUnionTypes(schema1)
    val schemas2 = getUnionTypes(schema2)

    val t1names = schemas1.map(t => t.getFullName).sortBy(x => x)
    val t2names = schemas2.map(t => t.getFullName).sortBy(x => x)

    if (!t1names.equals(t2names)) {
      val result = s"""Different types (entities)
                   |     source 1 vs 2 = ${t1names.diff(t2names).mkString(", ")}
                   |     source 2 vs 1 = ${t2names.diff(t1names).mkString(", ")}
                   |""".stripMargin
      List(result)
    } else {
      List()
    }
  }

  def getType(schema: Schema):String = {
    if (schema.getType != Type.UNION) {
      schema.getType.toString
    } else {
      val schemas = schema.getTypes.asScala.toList
      if (schemas.size == 2) {
        val scs = schemas.filter(t => t.getType != Type.NULL).map(sc => {
          if (sc.getLogicalType != null) s"optional:${sc.getType}:${sc.getLogicalType.getName}" else s"optional:${sc.getType}"
        })
        scs.mkString
      } else {
        Type.RECORD.toString
      }
    }
  }

  def compareTypeFields(schema1:Schema, schema2:Schema): List[String] = {
    val schemas1 = getUnionTypes(schema1)
    val schemas2 = getUnionTypes(schema2)

    val res = for {
        schema1 <- schemas1
        schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
        fields1 = schema1.getFields.asScala.map(fld => (fld.name(), getType(fld.schema))).toList.sortBy(x => x)
        fields2 = schema2.getFields.asScala.map(fld => (fld.name(), getType(fld.schema))).toList.sortBy(x => x)
        if fields1 != fields2
    } yield {
      s"""Different fields (properties) for ${schema1.getName}
      |    source 1 vs 2 = ${fields1.diff(fields2).mkString(", ")}
      |    source 2 vs 1 = ${fields2.diff(fields1).mkString(", ")}
      |""".stripMargin
    }
    res
  }

  def compareEntityRelationships(schema1:Schema, schema2:Schema): List[String] = {
    val schemas1 = getEntities(schema1)
    val schemas2 = getEntities(schema2)

    val ressize = if (schemas1.size != schemas2.size) {
      List(s"schemas1.size ${schemas1.size} != schemas2.size ${schemas2.size}")
    } else List()

    val res = for {
      schema1 <- schemas1
      schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
      rel1 = schema1.getField(RELATIONSHIPS)
      rel2 = schema2.getField(RELATIONSHIPS)
      if rel1 != null && rel2 != null
      rel1rec <- rel1.schema().getTypes.asScala.filter(sc => sc.getType == Type.RECORD).toList
      rel2rec <- rel2.schema().getTypes.asScala.filter(sc => sc.getType == Type.RECORD).toList
      rel1types = rel1rec.getFields.asScala.map(fld => fld.name).toList.sortBy(x => x)
      rel2types = rel2rec.getFields.asScala.map(fld => fld.name).toList.sortBy(x => x)
      if rel1types != rel2types
    } yield {
      s"""Different entity relationships for ${schema1.getName}
          |    source 1 vs 2 = ${rel1types.diff(rel2types).mkString(", ")}
          |    source 2 vs 1 = ${rel2types.diff(rel1types).mkString(", ")}
          |""".stripMargin
    }
    ressize ++ res
  }

  /*
  compare this....
 {
  "type" : "record",
  "name" : "HAS_PHONE_NUMBER",
  "namespace" : "org.profile.customer.avro.private",
  "fields" : [ {
    "name" : "vertices",
    "type" : {
      "type" : "record",
      "name" : "HAS_PHONE_NUMBER_vertices",
      "fields" : [ {
        "name" : "out_vertex",
        "type" : "string"
      }, {
        "name" : "out_vertex_label",
        "type" : {
          "type" : "enum",
          "name" : "HAS_PHONE_NUMBER_OUT_ENUM",
          "symbols" : [ "Organisation_Prospect_diff", "Person_Prospect", "Relationship_Manager", "Person", "Organisation", "Staff_Member" ],
          "schematype" : [ {
            "entity" : "Organisation_Prospect_diff",
            "primarykey" : "CONT_ID_diff"
          }, {
            "entity" : "Person_Prospect",
            "primarykey" : "CONT_ID"
          }, {
          ....
      }, {
        "name" : "in_vertex",
        "type" : "string"
      }, {
        "name" : "in_vertex_label",
        "type" : {
          "type" : "enum",
          "name" : "HAS_PHONE_NUMBER_IN_ENUM",
          "symbols" : [ "Phone_Number" ],
          "schematype" : [ {
            "entity" : "Phone_Number",
            "primarykey" : "CONTACT_METHOD_ID"
          } ]
        }
      } ]
    }
   */
  def compareRelationships(schema1:Schema, schema2:Schema): List[String] = {
    val schemas1 = getRelationships(schema1)
    val schemas2 = getRelationships(schema2)
    // schema names and properties are checked elsewhere e.g. HAS_PHONE_NUMBER
    // checks recond name e.g. HAS_PHONE_NUMBER_vertices
    val res = for {
      schema1 <- schemas1
      schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
      rel1 = schema1.getField(VERTICES)
      rel2 = schema2.getField(VERTICES)
      rel1recname = rel1.schema.getName
      rel2recname = rel2.schema.getName
      if rel1recname != rel2recname
    } yield {
      //println(rel1recname, rel2recname)
      s"""Different relationships for ${schema1.getName}
          |    source 1 vs 2 = ${rel1recname.diff(rel2recname).mkString(", ")}
          |    source 2 vs 1 = ${rel2recname.diff(rel1recname).mkString(", ")}
          |""".stripMargin
    }

    // compare vertices fields e.g. out_vertex_label
    val res2 = for {
      schema1 <- schemas1
      schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
      fldname1 = schema1.getField(VERTICES).schema().getFields.asScala.map(fld => fld.name).toList.sortBy(x => x)
      fldname2 = schema2.getField(VERTICES).schema().getFields.asScala.map(fld => fld.name).toList.sortBy(x => x)
      if fldname1 != fldname2
    } yield {
      s"""Different relationships for ${schema1.getName}
          |    source 1 vs 2 = ${fldname1.diff(fldname2).mkString(", ")}
          |    source 2 vs 1 = ${fldname2.diff(fldname1).mkString(", ")}
          |""".stripMargin
    }

    // compare out_vertex enum symbols e.g. Organisation_Prospect_diff
    val res3 = for {
      schema1 <- schemas1
      schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
      fld1 <- schema1.getField(VERTICES).schema.getFields.asScala.toList.sortBy(x => x.name())
      fld2 <- schema2.getField(VERTICES).schema.getFields.asScala.filter(x => x.name() == fld1.name())
      if fld1.schema().getType == Type.ENUM && fld2.schema().getType == Type.ENUM
      symbols1 = fld1.schema().getEnumSymbols.asScala.map(sym => sym).toList.sortBy(x => x)
      symbols2 = fld2.schema().getEnumSymbols.asScala.map(sym => sym).toList.sortBy(x => x)
      if symbols1 != symbols2
    } yield {
//      println(fld1, fld2)
//      println(symbols1, symbols2)
      s"""Different enum symbols for ${schema1.getName}:${schema2.getName}
          |    source 1 vs 2 = ${symbols1.diff(symbols2).mkString(", ")}
          |    source 2 vs 1 = ${symbols2.diff(symbols1).mkString(", ")}
          |""".stripMargin
    }

    // compare schema primary keys s e.g. Organisation_Prospect_diff e.g.
    //    "entity" : "Organisation_Prospect_diff",
    //    "primarykey" : "CONT_ID_diff"
    val res4 = for {
      schema1 <- schemas1
      schema2 <- schemas2.filter(sc2 => sc2.getName == schema1.getName)
      fld1 <- schema1.getField(VERTICES).schema.getFields.asScala.toList.sortBy(x => x.name())
      fld2 <- schema2.getField(VERTICES).schema.getFields.asScala.filter(x => x.name() == fld1.name())
      if fld1.schema().getType == Type.ENUM && fld2.schema().getType == Type.ENUM
      pk1 = fld1.schema().getObjectProp("schematype")
      pk2 = fld2.schema().getObjectProp("schematype")
      if pk1 != null && pk2 != null && pk1 != pk2
    } yield {
      val pkl1 = pk1.asInstanceOf[java.util.ArrayList[String]].asScala.toList
      val pkl2 = pk2.asInstanceOf[java.util.ArrayList[String]].asScala.toList
      s"""Different primary keys for ${schema1.getName}:${schema2.getName}
          |    source 1 vs 2 = ${pkl1.diff(pkl2).mkString(", ")}
          |    source 2 vs 1 = ${pkl2.diff(pkl1).mkString(", ")}
          |""".stripMargin
    }
    //println(res4)

    res ++ res2 ++ res3 ++ res4
  }


  // utils

  def getEntities(schema:Schema): List[Schema] = {
    schema.getTypes.asScala.toList.filter(sc => sc.getProp("schematype") == "entity").sortBy(sc => sc.getName)
  }

  def getRelationships(schema:Schema): List[Schema] = {
    schema.getTypes.asScala.toList.filter(sc => sc.getProp("schematype") == "relationship").sortBy(sc => sc.getName)
  }

  def getUnionTypes(schema:Schema): List[Schema] = {
    schema.getTypes.asScala.toList.sortBy(sc => sc.getName)
  }
}