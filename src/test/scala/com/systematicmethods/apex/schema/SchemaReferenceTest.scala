package com.systematicmethods.apex.schema

import java.io._

import org.junit.Test
import org.junit.Assert._

/**
  * Created by peter on 07/04/2016.
  */
class SchemaReferenceTest {

  @Test
  def testImportAvro_entities: Unit = {
    val rd = this.getClass.getClassLoader.getResourceAsStream("Entities.csv")
    val entities = Entities(rd)
    assertEquals(28, entities.size)
    val recs = entities.records.values.filter(_.get(Entities.typeName) == "Organisation")
    assertEquals(1, recs.size)
    recs.foreach { rec => {
      assertEquals("Party", rec.get("Subject_Area")) 
      assertTrue(rec.get("Active_Flag").isInstanceOf[Boolean])
      assertEquals(true, rec.get("Active_Flag"))
      assertEquals("A corporate client - formerly known as Institution", rec.get("Description"))
      }
    }
  }

  @Test
  def testImportAvro_relationships: Unit = {
    val recs = Relationships(getClass.getClassLoader.getResourceAsStream("Relationships.csv"))
    assertEquals(25, recs.size)
  }

  @Test
  def testImportAvro_Relationships_Valid: Unit = {
    val recs = RelationshipsValid(getClass.getClassLoader.getResourceAsStream("Relationships_Valid.csv"))
    assertEquals(72, recs.size)
  }

  @Test
  def testImportAvro_Relationship_Classes: Unit = {
    val recs = RelationshipClasses(getClass.getClassLoader.getResourceAsStream("Relationship_Classes.csv"))
    assertEquals(29, recs.size)
  }

  @Test
  def testImportAvro_Entity_Classes: Unit = {
    val recs = EntityClasses(getClass.getClassLoader.getResourceAsStream("Entity_Classes.csv"))
    assertEquals(75, recs.size)
  }

  @Test
  def testImportAvro_Classes: Unit = {
    val recs = Classes(getClass.getClassLoader.getResourceAsStream("Classes.csv"))
    assertEquals(64, recs.size)
  }

  @Test
  def testImportAvro_Class_Properties: Unit = {
    val recs = Properties(getClass.getClassLoader.getResourceAsStream("Class_Properties.csv"))
    assertEquals(462, recs.size)
  }

}
