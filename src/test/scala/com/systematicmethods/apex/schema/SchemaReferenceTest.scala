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
    val recs = entities.records.values.filter(_.record.get(Entity.typeName) == "Organisation")
    assertEquals(1, recs.size)
    recs.foreach { rec => {
      assertEquals("Party", rec.record.get("Subject_Area")) 
      assertTrue(rec.active)
      assertEquals("A corporate client - formerly known as Institution", rec.description)
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
    val recs = RelationshipGroups(getClass.getClassLoader.getResourceAsStream("Relationship_Classes.csv"))
    assertEquals(29, recs.size)
  }

  @Test
  def testImportAvro_Entity_Classes: Unit = {
    val recs = EntityGroups(getClass.getClassLoader.getResourceAsStream("Entity_Classes.csv"))
    assertEquals(75, recs.size)
  }

  @Test
  def testImportAvro_Classes: Unit = {
    val recs = Groups(getClass.getClassLoader.getResourceAsStream("Classes.csv"))
    assertEquals(64, recs.size)
  }

  @Test
  def testImportAvro_Class_Properties: Unit = {
    val recs = Properties(getClass.getClassLoader.getResourceAsStream("Class_Properties.csv"))
    assertEquals(462, recs.size)
  }

}
