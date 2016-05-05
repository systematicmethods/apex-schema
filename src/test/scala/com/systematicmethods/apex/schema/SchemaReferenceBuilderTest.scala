package com.systematicmethods.apex.schema

import org.junit.{Assert, Test}
import java.io.StringWriter
import java.io.PrintWriter

class SchemaReferenceBuilderTest {
  @Test
  def testbuild_entities: Unit = {
    val loader = this.getClass.getClassLoader
    val entities = Entities(loader.getResourceAsStream("Entities.csv"))
    val properties = Properties(loader.getResourceAsStream("Class_Properties.csv"))
    //val groups = Groups(loader.getResourceAsStream("Classes.csv"))
    val validRelationships = RelationshipsValid(loader.getResourceAsStream("Relationships_Valid.csv"))
    val relationshipGroups = RelationshipGroups(loader.getResourceAsStream("Relationship_Classes.csv"))
    val entityGroups = EntityGroups(loader.getResourceAsStream("Entity_Classes.csv"))
    val relationships = Relationships(loader.getResourceAsStream("Relationships.csv"))

    val builder = new SchemaReferenceBuilder("org.profile.customer.avro", entities,
        relationships,
        properties,
        validRelationships,
        relationshipGroups,
        entityGroups)
    
    builder.buildAvroSchema() match {
      case Some(schema) => println(schema.toString(true))
      case None => Assert.fail("Not a valid schema")
    }

  }


}