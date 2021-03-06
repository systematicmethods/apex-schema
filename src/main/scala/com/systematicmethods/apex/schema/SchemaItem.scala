package com.systematicmethods.apex.schema

import java.io.InputStream
import java.io.InputStreamReader

import org.apache.avro.generic.GenericRecord

trait SchemaItem {
  protected def record:GenericRecord
  def active: Boolean = record.get("Active_Flag").asInstanceOf[Boolean]
  def name:String
  def avroname:String = SchemaItem.avroname(name)
  def description:String = record.get("Description").asInstanceOf[String]
  def subjectArea:String = record.get("Subject_Area").asInstanceOf[String]
}
object SchemaItem {
  def avroname(aname:String) = aname.replaceAll(" ", "_")
}

trait SchemaItems[A <: SchemaItem] {
  protected def records:Map[String, A]
  def size:Int = records.size
  def filter(where:String): Map[String, A] = active
  def active: Map[String, A] = records.filter(rec => rec._2.active)
}

trait SchemaType {
  def typeName:String
  def primaryKey:List[String]
}

trait GroupTypes extends SchemaItem {
  def group:String
}

case class Entity(record:GenericRecord) extends SchemaItem {
  def name:String = record.get(Entity.typeName).asInstanceOf[String]
}
object Entity extends SchemaType {
  override val typeName = "Entity"
  override val primaryKey = List("Entity")
}

case class Entities(records:Map[String, Entity]) extends SchemaItems[Entity] 
object Entities {
  def apply(stream:InputStream): Entities = 
    Entities(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), Entity.typeName, Entity.primaryKey).map(x => (x._1, Entity(x._2))))
}

case class Relationship(record:GenericRecord) extends SchemaItem {
  def name:String = record.get(Relationship.typeName).asInstanceOf[String]
}
object Relationship extends SchemaType {
  override val typeName = "Relationship"
  override val primaryKey = List("Relationship")
}

case class Relationships(records:Map[String, Relationship]) extends SchemaItems[Relationship]
object Relationships {
  def apply(stream:InputStream): Relationships = 
    Relationships(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), Relationship.typeName, Relationship.primaryKey).map(x => (x._1, Relationship(x._2))))
}

case class RelationshipValid(record:GenericRecord) extends SchemaItem {
  override def name:String = record.get(Relationship.typeName).asInstanceOf[String]
  def relationship:String = record.get("Relationship").asInstanceOf[String]
  def entityStart:String = record.get("Entity_Start").asInstanceOf[String]
  def entityEnd:String = record.get("Entity_End").asInstanceOf[String]
  def relationshipAvroName:String = SchemaItem.avroname(record.get("Relationship").asInstanceOf[String])
  def entityStartAvroName:String = SchemaItem.avroname(record.get("Entity_Start").asInstanceOf[String])
  def entityEndAvroName:String = SchemaItem.avroname(record.get("Entity_End").asInstanceOf[String])
}
object RelationshipValid extends SchemaType {
  override val typeName = "Relationships_Valid"
  override val primaryKey = List("Relationship", "Entity_Start", "Entity_End")
}

case class RelationshipsValid(records:Map[String, RelationshipValid]) extends SchemaItems[RelationshipValid]
object RelationshipsValid {
  def apply(stream:InputStream): RelationshipsValid = 
    RelationshipsValid(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), RelationshipValid.typeName, RelationshipValid.primaryKey).map(x => (x._1, RelationshipValid(x._2))))
}

case class RelationshipGroup(record:GenericRecord) extends GroupTypes {
  override def name:String = record.get(Relationship.typeName).asInstanceOf[String]
  override def group:String = record.get("Class").asInstanceOf[String]
}
object RelationshipGroup extends SchemaType {
  def typeName = "Relationship_Class"
  def primaryKey = List("Relationship", "Class")
}

case class RelationshipGroups(records:Map[String, RelationshipGroup]) extends SchemaItems[RelationshipGroup] {
  override def filter(where:String): Map[String, RelationshipGroup] = records.filter(rec => rec._2.active && rec._2.name == where)
}
object RelationshipGroups {
  def apply(stream:InputStream): RelationshipGroups = 
    RelationshipGroups(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), RelationshipGroup.typeName, RelationshipGroup.primaryKey).map(x => (x._1, RelationshipGroup(x._2))))
}

case class EntityGroup(record:GenericRecord) extends GroupTypes {
  override def name:String = record.get(Entity.typeName).asInstanceOf[String]
  override def group:String = record.get("Class").asInstanceOf[String]
}
object EntityGroup extends SchemaType {
  def typeName = "Entity_Class"
  def primaryKey = List("Entity", "Class")
}

case class EntityGroups(records:Map[String, EntityGroup]) extends SchemaItems[EntityGroup] {
  override def filter(where:String): Map[String, EntityGroup] = records.filter(rec => rec._2.active && rec._2.name == where)
}
object EntityGroups {
  def apply(stream:InputStream): EntityGroups = 
    EntityGroups(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), EntityGroup.typeName, EntityGroup.primaryKey).map(x => (x._1, EntityGroup(x._2))))
}

case class Group(record:GenericRecord) extends SchemaItem {
  def name:String = record.get(Group.typeName).asInstanceOf[String]
  def category:String = record.get("Category").asInstanceOf[String]
}
object Group extends SchemaType {
  def typeName = "Class"
  def primaryKey = List("Class")
}

case class Groups(records:Map[String, Group]) extends SchemaItems[Group]
object Groups {
  def apply(stream:InputStream): Groups = 
    Groups(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), Group.typeName, Group.primaryKey).map(x => (x._1, Group(x._2))))
}

case class Property(record:GenericRecord) extends SchemaItem {
  def name:String = record.get(Property.typeName).asInstanceOf[String]
  def category  = record.get("Category").asInstanceOf[String]
  def group  = record.get("Class").asInstanceOf[String]
  def dataType  = record.get("DataType").asInstanceOf[String]
  def optional  = record.get("Optionality").asInstanceOf[String]
  def primaryKey  = record.get("PrimaryKey").asInstanceOf[String]
  def isPrimaryKey:Boolean = primaryKey.toLowerCase == "yes"
}
object Property extends SchemaType {
  def typeName = "Property"
  def primaryKey = List("Class", "Property")
}

case class Properties(records:Map[String, Property]) extends SchemaItems[Property] {
  override def filter(where:String): Map[String, Property] = records.filter(rec => rec._1.startsWith(where))
  def entityProperties: Map[String, Property] = records.filter(prop => prop._2.category == Entity.typeName && prop._2.active)
  def relationshipProperties: Map[String, Property] = records.filter(prop => prop._2.category == Relationship.typeName && prop._2.active)
}
object Properties {
  def apply(stream:InputStream): Properties = 
    Properties(SchemaDataAccess.importCSVAsAvro(new InputStreamReader(stream), Property.typeName, Property.primaryKey).map(x => (x._1, Property(x._2))))
}

//case class Entity_Out_Relationship(entity_start:String, relationship:String, entity_end:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Defined_By_Group(entity:String, group:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Relationship_Defined_By_Group(relationship:String, group:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class String_Composed_Of_Property(group:String, property:String, data_type:String, optionality:Boolean, category:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Identified_By_Property(entity:String, property:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship
//case class Entity_Indexed_By_Property(entity:String, property:String, subject_area:String, active_flag:Boolean, description:String) extends SchemaRelationship

//trait SchemaReferences {
//  def records:Map[String, GenericRecord]
//  def size:Int = records.size
//  def filter(where:String): Map[String, GenericRecord] = active
//  def active: Map[String, GenericRecord] = records.filter(rec => rec._2.get("Active_Flag").asInstanceOf[Boolean])
//}


