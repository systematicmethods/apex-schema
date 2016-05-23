package com.systematicmethods.apex.schema

import org.junit.Test
import java.io.StringWriter
import java.io.PrintWriter
import java.io.File
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.FileReader
import org.junit.Assert._

class SchemaCompareTest {
  val loader = this.getClass.getClassLoader

  @Test
  def compare091schema(): Unit = {
    val avsc1 = getresource("GraphStore_v0_9_1.avsc")
    val avsc2 = getresource("fromCSV-091.avsc")
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        fail("schema types should be the same")
      case None =>
    }
  }

  @Test
  def compare091schema_diff_types(): Unit = {
    val avsc1 = getresource("fromCSV-091.avsc")
    val avsc2 = getresource("fromCSV-091-diff-types.avsc")
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(1, reslist.count(res => res.contains("Different types")))
      case None =>
        fail("schema files should differ")
    }
  }

  @Test
  def compare091schema_diff_fields(): Unit = {
    val avsc1 = getresource("fromCSV-091.avsc")
    val avsc2 = getresource("fromCSV-091-diff-fields.avsc")
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(3, reslist.count(res => res.contains("Different fields")))
      case None =>
        fail("schema files should differ")
    }
  }

  @Test
  def compare091schema_diff_entity_relationships(): Unit = {
    val avsc1 = getresource("fromCSV-091.avsc")
    val avsc2 = getresource("fromCSV-091-diff-entity-relationships.avsc")

    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        assertEquals(2, reslist.size)
        assertEquals(2, reslist.count(res => res.contains("Different entity relationships")))
      case None =>
        fail("schema files should differ")
    }
  }

  @Test
  def compare091schema_diff_relationships(): Unit = {
    val avsc1 = getresource("fromCSV-091.avsc")
    val avsc2 = getresource("fromCSV-091-diff-relationships.avsc")

    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(1, reslist.count(res => res.contains("Different relationships")))
        assertEquals(1, reslist.count(res => res.contains("Different enum")))
        assertEquals(1, reslist.count(res => res.contains("Different primary keys")))
      case None =>
        fail("schema files should differ")
    }
  }

  @Test
  def compare091schema_invalid(): Unit = {
    val avsc1 = getresource("GraphStore_v0_9_1.avsc")
    val avsc2 = getresource("invalid.avsc")
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) =>
        println(reslist.mkString)
        assertEquals(1, reslist.size)
      case None =>
        fail("schema files should differ")
    }
  }

  private def getresource(name:String) : String = {
    scala.io.Source.fromInputStream(loader.getResourceAsStream(name)).mkString
  }

}