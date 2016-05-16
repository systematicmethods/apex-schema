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
  @Test
  def compare091schema: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/GraphStore_v0_9_1.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/fromCSV-091.avsc").mkString
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        fail("schema types should be the same")
      }
      case None => {}
    }
  }

  @Test
  def compare091schema_diff_types: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/fromCSV-091.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/fromCSV-091-diff-types.avsc").mkString
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(1, reslist.filter(res => res.contains("Different types")).size)
      }
      case None => {
        fail("schema files should differ")
      }
    }
  }

  @Test
  def compare091schema_diff_fields: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/fromCSV-091.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/fromCSV-091-diff-fields.avsc").mkString
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(3, reslist.filter(res => res.contains("Different fields")).size)
      }
      case None => {
        fail("schema files should differ")
      }
    }
  }

  @Test
  def compare091schema_diff_entity_relationships: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/fromCSV-091.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/fromCSV-091-diff-entity-relationships.avsc").mkString

    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        assertEquals(2, reslist.size)
        assertEquals(2, reslist.filter(res => res.contains("Different entity relationships")).size)
      }
      case None => {
        fail("schema files should differ")
      }
    }
  }

  @Test
  def compare091schema_diff_relationships: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/fromCSV-091.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/fromCSV-091-diff-relationships.avsc").mkString

    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        assertEquals(3, reslist.size)
        assertEquals(1, reslist.filter(res => res.contains("Different relationships")).size)
        assertEquals(1, reslist.filter(res => res.contains("Different enum")).size)
        assertEquals(1, reslist.filter(res => res.contains("Different primary keys")).size)
      }
      case None => {
        fail("schema files should differ")
      }
    }
  }

  @Test
  def compare091schema_invalid: Unit = {
    val avsc1 = scala.io.Source.fromFile("data/GraphStore_v0_9_1.avsc").mkString
    val avsc2 = scala.io.Source.fromFile("data/invalid.avsc").mkString
    
    SchemaCompare.compareSchema(avsc1, avsc2) match {
      case Some(reslist) => {
        println(reslist.mkString)
        assertEquals(1, reslist.size)
      }
      case None => {
        fail("schema files should differ")
      }
    }
  }
}