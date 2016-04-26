package com.systematicmethods.apex.schema

import org.junit.Test
import java.io.StringWriter
import java.io.PrintWriter

class SchemaReferenceBuilderTest {
  @Test
  def testbuild_entities: Unit = {
    val str = new StringWriter
    val out = new PrintWriter(str)
    SchemaReferenceBuilder2.buildSchema("org.profile.customer.avro", out)
    val avsc = str.toString()
    println(avsc)
    org.apache.avro.Schema.parse(avsc)
  }
}