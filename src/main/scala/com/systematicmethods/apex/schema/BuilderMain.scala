package com.systematicmethods.apex.schema

import joptsimple.OptionSet
import joptsimple.OptionParser

object BuilderMain extends App {
  val parser = new OptionParser
  val entities = parser.accepts("entities", "entities csv file").withRequiredArg()
  val properties = parser.accepts("properties", "properties csv file").withRequiredArg()
  val validRelationships = parser.accepts("validRelationships", "validRelationships csv file").withRequiredArg()
  val relationshipGroups = parser.accepts("relationshipGroups", "relationshipGroups csv file").withRequiredArg()
  val entityGroups = parser.accepts("entityGroups", "entityGroups csv file").withRequiredArg()
  val relationships = parser.accepts("relationships", "relationships csv file").withRequiredArg()
  val namespace = parser.accepts("namespace", "schema base namespace").withRequiredArg()
  parser.accepts("help", "shows this help").forHelp()

  val options: Option[OptionSet] = try {
    Option(parser.parse(args: _*))
  } catch {
    case (ex: Exception) => {
      System.err.println("Invalid option: " + ex.getLocalizedMessage())
      None
    }
  }

  options match {
    case Some(opts) =>
      if (opts.hasOptions) {
        try {
          val builder = SchemaReferenceBuilder(
            namespace.value(opts),
            entities.value(opts),
            relationships.value(opts),
            properties.value(opts),
            validRelationships.value(opts),
            relationshipGroups.value(opts),
            entityGroups.value(opts)
          )
          builder.buildAvroSchema() match {
            case Some(schema) => println(schema.toString(true))
            case None => System.err.println("Invalid schema data")
          }
        } catch {
          case (ex:Exception) =>
            parser.printHelpOn(System.err)
        }
      }
      else {
        parser.printHelpOn(System.err)
      }
    case None => parser.printHelpOn(System.err)
  }

}
