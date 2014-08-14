/**
 *  Copyright 2014 Coursera Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.coursera.autoschema

import play.api.libs.json.JsArray
import play.api.libs.json.Json
import play.api.libs.json.JsObject

import scala.reflect.runtime.{universe => ru}

/**
 * AutoSchema lets you take any Scala type and create JSON Schema out of it
 * @example
 * {{{
 *      // Pass the type as a type parameter
 *      case class MyType(...)
 *
 *      AutoSchema.createSchema[MyType]
 *
 *
 *      // Or pass the reflection type
 *      case class MyOtherType(...)
 *
 *      AutoSchema.createSchema(ru.typeOf[MyOtherType])
 * }}}
 */
object AutoSchema {
  // Hand written schemas for common types
  private[this] val schemaTypeForScala = Map(
    "org.joda.time.DateTime" -> Json.obj("type" -> "string", "format" -> "date"),
    "java.util.Date" -> Json.obj("type" -> "string", "format" -> "date"),
    "java.lang.String" -> Json.obj("type" -> "string"),
    "scala.Boolean" -> Json.obj("type" -> "boolean"),
    "scala.Int" -> Json.obj("type" -> "number", "format" -> "number"),
    "scala.Long" -> Json.obj("type" -> "number", "format" -> "number"),
    "scala.Double" -> Json.obj("type" -> "number", "format" -> "number")
  )

  private[this] val classSchemaCache = collection.concurrent.TrieMap[String, JsObject]()

  private[this] val isHideAnnotation = (annotation: ru.Annotation) =>
    annotation.tpe.typeSymbol.fullName == "org.coursera.autoschema.annotations.Term.Hide"

  private[this] val isFormatAnnotation = (annotation: ru.Annotation) =>
    annotation.tpe.typeSymbol.fullName == "org.coursera.autoschema.annotations.FormatAs"

  private[this] val isExposeAnnotation = (annotation: ru.Annotation) =>
    annotation.tpe.typeSymbol.fullName == "org.coursera.autoschema.annotations.ExposeAs"

  private[this] val isTermExposeAnnotation = (annotation: ru.Annotation) =>
    annotation.tpe.typeSymbol.fullName == "org.coursera.autoschema.annotations.Term.ExposeAs"

  // Generates JSON schema based on a FormatAs annotation
  private[this] def formatAnnotationJson(annotation: ru.Annotation) = {
    annotation.scalaArgs match {
      case typ :: Nil =>
        Json.obj("type" -> typ.toString.tail.init)
      case typ :: format :: Nil =>
        Json.obj("type" -> typ.toString.tail.init, "format" -> format.toString.tail.init)
    }
  }

  private[this] def createClassJson(tpe: ru.Type, previousTypes: Set[String]) = {
    // Check if schema for this class has already been generated
    classSchemaCache.getOrElseUpdate(tpe.typeSymbol.fullName, {
      val title = tpe.typeSymbol.name.decoded
      val propertiesList = tpe.members.flatMap { member =>
        if (member.isTerm) {
          val term = member.asTerm
          if ((term.isVal || term.isVar) && !term.annotations.exists(isHideAnnotation)) {
            val termFormat = term.annotations.find(isFormatAnnotation)
              .map(formatAnnotationJson)
              .getOrElse {
              term.annotations.find(isTermExposeAnnotation)
                .map(annotation =>
                  createSchema(annotation.tpe.asInstanceOf[ru.TypeRefApi].args.head, previousTypes))
                .getOrElse(createSchema(term.typeSignature, previousTypes + tpe.typeSymbol.fullName))
            }

            Some(term.name.decoded.trim -> termFormat)
          } else {
            None
          }
        } else {
          None
        }
      }.toList.sortBy(_._1)

      val properties = JsObject(propertiesList)

      // Return the value and add it to the cache (since we're using getOrElseUpdate
      Json.obj("title" -> title, "type" -> "object", "properties" -> properties)
    })
  }

  private[this] def extendsValue(tpe: ru.Type) = {
    tpe.baseClasses.exists(_.fullName == "scala.Enumeration.Value")
  }

  private[this] def createSchema(tpe: ru.Type, previousTypes: Set[String]): JsObject = {
    val typeName = tpe.typeSymbol.fullName

    if (extendsValue(tpe)) {
      val mirror = ru.runtimeMirror(getClass.getClassLoader)
      val enumName = tpe.toString.split('.').init.mkString(".")
      val module = mirror.staticModule(enumName)
      val enum = mirror.reflectModule(module).instance.asInstanceOf[Enumeration]
      val options = enum.values.map { v =>
        Json.toJson(v.toString)
      }.toList

      val optionsArr = JsArray(options)
      Json.obj(
        "type" -> "string",
        "enum" -> optionsArr
      )
    } else if (typeName == "scala.Option") {
      // Option[T] becomes the schema of T with required set to false
      Json.obj("required" -> false) ++ createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, previousTypes)
    } else if (tpe.baseClasses.exists(s => s.fullName == "scala.collection.Traversable" ||
                                           s.fullName == "scala.Array")) {
      // (Traversable)[T] becomes a schema with items set to the schema of T
      Json.obj("type" -> "array", "items" -> createSchema(tpe.asInstanceOf[ru.TypeRefApi].args.head, previousTypes))
    } else {
      tpe.typeSymbol.annotations.find(isFormatAnnotation)
        .map(formatAnnotationJson)
        .getOrElse {
        tpe.typeSymbol.annotations.find(isExposeAnnotation)
          .map(annotation => createSchema(annotation.tpe.asInstanceOf[ru.TypeRefApi].args.head, previousTypes))
          .getOrElse {
          schemaTypeForScala.getOrElse(typeName, {
            if (tpe.typeSymbol.isClass) {
              // Check if this schema is recursive
              if (previousTypes.contains(tpe.typeSymbol.fullName)) {
                throw new IllegalArgumentException(s"Recursive types detected: $typeName")
              }

              createClassJson(tpe, previousTypes)
            } else {
              Json.obj()
            }
          })
        }
      }
    }
  }

  /**
   * Create schema based on reflection type
   * @param tpe
   * The reflection type to be converted into JSON Schema
   * @return
   * The JSON Schema for the type as a JsObject
   */
  def createSchema(tpe: ru.Type): JsObject = createSchema(tpe, Set.empty)

  /**
   *
   * @tparam T
   * The type to be converted into JSON Schema
   * @return
   * The JSON Schema for the type as a JsObject
   */
  def createSchema[T: ru.TypeTag]: JsObject = createSchema(ru.typeOf[T])
}
