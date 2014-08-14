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

package org.coursera.AutoSchema

import org.coursera.autoschema.AutoSchema.createSchema

import org.coursera.autoschema.annotations.ExposeAs
import org.coursera.autoschema.annotations.FormatAs
import org.coursera.autoschema.annotations.Term

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import play.api.libs.json.Json

case class TypeOne(param1: Int)
case class TypeTwo(param1: Int, param2: Long)

case class TypeThreeParamOne(param1: String)
case class TypeThree(param1: TypeThreeParamOne, param2: Double)

case class TypeFour(param1: Int, @Term.Hide param2: Int)

@FormatAs("string")
case class TypeFiveParamOne()
case class TypeFiveParamTwo()
case class TypeFive(param1: TypeFiveParamOne, @Term.FormatAs("string") param2: TypeFiveParamTwo)

@ExposeAs[Int]
case class TypeSixParamOne()
case class TypeSixParamTwo()
case class TypeSix(param1: TypeSixParamOne, @Term.ExposeAs[Int] param2: TypeSixParamTwo)

case class RecursiveType(param1: RecursiveType)

case class MutuallyRecursiveTypeOne(param1: MutuallyRecursiveTypeTwo)
case class MutuallyRecursiveTypeTwo(param1: MutuallyRecursiveTypeOne)

class AutoSchemaTest extends AssertionsForJUnit {
  @Test
  def justAnInt: Unit = {
    assert(createSchema[Int] ===
      Json.obj(
        "type" -> "number",
        "format" -> "number"))
  }

  @Test
  def typeOne: Unit = {
    assert(createSchema[TypeOne] ===
      Json.obj(
        "title" -> "TypeOne",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "type" -> "number",
            "format" -> "number"))))
  }

  @Test
  def typeTwo: Unit = {
    assert(createSchema[TypeTwo] ===
      Json.obj(
        "title" -> "TypeTwo",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "type" -> "number",
            "format" -> "number"),
          "param2" -> Json.obj(
            "type" -> "number",
            "format" -> "number"))))
  }

  @Test
  def typeThree: Unit = {
    assert(createSchema[TypeThree] ===
      Json.obj(
        "title" -> "TypeThree",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "title" -> "TypeThreeParamOne",
            "type" -> "object",
            "properties" -> Json.obj(
              "param1" -> Json.obj(
                "type" -> "string"))),
          "param2" -> Json.obj(
            "type" -> "number",
            "format" -> "number"))))
  }

  @Test
  def typeFour: Unit = {
    assert(createSchema[TypeFour] ===
      Json.obj(
        "title" -> "TypeFour",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "type" -> "number",
            "format" -> "number"))))
  }

  @Test
  def typeFive: Unit = {
    assert(createSchema[TypeFive] ===
      Json.obj(
        "title" -> "TypeFive",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "type" -> "string"),
          "param2" -> Json.obj(
            "type" -> "string"))))
  }

  @Test
  def typeSix: Unit = {
    assert(createSchema[TypeSix] ===
      Json.obj(
        "title" -> "TypeSix",
        "type" -> "object",
        "properties" -> Json.obj(
          "param1" -> Json.obj(
            "type" -> "number",
            "format" -> "number"),
          "param2" -> Json.obj(
            "type" -> "number",
            "format" -> "number"))))
  }

  @Test
  def collections: Unit = {
    assert(createSchema[Array[Int]] ===
      Json.obj(
        "type" -> "array",
        "items" -> Json.obj(
          "type" -> "number",
          "format" -> "number")))
  }

  @Test
  def recursiveType: Unit = {
    intercept[IllegalArgumentException] {
      createSchema[RecursiveType]
    }
  }

  @Test
  def mutuallyRecursiveType: Unit = {
    intercept[IllegalArgumentException] {
      createSchema[MutuallyRecursiveTypeOne]
    }
  }
}
