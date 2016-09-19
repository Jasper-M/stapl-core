/*
 * Copyright 2016 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package stapl.core

import scala.language.experimental.macros
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class AttributeDeclarationException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause) 

/**
 * Base class for all attribute containers, such as the subject, resource, action and environment
 * in most STAPL policies.
 * 
 * Usage example: 
 * {{{
 * object subject extends AttributeContainer(Subject) {
 *   val foo = Attribute[String]
 *   val bar = Attribute[List[Int]]
 *   val baz = Attribute[String]("bazz")
 * }
 * }}}
 * 
 * !!!!!!!!!!!!!!
 * !!
 * !! TODO this should be part of dsl
 * !!
 * !!!!!!!!!!!!!!
 */
abstract class AttributeContainer (category: Category) {

  protected final def Attribute[T: TypeTag](name: String): Attribute[T] = {
    new Attribute[T](category, name, typeOf[T])
  }

  protected final def Attribute[T: TypeTag](implicit name: sourcecode.Name): Attribute[T] = {
    Attribute(name.value)
  }
}

/**
 * All attribute container types for now.
 * 
 * TODO: extend this to support the container hierarchies as well?
 */
sealed abstract class Category
case object SUBJECT extends Category
case object RESOURCE extends Category
case object ENVIRONMENT extends Category
case object ACTION extends Category
