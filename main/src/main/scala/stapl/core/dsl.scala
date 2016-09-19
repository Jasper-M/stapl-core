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

import stapl.core.dslimpl._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.annotation.StaticAnnotation

object dsl extends DSL with JodaTime {
  
  object any2stringadd // DIE any2stringadd DIE !!!
  
  type CombinationAlgorithm = stapl.core.CombinationAlgorithm
  
  val PermitOverrides = stapl.core.PermitOverrides
  val DenyOverrides = stapl.core.DenyOverrides
  val FirstApplicable = stapl.core.FirstApplicable
  /*val Permit = stapl.core.Permit
  val Deny = stapl.core.Deny*/
  
  
  private[dsl] abstract class SubjectTemplate extends AttributeContainer(SUBJECT) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ResourceTemplate extends AttributeContainer(RESOURCE) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ActionTemplate extends AttributeContainer(ACTION) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class EnvironmentTemplate extends AttributeContainer(ENVIRONMENT)
  
  trait Subject extends SubjectTemplate
  trait Resource extends ResourceTemplate
  trait Action extends ActionTemplate
  trait Environment extends EnvironmentTemplate
  
  type Expression = stapl.core.Expression
  
  
  
  

  
  private final class staplFromValue extends StaticAnnotation
  
  @compileTimeOnly("fromValue should not be called")
  @staplFromValue
  implicit def fromValue[T](v: Value[T]): T = ???

  
  implicit def convertToValue[T](expr: T): Value[T] = macro Macros.ruleMacro
  
  object Macros {
    
    def ruleMacro(c: Context)(expr: c.Tree): c.Tree = {
      import c.universe._
      
      val typeOfValue = typeOf[Value[_]] // typeOf[Value[_]] is unknown inside Transformer...
      val ctxName = TermName(c.freshName())
      
      var values: List[(TermName, c.Tree)] = Nil
      
      def registerValue(value: c.Tree): c.Tree = {
        val name = TermName(c.freshName())
        values = (name, value) :: values
        q"$name"
      }
      
      val transformer = new Transformer {
        private def isStaplFromValue(symbol: Symbol): Boolean =
          symbol.isMethod && symbol.asMethod.annotations.exists(a => a.tree.tpe =:= typeOf[staplFromValue])
        private def isOfTypeValue(tree: c.Tree): Boolean =
          tree.tpe != null && tree.tpe <:< typeOfValue
        override def transform(tree: c.Tree) = tree match {
          case q"$method[..$_]($value)" if isStaplFromValue(method.symbol) => 
            val name = registerValue(value)
            q"$name.getConcreteValue($ctxName)"
          case _ if isOfTypeValue(tree) => 
            val name = registerValue(tree)
            q"$name.getConcreteValue($ctxName)"
          case _ => super.transform(tree)
        }
      }
      
      import org.scalamacros.resetallattrs._
      val transformed = c.resetAllAttrs(transformer.transform(expr))
      
      val vals = values map { case (name, value) => q"val $name = $value" }
      val names = values map (_._1)
      
      val ctxParam = q"val $ctxName = $EmptyTree" //ValDef(Modifiers(Flag.PARAM), ctxName, TypeTree(), EmptyTree)
      val result = q"..$vals; _root_.stapl.core.Value(_root_.scala.collection.immutable.Set(..$names), $ctxParam => $transformed)"
      result
    }
  }
}
