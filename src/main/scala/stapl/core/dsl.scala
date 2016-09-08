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
  
  /**
   * You might want to manually wrap something in a Value. Use with care.
   */
  
  type CombinationAlgorithm = stapl.core.CombinationAlgorithm
  
  val PermitOverrides = stapl.core.PermitOverrides
  val DenyOverrides = stapl.core.DenyOverrides
  val FirstApplicable = stapl.core.FirstApplicable
  val Permit = stapl.core.Permit
  val Deny = stapl.core.Deny
  
  
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
      
      val typeOfValue = typeOf[Value[_]]
      val ctxName = TermName(c.freshName())
      
      val transformer = new Transformer {
        private def isStaplFromValue(symbol: Symbol): Boolean =
          symbol.isMethod && symbol.asMethod.annotations.exists(a => a.tree.tpe =:= typeOf[staplFromValue])
        private def isOfTypeValue(tree: c.Tree): Boolean =
          tree.tpe != null && tree.tpe <:< typeOfValue
        override def transform(tree: c.Tree) = tree match {
          case q"$method[..$_]($value)" if isStaplFromValue(method.symbol) => 
            q"$value.getConcreteValue($ctxName)"
          case _ if isOfTypeValue(tree) => 
            q"$tree.getConcreteValue($ctxName)"
          case _ => super.transform(tree)
        }
      }
      
      val transformed = transformer.transform(expr)
      
      /*if(transformed.exists(t => t.tpe != null && t.tpe <:< typeOf[Value[_]]))
        c.error(c.enclosingPosition, "A Value did not get converted!")*/
      
      val ctxParam = ValDef(Modifiers(Flag.PARAM), ctxName, TypeTree(), EmptyTree)
      val result = q"_root_.stapl.core.Value($ctxParam => $transformed)"
      c.untypecheck(result)
    }
  }
}
