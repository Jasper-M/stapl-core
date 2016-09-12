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

package stapl.core.pdp

import scala.annotation.tailrec
import stapl.core.Attribute
import stapl.core.AttributeContainerType
import scala.reflect.runtime.universe.Type

/**
 * Class used for representing an attribute finder used by a PDP. An attribute
 * finder tries to find values for a certain value during policy evaluation.
 * An attribute finder contains multiple attribute finder modules that each
 * connect to a certain attribute value source, such as a hard-coded list, a file or
 * a database.
 */
sealed class AttributeFinder extends Modules[AttributeFinderModule] {

  /**
   * Tries to find the value of a certain attribute in the given evaluation context.
   */
  def find(ctx: EvaluationCtx, attribute: Attribute[_]): Option[Any] = {
    @tailrec
    def find(modules: List[AttributeFinderModule]): Option[Any] = modules match {
      case module :: tail => module.find(ctx, attribute) match {
        case result @ Some(_) => result
        case None => find(tail)
      }
      case Nil => None
    }
    find(modules)
  }
}

/**
 * Trait for all attribute finder modules passed to an attribute finder.
 */
trait AttributeFinderModule {
  
  /**
   * The public method for trying to find the value of a certain attribute.
   * 
   * TODO why is this private[core]?
   */
  private[core] def find(ctx: EvaluationCtx, attribute: Attribute[_]): Option[Any] = attribute match {
    case Attribute(cType,name, tpe) => find(ctx, cType, name, tpe)
  }
  
  /**
   * The actual implementation for trying to find the value of a certain attribute.
   */
  protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, tpe: Type): Option[Any]
  
}
