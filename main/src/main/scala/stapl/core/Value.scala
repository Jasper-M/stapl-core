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

import stapl.core.pdp.EvaluationCtx
import scala.language.implicitConversions

/**
 * The base trait for all elements of expressions that encapsulate concrete values. 
 * 
 * @tparam T The type of the concrete value this Value represents.
 */
trait Value[T] {
  
  def getConcreteValue(ctx: EvaluationCtx): T
  
  private[core] def dependencies: Set[Attribute[_]]
}

object Value {
  
  def apply[T](deps: Set[Value[_]], f: EvaluationCtx => T): Value[T] = new Value[T] {
    def getConcreteValue(ctx: EvaluationCtx): T = f(ctx)
    
    private[core] lazy val dependencies = deps flatMap (_.dependencies)
  }
  
  def apply[T](value: T): Value[T] = apply(Set(), _ => value)
}
