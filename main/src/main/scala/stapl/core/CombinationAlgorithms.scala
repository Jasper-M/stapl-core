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


sealed trait CombinationDecision
object CombinationDecision {
  final case class Final(decision: Decision) extends CombinationDecision
  final case class Temporary(decision: Decision) extends CombinationDecision
}

trait CombinationAlgorithm {

  def combine(previous: Decision, recent: Decision): CombinationDecision
}

import CombinationDecision._

case object PermitOverrides extends CombinationAlgorithm {
  
  def combine(previous: Decision, recent: Decision): CombinationDecision = recent match {
    case Permit        => Final(Permit)
    case Deny          => Temporary(Deny)
    case NotApplicable => Temporary(previous)
  }
}
case object DenyOverrides extends CombinationAlgorithm {
  
  def combine(previous: Decision, recent: Decision): CombinationDecision = recent match {
    case Deny          => Final(Deny)
    case Permit        => Temporary(Permit)
    case NotApplicable => Temporary(previous)
  }
}
case object FirstApplicable extends CombinationAlgorithm {
  
  def combine(previous: Decision, recent: Decision): CombinationDecision = recent match {
    case Permit | Deny => Final(recent)
    case NotApplicable => Temporary(previous)
  }
}
