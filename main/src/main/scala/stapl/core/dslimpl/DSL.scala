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

package stapl.core.dslimpl

import stapl.core._
import scala.language.implicitConversions

/**
 * **************************************
 * The more natural DSL for policies and policy sets
 *
 * Examples for policies:
 *  Policy("policy1") := when ("role" in subject.roles) deny iff (subject.allowed === false)
 *  Policy("policy2") := deny iff (subject.allowed === false)
 *  Policy("policy3") := when ("role" in subject.roles) deny
 *  Policy("policy4") := deny
 *
 * Examples for policy sets:
 *  TODO
 *
 * FIXME the "policy" in this line should not be possible:
 *  Policy("view document") := when (action.id === "view" & resource.type_ === "document") permit
 *  ====== Why not? ====== => because this is a policy and the keyword "permit" should not be in here
 *
 * TODO does a rule need a target?
 */
trait DSL {
  class OnlyIdRule(private val id: String) {
  
    def :=(t: EffectConditionAndObligationActions): Rule =
      new Rule(id, t.effect, t.condition, List(t.obligationActions: _*))
  
    def :=(t: EffectAndCondition): Rule =
      new Rule(id, t.effect, t.condition, List.empty)
  
    def :=(t: EffectAndObligationActions): Rule =
      new Rule(id, t.effect, Value(true), List(t.obligationActions: _*))
  
    def :=(effectKeyword: EffectKeyword): Rule = effectKeyword match {
      case `deny` => new Rule(id, Deny, Value(true), List.empty)
      case `permit` => new Rule(id, Permit, Value(true), List.empty)
    }
  
  }
  
  class OnlyIdPolicy(private val id: String) {
  
    def :=(t: TargetPCASubpoliciesAndObligations): Policy =
      new Policy(id, t.target, t.pca, t.subpolicies, t.obligations)
  
    def :=(t: TargetPCAAndSubpolicies): Policy =
      new Policy(id, t.target, t.pca, List(t.subpolicies: _*), List.empty)
  }
  
  implicit class ObligationActionWithOn(val obligationAction: ObligationAction) {
    def on(effect: EffectKeyword): Obligation = effect match {
      case `deny` => new Obligation(obligationAction, Deny)
      case `permit` => new Obligation(obligationAction, Permit)
    }
  }
  
  
  class EffectAndCondition(val effect: Effect, val condition: Expression) {
  
    def performing(obligationActions: ObligationAction*): EffectConditionAndObligationActions =
      new EffectConditionAndObligationActions(effect, condition, obligationActions: _*)
  }
  
  class EffectConditionAndObligationActions(
    val effect: Effect, val condition: Expression, val obligationActions: ObligationAction*)
  
  class EffectAndObligationActions(
    val effect: Effect, val obligationActions: ObligationAction*)
  
  class EffectKeyword // FIXME this cannot be the best way to do this...
  case object deny extends EffectKeyword {
    /**
     * Needed if no target is given
     */
    def iff(condition: Expression): EffectAndCondition =
      new EffectAndCondition(Deny, condition)
  
    def performing(obligationActions: ObligationAction*): EffectAndObligationActions =
      new EffectAndObligationActions(Deny, obligationActions: _*)
  }
  case object permit extends EffectKeyword {
    /**
     * Needed if no target is given
     */
    def iff(condition: Expression): EffectAndCondition =
      new EffectAndCondition(Permit, condition)
  
    def performing(obligationActions: ObligationAction*): EffectAndObligationActions =
      new EffectAndObligationActions(Permit, obligationActions: _*)
  }
  
  class TargetPCAAndSubpolicies(val target: Expression, val pca: CombinationAlgorithm, val subpolicies: AbstractPolicy*) {
  
    def performing(obligations: Obligation*): TargetPCASubpoliciesAndObligations =
      new TargetPCASubpoliciesAndObligations(target, pca, List(subpolicies: _*), List(obligations: _*))
  }
  
  class TargetPCASubpoliciesAndObligations(val target: Expression, val pca: CombinationAlgorithm,
    val subpolicies: List[AbstractPolicy], val obligations: List[Obligation])
  
  class TargetAndPCA(val target: Expression, val pca: CombinationAlgorithm) {
  
    def to(subpolicies: AbstractPolicy*): TargetPCAAndSubpolicies =
      new TargetPCAAndSubpolicies(target, pca, subpolicies: _*)
  }
  
  class OnlyTarget(val target: Expression) {
  
    def apply(pca: CombinationAlgorithm): TargetAndPCA =
      new TargetAndPCA(target, pca)
  
  }
  def when(target: Expression): OnlyTarget = new OnlyTarget(target)
  
  object always {
    def apply(pca: CombinationAlgorithm): TargetAndPCA =
      new TargetAndPCA(Value(true), pca)
  }
  
  class OnlySubpolicies(val subpolicies: AbstractPolicy*)
  def to(subpolicies: AbstractPolicy*): OnlySubpolicies = new OnlySubpolicies(subpolicies: _*)
  
  def iff(condition: Expression): Expression = condition
  
  def Rule(id: String) = new OnlyIdRule(id)
  
  def Policy(id: String) = new OnlyIdPolicy(id)
  
  
  object log {
    def apply(msg: Value[String]) = new LogObligationAction(msg)
  }
  object mail {
    def apply(to: String, msg: String) = new MailObligationAction(to, msg)
  }
  /**
   * Updating attribute values
   */
  object update {
    def apply[T](attribute: Attribute[T], value: Value[T]) =
      new ChangeAttributeObligationAction(attribute, value, Update)
  }
  
  /**
   * Appending to attribute values
   */
  object append {
    def apply[T](attribute: Attribute[T], value: Value[T]) =
      new ChangeAttributeObligationAction(attribute, value, Append)
  }
  
  object evict {
    def apply(attributes: Attribute[_]*) = EvictObligationAction(attributes.toSet)
  }
}
