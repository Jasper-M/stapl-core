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

import stapl.core.AbstractPolicy
import stapl.core.Attribute
import stapl.core.Decision
import stapl.core.Result
import scala.collection.mutable.ListBuffer
import stapl.core.ObligationAction
import stapl.core.ConcreteObligationAction
import stapl.core.Policy
import stapl.core.Rule
import stapl.core.RemotePolicy
import stapl.core.NotApplicable
import stapl.core.Result
import stapl.core.CombinationAlgorithm
import stapl.core.CombinationDecision
import grizzled.slf4j.Logging

/**
 * Class used for representing a policy decision point (PDP). A PDP provides
 * access decisions by evaluating a policy.
 */
class PDP(policy: AbstractPolicy,
  attributeFinder: AttributeFinder = new AttributeFinder,
  obligationService: ObligationService = new ObligationService,
  remoteEvaluator: RemoteEvaluator = new RemoteEvaluator) extends Logging {

  private val timestampGenerator = new SimpleTimestampGenerator

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes) and empty remote evaluator.
   */
  def this(policy: AbstractPolicy) = this(policy, new AttributeFinder, new ObligationService, new RemoteEvaluator)

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes).
   */
  def this(policy: AbstractPolicy, remoteEvaluator: RemoteEvaluator) = this(policy, new AttributeFinder, new ObligationService, remoteEvaluator)

  /**
   * Set up this new PDP with an empty remote evaluator.
   */
  def this(policy: AbstractPolicy, attributeFinder: AttributeFinder) = this(policy, attributeFinder, new ObligationService, new RemoteEvaluator)

  /**
   * Evaluate the policy of this PDP with given subject id, action id, resource id
   * and possibly extra attributes and return the result.
   * This will employ the attribute finder of this PDP.
   */
  def evaluate(subjectId: String, actionId: String,
    resourceId: String, extraAttributes: Map[Attribute[_], Any] = Map()): Result =
    evaluate(new RequestCtx(extraAttributes ++ Map((Attributes.subjectId -> subjectId), (Attributes.actionId -> actionId), (Attributes.resourceId -> resourceId))))

  /**
   * Evaluate the policy of this PDP with given request context and generated incrementing
   * evaluation id and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(ctx: RequestCtx): Result =
    evaluate(timestampGenerator.getTimestamp, ctx)

  /**
   * Evaluate the policy of this PDP with given evaluation id and request context
   * and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(evaluationId: String, ctx: RequestCtx): Result =
    evaluate(new BasicEvaluationCtx(evaluationId, ctx, attributeFinder, remoteEvaluator))

  /**
   * Evaluate the policy of this PDP with given evaluation context and return
   * the result.
   * This allows you to specify another attribute finder than the one of this PDP.
   *
   * The PDP will try to fulfill all obligations using the ObligationService and removes
   * the fulfilled obligations from the result.
   */
  def evaluate(ctx: EvaluationCtx): Result = {
    val result = evaluateAbstractPolicy(policy, ctx)
    // try to fulfill the obligations
    val remainingObligations = ListBuffer[ConcreteObligationAction]()
    for (obl <- result.obligationActions) {
      if (!obligationService.fulfill(obl)) {
        remainingObligations += obl
      }
    }
    // return the result with the remaining obligations
    new Result(result.decision, remainingObligations.toList)
  }
  
  private def evaluateAbstractPolicy(policy: AbstractPolicy, ctx: EvaluationCtx) = policy match {
    case policy: Policy       => evaluatePolicy(policy, ctx)
    case rule: Rule           => evaluateRule(rule, ctx)
    case remote: RemotePolicy => evaluateRemotePolicy(remote, ctx)
  }
  
  private def evaluatePolicy(policy: Policy, ctx: EvaluationCtx) = {
    import policy._
    debug(s"FLOW: starting evaluation of PolicySet #$fqid")
    if (policyIsApplicable(policy, ctx)) {
      val result = combinePolicies(subpolicies, pca, ctx)
      // add applicable obligations of our own
      val applicableObligationActions = result.obligationActions ::: obligations.withFilter(_.fulfillOn == result.decision).map(_.action.getConcrete(ctx))
      val finalResult = Result(result.decision, applicableObligationActions)
      debug(s"FLOW: PolicySet #$fqid returned $finalResult")
      finalResult
    } else {
      debug(s"FLOW: PolicySet #$fqid was NotApplicable because of target")
      Result(NotApplicable)
    }
  }
  
  private def combinePolicies(policies: List[AbstractPolicy], pca: CombinationAlgorithm, ctx: EvaluationCtx): Result = {
    import CombinationDecision._
    var tmpResult = Result(NotApplicable)
    for(policy <- policies) {
      val Result(currentDecision, obligationActions) = evaluateAbstractPolicy(policy, ctx)
      pca.combine(tmpResult.decision, currentDecision) match {
        case Temporary(decision) => tmpResult = Result(decision, tmpResult.obligationActions ::: obligationActions)
        case Final(decision) => return Result(decision, obligationActions)
      }
    }
    tmpResult
  }
  
  private def policyIsApplicable(policy: Policy, ctx: EvaluationCtx): Boolean = policy.target.getConcreteValue(ctx)
  
  private def evaluateRule(rule: Rule, ctx: EvaluationCtx) = {
    import rule._
    debug(s"FLOW: starting evaluation of Policy #$fqid (evaluation id #${ctx.evaluationId})")
    if (condition.getConcreteValue(ctx)) {
      debug(s"FLOW: Rule #$fqid returned $effect with obligations $obligationActions")
      Result(effect, obligationActions map { _.getConcrete(ctx) })
    } else {
      debug(s"FLOW: Rule #$fqid was NotApplicable because of condition")
      Result(NotApplicable)
    }
  }
  
  private def evaluateRemotePolicy(policy: RemotePolicy, ctx: EvaluationCtx) = {
    import policy._
    debug(s"FLOW: starting evaluation of Remote Policy #$fqid (evaluation id #${ctx.evaluationId})")
    val result = ctx.remoteEvaluator.findAndEvaluate(policy.id, ctx)
    // TODO Filter obligations? 
    //   - I don't think so?
    debug(s"FLOW: Remote Policy #$fqid returned $result")
    result
  }
}
