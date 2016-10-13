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
import scala.util.control.NoStackTrace
import stapl.core.Policy
import stapl.core.Rule
import stapl.core.Decision
import stapl.core.Permit
import stapl.core.Value
import stapl.core.Effect
import scala.collection.mutable
import scala.collection.concurrent

class PartialEvaluationPDP(policy: AbstractPolicy,
  //attributeFinder: AttributeFinder = new AttributeFinder,
  obligationService: ObligationService = new ObligationService,
  remoteEvaluator: RemoteEvaluator = new RemoteEvaluator) extends Logging {

  import PartialEvaluationPDP._
  
  private val timestampGenerator = new SimpleTimestampGenerator
  
  private val partials: concurrent.Map[String, AbstractPolicy] = concurrent.TrieMap()

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes) and empty remote evaluator.
   */
  def this(policy: AbstractPolicy) = this(policy, new ObligationService, new RemoteEvaluator)

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes).
   */
  def this(policy: AbstractPolicy, remoteEvaluator: RemoteEvaluator) = this(policy, new ObligationService, remoteEvaluator)

  
  def expire(evaluationId: String) {
    partials.remove(evaluationId)
  }

  /**
   * Evaluate the policy of this PDP with given subject id, action id, resource id
   * and possibly extra attributes and return the result.
   * This will employ the attribute finder of this PDP.
   */
  def evaluate(attributes: Map[Attribute[_], Any]): Either[String, Result] =
    evaluate(new RequestCtx(attributes))
    
  def evaluate(evaluationId: String, attributes: Map[Attribute[_], Any]): Either[String, Result] =
    evaluate(evaluationId, new RequestCtx(attributes))

  /**
   * Evaluate the policy of this PDP with given request context and generated incrementing
   * evaluation id and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(ctx: RequestCtx): Either[String, Result] =
    evaluate(new PEEvaluationCtx(timestampGenerator.getTimestamp, ctx, remoteEvaluator), policy)

  /**
   * Evaluate the policy of this PDP with given evaluation id and request context
   * and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(evaluationId: String, ctx: RequestCtx): Either[String, Result] =
    evaluate(new PEEvaluationCtx(timestampGenerator.getTimestamp, ctx, remoteEvaluator), partials.getOrElse(evaluationId, throw new RuntimeException(s"Unknown evaluation id: $evaluationId")))

  
  private def evaluate(ctx: EvaluationCtx, policy: AbstractPolicy): Either[String, Result] = {
    val peresult = evaluateAbstractPolicy(policy, ctx)
    val resultOpt = peresult.result.map { result =>
      // try to fulfill the obligations
      val remainingObligations = ListBuffer[ConcreteObligationAction]()
      for (obl <- result.obligationActions) {
        if (!obligationService.fulfill(obl)) {
          remainingObligations += obl
        }
      }
      // return the result with the remaining obligations
      Result(result.decision, remainingObligations.toList)
    }
    
    resultOpt match {
      case None => 
        partials.putIfAbsent(ctx.evaluationId, peresult.policy)
        Left(ctx.evaluationId)
      case Some(result) => Right(result)
    }
  }
  
  private def evaluateAbstractPolicy(policy: AbstractPolicy, ctx: EvaluationCtx) = policy match {
    case completed: CompletedPolicy => evaluateCompletedPolicy(completed)
    case policy: Policy       => evaluatePolicy(policy, ctx)
    case rule: Rule           => evaluateRule(rule, ctx)
    case remote: RemotePolicy => evaluateRemotePolicy(remote, ctx)
  }
  
  private def evaluateCompletedPolicy(policy: CompletedPolicy) = {
    PEResult(Some(policy.result), policy, Set())
  }
  
  private def evaluatePolicy(policy: Policy, ctx: EvaluationCtx): PEResult = {
    import policy._
    debug(s"FLOW: starting evaluation of PolicySet #$fqid")
    try {
      val concreteTarget = policyIsApplicable(policy, ctx)
      if (concreteTarget) {
        val pecresult = combinePolicies(subpolicies, pca, ctx)
        (pecresult.result.map { result =>
          // add applicable obligations of our own
          val applicableObligationActions = result.obligationActions ::: obligations.withFilter(_.fulfillOn == result.decision).map(_.action.getConcrete(ctx))
          val finalResult = Result(result.decision, applicableObligationActions)
          debug(s"FLOW: PolicySet #$fqid returned $finalResult")
          PEResult(Some(finalResult), CompletedPolicy(policy.id, finalResult), Set())
        } orElse {
          Some( PEResult(None, policy.copy(target = Value(true), subpolicies = pecresult.policies), pecresult.attributes) )
        }).get
      } else {
        debug(s"FLOW: PolicySet #$fqid was NotApplicable because of target")
        val finalResult = Result(NotApplicable)
        PEResult(Some(finalResult), CompletedPolicy(policy.id, finalResult), Set())
      }
    } catch {
      case AttributeNotFound => PEResult(None, policy, policy.target.dependencies)
    }
  }
  
  private def combinePolicies(policies: List[AbstractPolicy], pca: CombinationAlgorithm, ctx: EvaluationCtx): PECResult = {
    import CombinationDecision._
    var tmpResult = Result(NotApplicable)
    var error = false
    var attributes: Set[Attribute[_]] = Set()
    val resultPolicies = for(policy <- policies) yield {
      if (!error) {
        val peresult = evaluateAbstractPolicy(policy, ctx)
        if (peresult.result.nonEmpty) {
          val Result(currentDecision, obligationActions) = peresult.result.get
          pca.combine(tmpResult.decision, currentDecision) match {
            case Temporary(decision) => tmpResult = Result(decision, tmpResult.obligationActions ::: obligationActions)
            case Final(decision) => return PECResult(Some(Result(decision, obligationActions)),List(),Set())
          }
          CompletedPolicy(policy.id, peresult.result.get)
        } else {
          error = true
          attributes = peresult.attributes
          policy
        }
      } else {
        policy
      }
    }
    if (!error) PECResult(Some(tmpResult), List(), Set())
    else PECResult(None, resultPolicies, attributes)
  }
  
  private def policyIsApplicable(policy: Policy, ctx: EvaluationCtx): Boolean = policy.target.getConcreteValue(ctx)
  
  private def evaluateRule(rule: Rule, ctx: EvaluationCtx) = {
    import rule._
    debug(s"FLOW: starting evaluation of Policy #$fqid (evaluation id #${ctx.evaluationId})")
    try {
      val concreteCondition = condition.getConcreteValue(ctx)
      val result = if (concreteCondition) {
        debug(s"FLOW: Rule #$fqid returned $effect with obligations $obligationActions")
        Result(effect, obligationActions map { _.getConcrete(ctx) })
      } else {
        debug(s"FLOW: Rule #$fqid was NotApplicable because of condition")
        Result(NotApplicable)
      }
      PEResult(Some(result), CompletedPolicy(rule.id, result), Set())
    } catch {
      case AttributeNotFound => PEResult(None, rule, condition.dependencies)
    }
  }
  
  private def evaluateRemotePolicy(policy: RemotePolicy, ctx: EvaluationCtx) = {
    import policy._
    debug(s"FLOW: starting evaluation of Remote Policy #$fqid (evaluation id #${ctx.evaluationId})")
    val result = ctx.remoteEvaluator.findAndEvaluate(policy.id, ctx)
    // TODO Filter obligations? 
    //   - I don't think so?
    debug(s"FLOW: Remote Policy #$fqid returned $result")
    PEResult(Some(result), policy, Set())
  }
}

object PartialEvaluationPDP {
  
  private final case class PEResult(result: Option[Result], policy: AbstractPolicy, attributes: Set[Attribute[_]])
  private final case class PECResult(result: Option[Result], policies: List[AbstractPolicy], attributes: Set[Attribute[_]])
  
  private case class CompletedPolicy(override val id: String, result: Result) extends AbstractPolicy(id)
  
  private object AttributeNotFound extends Throwable with NoStackTrace
  
  private final class PEEvaluationCtx(override val evaluationId: String, request: RequestCtx,
    override val remoteEvaluator: RemoteEvaluator) extends EvaluationCtx {
  
    override def subjectId: String = request.subjectId.getOrElse(throw AttributeNotFound)
  
    override def resourceId: String = request.resourceId.getOrElse(throw AttributeNotFound)
  
    override def actionId: String = request.actionId.getOrElse(throw AttributeNotFound)
  
  
    override def cachedAttributes: Map[Attribute[_], Any] = request.attributes.toMap

  
    override def findAttribute[T](attribute: Attribute[T]): T = {
      request.get(attribute) match {
        case Some(value) => {
          value.asInstanceOf[T]
        }
        case None => throw AttributeNotFound
      }
    }
  }
}