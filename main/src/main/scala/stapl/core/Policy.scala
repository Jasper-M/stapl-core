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

import grizzled.slf4j.Logging
import stapl.core.pdp.EvaluationCtx

/**
 * *******************************************
 * The basic constructors
 */
abstract class AbstractPolicy(val id: String) {
  protected[core] var parent: Option[Policy] = None

  //def allIds: List[String]

  /**
   * Returns the ordered list of all ids from the top of the policy tree
   * to this element of the policy tree, this element first and working to the top.
   */
  def treePath: List[String] = parent match {
    case Some(parent) => id :: parent.treePath
    case None => List(id)
  }

  /**
   * Returns the fully qualified id of this element of the policy tree.
   * This id is the concatenation of all ids of the elements on the tree
   * path of this element, starting from the top and working down.
   */
  def fqid: String = treePath.reverse.mkString(">") // TODO performance optimization: cache this stuff
}

/**
 * Represents one rule.
 */
case class Rule(override val id: String)(val effect: Effect,
  val condition: Expression, val obligationActions: List[ObligationAction])
  extends AbstractPolicy(id) {

  //override def allIds: List[String] = List(id)

  override def toString = s"Policy #$fqid"
}

/**
 * Represents a policy of one or more rules and/or subpolicies.
 */
case class Policy(override val id: String, val target: Expression, val pca: CombinationAlgorithm,
  val subpolicies: List[AbstractPolicy], val obligations: List[Obligation])
  extends AbstractPolicy(id) with Logging {

  // assign this PolicySet as parent to the children
  subpolicies.foreach(_.parent = Some(this))

  require(!subpolicies.isEmpty, "A PolicySet needs at least one SubPolicy")
  //require(uniqueIds, "All policies require a unique ID")

  /*private def uniqueIds(): Boolean = {
    val ids = allIds
    val distinctIds = ids.distinct
    distinctIds.size == ids.size
  }*/

  //override def allIds: List[String] = id :: subpolicies.flatMap(_.allIds)

  override def toString = {
    val subs = subpolicies.toString
    s"PolicySet #$id = [${subs.substring(5, subs.length - 1)}]"
  }
}

/**
 * Represents a reference to a policy that resides at a remote location and should be evaluated
 * at that remote location.
 *
 * TODO Do remote policy references reference the id or the fqid?
 */
case class RemotePolicy(override val id: String) extends AbstractPolicy(id)
