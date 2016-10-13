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

import grizzled.slf4j.Logging
import stapl.core.Attribute
import stapl.core.AttributeNotFoundException
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ACTION
import stapl.core.ENVIRONMENT
import scala.collection.mutable

/**
 * The base class of the context for evaluating a policy. This context
 * represents all information for that policy evaluation, such as the
 * id of the subject, the id of the resource, the id of the action and
 * a method to find attributes.
 *
 * The method to find attributes is required in the evaluation context
 * because certain aspects such as an attribute cache are specific for
 * each individual evaluation context.
 */
trait EvaluationCtx {

  def evaluationId: String
  def subjectId: String
  def resourceId: String
  def actionId: String
  def remoteEvaluator: RemoteEvaluator
  def cachedAttributes: Map[Attribute[_], Any]
  protected[core] def findAttribute[T](attribute: Attribute[T]): T
  
  // TODO add type checking here
  //final def findAttribute(attribute: Attribute): ConcreteValue = 
}

/**
 * An implementation of a basic evaluation context. This evaluation context
 * stores the subject id, the resource id, the action id and stores found
 * attribute values in a cache for this evaluation context.
 */
class BasicEvaluationCtx(override val evaluationId: String, request: RequestCtx,
  finder: AttributeFinder, override val remoteEvaluator: RemoteEvaluator) extends EvaluationCtx with Logging {

  override val subjectId: String = request.subjectId.getOrElse(throw new AttributeNotFoundException(evaluationId, "???", Attributes.subjectId))

  override val resourceId: String = request.resourceId.getOrElse(throw new AttributeNotFoundException(evaluationId, "???", Attributes.resourceId))

  override val actionId: String = request.actionId.getOrElse(throw new AttributeNotFoundException(evaluationId, "???", Attributes.actionId))

  protected val attributeCache: mutable.Map[Attribute[_], Any] = mutable.Map(request.attributes.toSeq :_*) //scala.collection.concurrent.TrieMap()

  override def cachedAttributes: Map[Attribute[_], Any] = attributeCache.toMap

  /**
   * Try to find the value of the given attribute. If the value is already
   * in the attribute cache, that value is returned. Otherwise, the attribute
   * finder is checked and the found value is stored in the attribute cache if
   * a value is found.
   *
   * @throws	AttributeNotFoundException	If the attribute value isn't found
   */
  @throws[AttributeNotFoundException]("if the attribute value isn't found")
  override def findAttribute[T](attribute: Attribute[T]): T = {
    attributeCache.get(attribute) match {
      case Some(value) => {
        debug("FLOW: found value of " + attribute + " in cache: " + value)
        value.asInstanceOf[T]
      }
      case None => { // Not in the cache
        finder.find(this, attribute) match {
          case None =>
            val entityId = attribute.category match {
              case SUBJECT => subjectId
              case RESOURCE => resourceId
              case ACTION => "ACTION??" // we don't support this
              case ENVIRONMENT => "ENVIRONMENT??" // we don't support this
            }
            debug(s"Didn't find value of $attribute for entity $entityId anywhere, throwing exception")
            throw new AttributeNotFoundException(evaluationId, entityId, attribute)
          case Some(value) =>
            attributeCache(attribute) = value // add to cache
            debug("FLOW: retrieved value of " + attribute + ": " + value + " and added to cache")
            value.asInstanceOf[T]
        }
      }
    }
  }

}
