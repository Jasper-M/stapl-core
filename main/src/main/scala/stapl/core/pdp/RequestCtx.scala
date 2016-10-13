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

import stapl.core.Attribute
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ACTION
import scala.reflect.runtime.universe.typeOf

/**
 * A class used for representing the context of a request.
 * For now, this is nothing more than the ids of the subject,
 * the action and the resource and any other attribute values given
 * with the request.
 *
 * Constructor: Initialize this new RequestCtx with given values. 
 * The ids of the subject, action and resource should always be 
 * given. Optionally, extra attributes can be provided (which should NOT
 * contain the ids of the subject, action and resource again).
 */
class RequestCtx(val attributes: Map[Attribute[_], Any]) {
  
  def get(attribute: Attribute[_]) = attributes.get(attribute)
  
  val subjectId: Option[String] = attributes.get(Attributes.subjectId).asInstanceOf[Option[String]]

  val resourceId: Option[String] = attributes.get(Attributes.resourceId).asInstanceOf[Option[String]]

  val actionId: Option[String] = attributes.get(Attributes.actionId).asInstanceOf[Option[String]]
  
  
  import Attributes._
  override def toString(): String = s"${subjectId.getOrElse("???")}--${actionId.getOrElse("???")}->${resourceId.getOrElse("???")} + ${this.attributes}" 
  
}
