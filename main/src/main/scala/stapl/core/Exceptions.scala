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

class AttributeNotFoundException(evaluationId: String, entityId: String, attribute: Attribute[_]) extends RuntimeException(s"[Evaluation $evaluationId] $attribute wasn't found for entity $entityId.")

class AttributeDoesNotExistException(name: String) extends RuntimeException(s"No attribute with name '$name' has been defined.")

class RemotePolicyNotFoundException(policyId: String) extends RuntimeException(s"The remote policy with id $policyId wasn't found.")

class AttributeClashException(doubles: Set[Attribute[_]]) extends RuntimeException(s"These attributes are provided more than once: ${doubles.mkString("[", ", ", "]")}")