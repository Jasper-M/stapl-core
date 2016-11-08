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

package stapl.core.parser

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import stapl.core.AbstractPolicy

/**
 * A class for parsing policies dynamically from strings.
 * This parser employs the Scala interpreter.
 */
class PolicyParser {
  val settings = new Settings
  //settings.usejavacp.value = true
  settings.nowarnings.value = true
  settings.embeddedDefaults[PolicyParser]
  val interpreter = new IMain(settings)
  

  private object lock
  
  /**
   * Parse the given policy string and return the resulting policy.
   * 
   * @param		policyString: String
   * 			The string containing the STAPL policy. This policy should not
   *    		contain attribute definitions and should just contain the policy
   *      		specification, not an assignment to a val. 
   *        	For example: policyString == "Policy(...) := ..."
   * 
   * @throws 	RuntimeException	
   * 			When the parser did not success in getting 
   * 			an abstract policy from the given string.
   */
  def parse(policyString: String): AbstractPolicy = {
    val somePolicy = lock.synchronized {
      interpreter.beQuietDuring({
        interpreter.interpret(policyString)
      })
  
      val somePolicy = interpreter.valueOfTerm(interpreter.mostRecentVar)
      interpreter.reset()
      somePolicy
    }

    somePolicy match {
      case None => throw new RuntimeException("Could not load policy from given policyString (returned None)")
      case Some(policy: AbstractPolicy) => policy
      case _ => throw new RuntimeException("Could not load policy from given policyString (result was not an AbstractPolicy)")
    }
  }
  
  /**
   * Parse the contents of the given file as a policy string and return 
   * the resulting policy.
   * 
   * More details: see parse(policyString: String)
   */
  def parseFile(path: String): AbstractPolicy = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    parse(policyString)
  }
  
  def parseFile(path: java.io.File): AbstractPolicy = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    parse(policyString)
  }
}
