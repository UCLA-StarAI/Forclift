/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
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

package liftedinference.cli

import java.io._
import java.lang.System._
import scala.collection.JavaConverters._
import scala.io._
import org.clapper.argot._
import liftedinference._
import liftedinference.inference._
import liftedinference.languages._
import liftedinference.learning.structure.StructureLearner
import liftedinference.util.ExternalBinaries
import liftedinference.languages.mln._
import liftedinference.learning.Likelihood
import liftedinference.languages.focnf._

object WFOMC extends App {

  assertFalse()

  val argumentParser = new ArgotParser("wfomc", false, 80,
    Some("Version 3.0"),
    Some("EXAMPLE\n\njava -jar ./wfomc.jar -q \"smokes(Guy)\" ./models/friendsmoker.mln\njava -jar ./wfomc.jar -q \"smokes(Guy)\" ./models/friendsmoker.mln"), true)

  val debugCLI = new DebugCLI(argumentParser)
  val inputCLI = new InputCLI(argumentParser,debugCLI)
  val inferenceCLI = new InferenceCLI(argumentParser,debugCLI,inputCLI)
  val learningCLI = new LearningCLI(argumentParser,debugCLI,inputCLI)
  val outputCLI = new OutputCLI(argumentParser,debugCLI,inputCLI)

  /* PARSE FLAGS AND HANDLE LOGIC */

  try {
    argumentParser.parse(args)
    
    debugCLI.runDebugging(inputCLI)
    inferenceCLI.runInference()
    learningCLI.runLearning()
    outputCLI.runOutput()
    
  } catch {
    case e: ArgotUsageException =>
      println(e.message)
      System.exit(1)
  }

  def assertFalse() = assert(false, "Assertions are enabled in CLI: check compiler flags")

}
