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

package edu.ucla.cs.starai.forclift.cli

import org.clapper.argot.ArgotConverters._
import org.clapper.argot.ArgotParser
import org.clapper.argot.FlagOption
import org.clapper.argot.SingleValueOption
import org.clapper.argot.SingleValueParameter
import edu.ucla.cs.starai.forclift.propositional.C2DError
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.languages.ModelConverters._
import edu.ucla.cs.starai.forclift.propositional.DimacsCNF
import edu.ucla.cs.starai.forclift.inference.WeightedCNF

/**
 * Handle all debugging logic for CLI
 */
class DebugCLI(argumentParser: ArgotParser) {
  
  /* DEBUGGING FLAGS */
    
  val helpFlag = argumentParser.flag[Any](
    List("h", "help"),
    "This help.") {
      (s, opt) => argumentParser.usage("")
    }

  //TODO change to using verbosity levels (int)
  val verboseFlag = argumentParser.flag[Boolean](
    List("v", "verbose"),
    "Verbose output (and verbose pdf).")
  def verbose = verboseFlag.value.getOrElse(false)

  val verifyWmcFlag = argumentParser.flag[Boolean](
    List("verify"),
    "Verify the result of wfomc using the UCLA c2d compiler. The c2d compiler command can be set with environment variable C2DCMD (default: ./c2d_linux).")
  def verifyWmc = verifyWmcFlag.value.getOrElse(false)
  
  val showNNFFlag = argumentParser.flag[Boolean](
    List("pdf"),
    "Create a pdf visualizing the NNF circuit. Requires pdflatex and graphviz dot to be in your path and the dot2texi package installed.")
  def showNNF = showNNFFlag.value.getOrElse(false)
    
  val showGroundingFlag = argumentParser.flag[Boolean](
    List("ground"),
    "Show a ground CNF for the model.")
  def showGrounding = showGroundingFlag.value.getOrElse(false)
  
  
  def runDebugging(inputCLI: InputCLI) {
    
    if (showGrounding) {
      println("Ground model:")
      println(inputCLI.model.ground)
      println
    }
    
    if (verifyWmc) {
      inputCLI.wcnfModel.verifyLogWmc
      if (inputCLI.hasQuery){
        val wcnfQuery = inputCLI.wcnfModel.addConstraint(inputCLI.query)
        wcnfQuery.verifyLogWmc
      }
    }
    
    if(showNNF){
      // set some default parameter that have no flags
      val compact = true;
      val maxDepth = Integer.MAX_VALUE
      inputCLI.wcnfModel.showNnfPdf(compact, maxDepth, "theory.nnf", verbose = verbose)
      inputCLI.wcnfModel.showSmoothNnfPdf(compact, maxDepth, "theory.smooth.nnf", verbose = verbose)
      if (inputCLI.hasQuery){
        val wcnfQuery = inputCLI.wcnfModel.addConstraint(inputCLI.query)
        wcnfQuery.showNnfPdf(compact, maxDepth, "query.nnf", verbose = verbose)
        wcnfQuery.showSmoothNnfPdf(compact, maxDepth, "query.smooth.nnf", verbose = verbose)
      }
    }
    
  }

  
}
