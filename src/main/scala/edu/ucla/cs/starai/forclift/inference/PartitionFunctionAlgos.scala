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

package edu.ucla.cs.starai.forclift.inference

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.rcr.NoTruthRCR
import edu.ucla.cs.starai.forclift.util.Timer
import edu.ucla.cs.starai.forclift.util.SignLogDouble

trait PartitionFunctionAlgorithm{
   
  def verbose: Boolean
  
  /**
   * Output partition function
   */
  def computePartitionFunction(wcnf: WeightedCNF): SignLogDouble
  
}

class PartitionFunctionExact(
    override val verbose: Boolean = false) 
    extends PartitionFunctionAlgorithm {

  /**
   * Output partition function using exact first-order knowledge compilation
   */
  def computePartitionFunction(wcnf: WeightedCNF): SignLogDouble = {

    println(s"Running first-order knowledge compilation")
    
    if(verbose){
      Timer{
        wcnf.smoothNnf
      }("Compilation took "+_+" ms")
      println("evidence nnf size = " + wcnf.nnf.size)
      println("evidence smooth nnf size = " + wcnf.smoothNnf.size)
    }
       
    val wmc = wcnf.logSmoothWmc
    println(s"Z = $wmc = ${wmc.toDouble}")
      
    wmc
  }

}


class PartitionFunctionC2D(
    override val verbose: Boolean = false) 
    extends PartitionFunctionAlgorithm {

  /**
   * Output partition function using c2d (propositional)
   */
  def computePartitionFunction(wcnf: WeightedCNF): SignLogDouble = {

      println(s"Running C2D (propositonal inference)")
      val wmc = wcnf.logSmoothPropWmc
      println(s"Z = $wmc = ${wmc.toDouble}")
      
      wmc
  }

}
