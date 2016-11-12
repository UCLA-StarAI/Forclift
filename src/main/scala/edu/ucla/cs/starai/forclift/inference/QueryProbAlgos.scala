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
import edu.ucla.cs.starai.forclift.util.LogDouble

trait QueryProbAlgorithm{
   
  def verbose: Boolean
  
  /**
   * Output query probability
   */
  def computeQueryProb(wcnf: WeightedCNF, query: PositiveUnitClause): SignLogDouble
  
}

class QueryProbExact(
    override val verbose: Boolean = false) 
    extends QueryProbAlgorithm {

  /**
   * Output query probability using exact first-order knowledge compilation
   */
  def computeQueryProb(wcnf: WeightedCNF, query: PositiveUnitClause): SignLogDouble = {

    println(s"Running first-order knowledge compilation")
        
    require(query.isGround, s"Query $query is not ground")
    
    val queryWcnf = wcnf.addConstraint(query)
    
    if(verbose){
      Timer{
        wcnf.smoothNnf
      }("Compilation of evidence circuit took "+_+" ms")
      Timer{
        queryWcnf.smoothNnf
      }("Compilation of query circuit took "+_+" ms")
      println("evidence nnf size = " + wcnf.nnf.size)
      println("evidence smooth nnf size = " + wcnf.smoothNnf.size)
      println("query nnf size = " + queryWcnf.nnf.size)
      println("query smooth nnf size = " + queryWcnf.smoothNnf.size)
    }
       
    val wmc = wcnf.logSmoothWmc
    println(s"Z = $wmc = ${wmc.toDouble}")
    
    val queryWmc = queryWcnf.logSmoothWmc
    println(s"Z(query) = $queryWmc = ${queryWmc.toDouble}")
    
    val prob = queryWmc / wmc
    println(s"P($query) = $prob = ${prob.toDouble}")
    
    prob
  }

}


class QueryProbC2D(
    override val verbose: Boolean = false) 
    extends QueryProbAlgorithm {

  /**
   * Output query probability using c2d (propositional)
   */
  def computeQueryProb(wcnf: WeightedCNF, query: PositiveUnitClause): SignLogDouble = {
    
    println(s"Running C2D (propositonal inference)")
      
    require(query.isGround, s"Query $query is not ground")
    
    val queryWcnf = wcnf.addConstraint(query)

    val wmc = wcnf.logPropWmc
    println(s"Z = $wmc = ${wmc.toDouble}")
    
    val queryWmc = queryWcnf.logPropWmc
    println(s"Z(query) = $queryWmc = ${queryWmc.toDouble}")
    
    val prob = queryWmc / wmc
    println(s"P($query) = $prob = ${prob.toDouble}")
    
    prob
  }

}
