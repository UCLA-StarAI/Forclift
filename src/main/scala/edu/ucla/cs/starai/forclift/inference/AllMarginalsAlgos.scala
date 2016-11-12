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

trait AllMarginalsAlgorithm{
   
  def verbose: Boolean
  
  /**
   * Output all marginals
   */
  def computeAllMarginals(theoryWmc: WeightedCNF)
  
}

class AllMarginalsExact(
    override val verbose: Boolean = false)
    extends AllMarginalsAlgorithm {

  /**
   * Output all marginals using exact first-order knowledge compilation
   */
  def computeAllMarginals(theoryWmc: WeightedCNF) {

    println(s"Running first-order knowledge compilation")
    
    // TODO: Fix allmarginals for subdomains
    //       Problem is that this is based on a constant which are associated
    //       with the rootdomain instead of subdomain
    require(!theoryWmc.cnf.domains.exists(_.isInstanceOf[SubDomain]),
      s"The all-marginals query does not support models that contain subdomains: \n ${theoryWmc}")

    val equip = new EquiprobableAtoms(theoryWmc, verbose)
    val compiler = theoryWmc.compilerBuilder(theoryWmc.domainSizes.toInts)

    // compute partition function
    val z = new CNFCircuit(compiler, theoryWmc.cnf);
    z.cacheWmc(theoryWmc.domainSizes, theoryWmc.predicateWeights)
    println("Partition function is " + z.cachedWmc)

    // compute individual marginals
    for (queryClass <- equip.coshatteredFgCAtoms) {
      //      println("Running query: "+queryClass.atom.toString())
      if (queryClass.atom.toString.startsWith("sef_") ||
        queryClass.atom.toString.startsWith("zef_")) {
        println(s"Ignoring auxiliary atom $queryClass")
      } else {
        val circuit = new MarginalCircuits(compiler, z, queryClass, theoryWmc.domainSizes)
        circuit.cacheQueryWmc(theoryWmc.predicateWeights)
        println("Probability for class of queries " + queryClass + " is " + circuit.marginal)
      }
    }
    println("done")
  }

}

class AllMarginalsRCR(
    override val verbose: Boolean = false) 
    extends AllMarginalsAlgorithm {
    

  /**
   * Output all marginals using relax, compensate, and recover
   */
  def computeAllMarginals(theoryWmc: WeightedCNF) {
      println(s"Running lifted relax, compensate, and recover")
      val rcr = new NoTruthRCR(theoryWmc, verbose = verbose)
      rcr.compensateFullRelaxationAndRecover()
      println("done")
    }

      
}
