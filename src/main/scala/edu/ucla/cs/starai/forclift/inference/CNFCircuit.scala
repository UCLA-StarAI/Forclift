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

import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.util.SignLogDouble
import edu.ucla.cs.starai.forclift.util.SignLogDouble._
import edu.ucla.cs.starai.forclift.nnf.visitors.SignLogDoubleWmc
import edu.ucla.cs.starai.forclift.nnf.visitors.WmcVisitor
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.nnf.NNFNode
import edu.ucla.cs.starai.forclift.CNF
import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.Constant
import edu.ucla.cs.starai.forclift.Clause
import edu.ucla.cs.starai.forclift.SubDomain

abstract class CachingCNFCircuit {

  def smoothNNF: NNFNode

  private[this] var cachedWmcOption: Option[SignLogDouble] = None

  def hasCachedWmc = cachedWmcOption.nonEmpty

  def cachedWmc = cachedWmcOption.get

  def clearCache { cachedWmcOption = None }

  def cacheWmc(domainSizes: DomainSizes, weights: PredicateWeights) = {
    require(cachedWmcOption.isEmpty)
    val wmcVisitor = WmcVisitor(weights)
    val wmc = wmcVisitor.wmc(smoothNNF,domainSizes,weights)
    cachedWmcOption = Some(wmc)
  }

}

class PrecompiledCNFCircuit(val smoothNNF: NNFNode) extends CachingCNFCircuit {

  def cnf = smoothNNF.cnf

}

class CNFCircuit(val compiler: Compiler, val cnf: CNF) extends CachingCNFCircuit {

  lazy val smoothNNF = compiler.compile(cnf).smoothWithPredicates(cnf.predicates)

}

class MarginalCircuits(
  compiler: Compiler, val Z: CNFCircuit, val queryClass: PositiveUnitClause, 
  domainSizes: DomainSizes //        representativeGroundQueryOption: Option[Atom] = None,
  //        queryCNFCircuitOption: Option[CNFCircuit] = None
  ) {

  println(s"Building marginal query class for $queryClass")
  
  require(!Z.cnf.domains.exists(_ .isInstanceOf[SubDomain]),
      s"Cannot create a marginal circuit for a CNF with subdomains: ${Z.cnf}")
  
  require(!queryClass.domains.exists(_ .isInstanceOf[SubDomain]),
      s"Cannot create a marginal circuit for a query class with subdomains: ${queryClass}")
      
  
  val representativeGroundQuery = {
    val query = queryClass.getGrounding(domainSizes)
    for ((d, c) <- query.predicate.domains.zip(query.args)) {
      if (!d.knownConstants.contains(c)) d.addConstant(c.asInstanceOf[Constant])
    }
    query
  }

  def randomGroundQuery: Atom = {
    val r = new scala.util.Random
    val groundings = queryClass.ground(domainSizes).toSeq
    r.shuffle(groundings).head.groundLiterals.head
  }

  val queryCNFCircuit = {
    new CNFCircuit(compiler, Z.cnf ++ CNF(Clause(List(representativeGroundQuery), Nil)))
  }

  def marginal = {
    var marginal = queryCNFCircuit.cachedWmc / Z.cachedWmc
    if (marginal > one) {
      assume(marginal.logToDouble < 0.0000000001)
      marginal = one
    }
    assume(!marginal.isNaN && !marginal.isInfinite, "marginal = " + marginal)
    marginal
  }

  def negMarginal = {
    val negMarginal = one - marginal
    assume(!negMarginal.isNaN && !negMarginal.isInfinite && negMarginal <= one, "negMarginal = " + negMarginal)
    negMarginal
  }

  def nbMarginals = queryClass.nbGroundings(domainSizes)

  def cacheQueryWmc(weights: PredicateWeights) {
    queryCNFCircuit.cacheWmc(domainSizes, weights)
    if (Z.cachedWmc < queryCNFCircuit.cachedWmc * 0.9999999999) {
      println(s"[Error] FOUND BAD WMC (${Z.cachedWmc} < ${queryCNFCircuit.cachedWmc}), trying ground WMC")
      // on error, run verification here
      throw new IllegalStateException
    }
    assume(Z.cachedWmc >= queryCNFCircuit.cachedWmc * 0.9999999999)
  }

  def clearQueryCache = queryCNFCircuit.clearCache

  def verifyExchangeability(domainSizes: DomainSizes, weights: PredicateWeights) {
    val query = randomGroundQuery
    for ((d, c) <- query.predicate.domains.zip(query.args)) {
      if (!d.knownConstants.contains(c)) d.addConstant(c.asInstanceOf[Constant])
    }
    val testCircuit = new CNFCircuit(compiler, Z.cnf ++ CNF(Clause(List(query), Nil)))
    testCircuit.cacheWmc(domainSizes, weights)
    val error = (testCircuit.cachedWmc / queryCNFCircuit.cachedWmc).abs
    if (error.logToDouble > 0.0000000001) {
      println("Verification BAD")
      throw new IllegalStateException
    } else {
      println("Verification OK on " + query + " vs " + representativeGroundQuery + " error is " + error)
    }
  }

}
