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

package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.SignLogDouble
import edu.ucla.cs.starai.forclift.util.Binomial
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.propositional.C2DError
import edu.ucla.cs.starai.forclift.util.LogDouble
import edu.ucla.cs.starai.forclift.util.SoftMemCache
import edu.ucla.cs.starai.forclift.Domain

trait WmcVisitor {
  def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble
}

object WmcVisitor {

  def apply(predicateWeights: PredicateWeights): WmcVisitor = {
    val hasNegativeWeight = predicateWeights.values.exists(w => w.negW < 0 || w.posW < 0)
    if (hasNegativeWeight) {
      //new SignLogDoubleWmc
      new CachingSignLogDoubleWmc
    } else {
      //new LogDoubleWmc
      new CachingLogDoubleWmc
    }
  }

}

protected class LogDoubleWmc extends NnfVisitor[(DomainSizes, PredicateWeights), LogDouble] with WmcVisitor {

  import edu.ucla.cs.starai.forclift.util.LogDouble._

  def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble = {
    visit(nnf, (domainSizes, predicateWeights))
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild, params)
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.child, params)
      groundChildWmc.pow(maxSize) * childchildWmc.pow((maxSize * (maxSize - 1)) / 2)
    }
  }

  protected def visitExists(exists: CountingNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement, (maxSize - nbTrue)))
      val newParams = (newDomainSizes, predicateWeights)
      val childWeight = visit(exists.child, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
    }
    logWeight
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val plus1lwmc = visit(ie.plus1, params)
    val plus2lwmc = visit(ie.plus2, params)
    val minlwmc = visit(ie.min, params)
    plus1lwmc + plus2lwmc - minlwmc
  }

  protected def visitOrNode(or: Or, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val llwmcc = visit(or.l, params)
    val rlwmcc = visit(or.r, params)
    llwmcc + rlwmcc
  }

  protected def visitAndNode(and: And, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val llwmcc = visit(and.l, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r, params)
      llwmcc * rlwmcc
    }
  }

  protected def visitRefNode(ref: Ref, params: (DomainSizes, PredicateWeights)): LogDouble = {
    visit(ref.nnfNode, params)
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    weights.negWPlusPosWLogDouble.pow(nbGroundings)
  }

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(leaf: UnitLeaf, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) one
    else if (leaf.positive) weights.posWLogDouble.pow(nbGroundings)
    else weights.negWLogDouble.pow(nbGroundings)
  }

  protected def visitGroundingNode(leaf: GroundingNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc.toLogDouble
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitFalse(params: (DomainSizes, PredicateWeights)): LogDouble = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights)): LogDouble = one

}


object NnfVisitorCache {
  
  var hit = 0;
  var miss = 0;
  
  @inline def addHit(){}
  @inline def addMiss(){}
  @inline def info(){}
  
//  @inline def addHit(){ hit += 1; info}
//  @inline def addMiss(){ miss += 1; info}
//  @inline def info(){
//    if((hit+miss)%10000 == 0 ){
//      println(s"hits: $hit, miss: $miss, hitRate: ${hit*1.0/(hit+miss)}")
//    }
//  }
  
  class Key(val node: NNFNode, val domainSizes: IndexedSeq[Int]) {

    override def equals(that: Any) = {
      if (that == null || !that.isInstanceOf[Key]) false
      else {
        val thatKey = that.asInstanceOf[Key]
        this.node == thatKey.node && this.domainSizes == thatKey.domainSizes
      }
    }

    override val hashCode = (node, domainSizes).hashCode

  }
  
} 

protected class CachingLogDoubleWmc extends LogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.LogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, LogDouble]

  override def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble = {
    cache.clear()
    super.wmc(nnf, domainSizes, predicateWeights)
  }
  
  // only decomposition nodes can reduce the number of relevant domains!

  override protected def visitAndNode(and: And, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val llwmcc = retrieveWmc(and.l, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r, params)
      llwmcc * rlwmcc
    }
  }

  override protected def visitForallNode(forall: IndependentPartialGroundingNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = retrieveWmc(forall.child, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  @inline private def retrieveWmc(node: NNFNode, params: (DomainSizes, PredicateWeights)): LogDouble = {
    if(node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1){
      return visit(node, params)
    }else {
	    val domains: IndexedSeq[Domain] = node.orderedDomains
	    val (domainSizes, predicateWeights) = params
	    val key = new Key(node, domains.map(domainSizes(_).size))
	    val result = cache.get(key)
	    if (result.nonEmpty) {
	      addHit()
	      result.get
	    }else {
	      addMiss()
	      val childWeight = visit(node, params)
	      cache.update(key, childWeight)
	      childWeight
	    }
    }
  }

}

protected class SignLogDoubleWmc extends NnfVisitor[(DomainSizes, PredicateWeights), SignLogDouble] with WmcVisitor {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._

  def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble = {
    visit(nnf, (domainSizes, predicateWeights))
  }

  //  // add to debug NaN evaluation
  //  override def visit(node: NNFNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
  //    val wmc = super.visit(node, params)
  //    require(!wmc.isNaN)
  //    wmc
  //  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.child, params)
      groundChildWmc.pow(maxSize) * childchildWmc.pow((maxSize * (maxSize - 1)) / 2)
    }
  }

  protected def visitExists(exists: CountingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement, (maxSize - nbTrue)));
      val newParams = (newDomainSizes, predicateWeights)
      val childWeight = visit(exists.child, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue).toSignDouble
      logWeight += binomialCoeff * childWeight
    }
    logWeight
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val plus1lwmc = visit(ie.plus1, params)
    val plus2lwmc = visit(ie.plus2, params)
    val minlwmc = visit(ie.min, params)
    plus1lwmc + plus2lwmc - minlwmc
  }

  protected def visitOrNode(or: Or, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val llwmcc = visit(or.l, params)
    val rlwmcc = visit(or.r, params)
    llwmcc + rlwmcc
  }

  protected def visitAndNode(and: And, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val llwmcc = visit(and.l, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r, params)
      llwmcc * rlwmcc
    }
  }

  protected def visitRefNode(ref: Ref, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    visit(ref.nnfNode, params)
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    weights.negWPlusPosW.pow(nbGroundings)
  }

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(leaf: UnitLeaf, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) one
    else if (leaf.positive) weights.posW.pow(nbGroundings)
    else weights.negW.pow(nbGroundings)
  }

  protected def visitGroundingNode(leaf: GroundingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitFalse(params: (DomainSizes, PredicateWeights)): SignLogDouble = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights)): SignLogDouble = one

}


protected class CachingSignLogDoubleWmc extends SignLogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, SignLogDouble]

  override def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble = {
    cache.clear()
    super.wmc(nnf, domainSizes, predicateWeights)
  }

  // only decomposition nodes can reduce the number of relevant domains!
  
  override protected def visitAndNode(and: And, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val llwmcc = retrieveWmc(and.l, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r, params)
      llwmcc * rlwmcc
    }
  }

  override protected def visitForallNode(forall: IndependentPartialGroundingNode, 
      params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = retrieveWmc(forall.child, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  @inline private def retrieveWmc(node: NNFNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    if(node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1)
      return visit(node, params)
    } else{
	    val domains: IndexedSeq[Domain] = node.orderedDomains
	    val (domainSizes, predicateWeights) = params
	    val key = new Key(node, domains.map(domainSizes(_).size))
	    val result = cache.get(key)
	    if (result.nonEmpty) {
	      addHit()
	      result.get
	    }else {
	      addMiss()
	      val childWeight = visit(node, params)
	      cache.update(key, childWeight)
	      childWeight
	    }
    }
  }

}

class SafeSignLogDoubleWmc extends SignLogDoubleWmc {

  import SignLogDouble._

  override protected def visitForallNode(forall: IndependentPartialGroundingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(forall.d)) {
      super.visitForallNode(forall, params)
    } else NaN
  }

  override protected def visitExists(exists: CountingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(exists.domain)) {
      super.visitExists(exists, params)
    } else NaN
  }

  override protected def visitDomainRecursion(dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights)) = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(dr.domain)) {
      super.visitDomainRecursion(dr, params)
    } else NaN
  }

  override protected def visitSmoothingNode(leaf: SmoothingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitSmoothingNode(leaf, params)
    } else NaN
  }

  override protected def visitUnitLeaf(leaf: UnitLeaf, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitUnitLeaf(leaf, params)
    } else NaN
  }

  override protected def visitContradictionLeaf(leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitContradictionLeaf(leaf, params)
    } else NaN
  }

  override protected def visitGroundingNode(leaf: GroundingNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.cnf.domains.forall { domainSizes.contains(_) }) {
      super.visitGroundingNode(leaf, params)
    } else NaN
  }

}

/**
 * Compute WMC while also verifying the counts of every intermediate step using C2D
 */

object VerifyWmcVisitor {

  class VerificationFailedException extends Exception

  def verify(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights) {
    (new VerifyWmcVisitor()).visit(nnf, (domainSizes, predicateWeights))
  }

}

protected class VerifyWmcVisitor extends SignLogDoubleWmc {
  
  val nonVerifyingWmcVisitor = new SignLogDoubleWmc

  override def visit(nnfNode: NNFNode, params: (DomainSizes, PredicateWeights)): SignLogDouble = {
    val wmc = super.visit(nnfNode, params)
    verifyLocal(nnfNode, params)
    wmc
  }

  protected def verifyLocal(nnfNode: NNFNode, params: (DomainSizes, PredicateWeights)) {
    val (domainSizes, predicateWeights) = params
    try {
      val cnf = nnfNode.cnf
      val weightedCNF = WeightedCNF(cnf, domainSizes, predicateWeights)
      // ground truth (pun intended)
      val groundCNF = cnf.ground(domainSizes)
      // smooth with everything appearing in the ground cnf, not with every predicate grounding!!
      val atomsInPropWmc = groundCNF.atoms.map { new PositiveUnitClause(_, Constraints.empty) }
      val (thisSmooth, countedAtoms) = nnfNode.smooth
      // not used -- debugging purposes only
      val onceSmoothedLiftedLogWmc = nonVerifyingWmcVisitor.visit(thisSmooth, params)
      // must also ground before subtracting, otherwise constants will unify with empty domain variables
      val atomsInLiftedWmc = countedAtoms.flatMap { _.ground(domainSizes).map { _.toPositiveUnitClause } }
      val atomsMissingFromLiftedWmc = atomsInPropWmc.flatMap { _.minus(atomsInLiftedWmc) }
      val atomsMissingFromPropWmc = atomsInLiftedWmc.flatMap { _.minus(atomsInPropWmc) }

      val thisTwiceSmoothed = thisSmooth.smoothWith(atomsMissingFromLiftedWmc)
      val twiceSmoothedLiftedLogWmc = nonVerifyingWmcVisitor.visit(thisTwiceSmoothed, params)

      val weightsMissingFromPropWmc = atomsMissingFromPropWmc.toList.map { clause =>
        nonVerifyingWmcVisitor.visit((new SmoothingNode(clause)), params)
      }
      val propLogWmc = weightedCNF.logPropWmc
      val twiceSmoothedPropLogWmc = {
        weightsMissingFromPropWmc.foldLeft(propLogWmc) { _ * _ }
      }

      val correct = ((twiceSmoothedPropLogWmc == twiceSmoothedLiftedLogWmc)
        || twiceSmoothedPropLogWmc.isZero && twiceSmoothedLiftedLogWmc.isZero
        || (twiceSmoothedPropLogWmc.logToDouble - twiceSmoothedLiftedLogWmc.logToDouble).abs < 0.0000001
        || (twiceSmoothedPropLogWmc - twiceSmoothedLiftedLogWmc).abs.logToDouble < 0.0000001)
      if (!correct) {
        println("WMC:")
        println(weightedCNF)
        println("Ground CNF:")
        println(weightedCNF.groundCnf)
        println("correct wmc = " + twiceSmoothedPropLogWmc.exp)
        println("our wmc = " + twiceSmoothedLiftedLogWmc.exp)
        println("test wmc = " + onceSmoothedLiftedLogWmc.exp)
        println("debug")
        thisTwiceSmoothed.showPDF(domainSizes, predicateWeights, false, file = "bug.nnf")
        throw new VerifyWmcVisitor.VerificationFailedException
      }

    } catch {
      case c2d: C2DError => {
        // C2D refuses: can't verify this node.
      }
    }
  }

}
 

/**
 * Goes out of memory
 */
protected class BigIntWmc(val decimalPrecision: Int = 100) extends NnfVisitor[(DomainSizes, PredicateWeights), BigInt] with WmcVisitor {
  
  val zero: BigInt = 0
  val one: BigInt = 1
  
  def wmc(nnf: NNFNode, domainSizes: DomainSizes, predicateWeights: PredicateWeights): SignLogDouble = {
    val normalization: LogDouble = LogDouble.doubleToLogDouble(decimalPrecision).pow(numRandVars(domainSizes,predicateWeights))
    bigInt2SignLogDouble(visit(nnf, (domainSizes, predicateWeights)))/normalization
  }
  
  def numRandVars(domainSizes: DomainSizes, predicateWeights: PredicateWeights): Int = {
    predicateWeights.predicates.map { _.toAtom.nbGroundings(domainSizes) }.sum
  }
  
  val LOG2 = Math.log(2.0);

  // see http://stackoverflow.com/questions/6827516/logarithm-for-BigInt
  def bigInt2SignLogDouble(bigint: BigInt): SignLogDouble = {
      var v = bigint
      val blex = v.bitLength - 1022; // any value in 60..1023 is ok
      if (blex > 0)
          v = v >> blex;
      val res = Math.log(v.doubleValue());
      return SignLogDouble.fromLog(if(blex > 0) res + blex * LOG2 else res);
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.child, params)
      groundChildWmc.pow(maxSize) * childchildWmc.pow((maxSize * (maxSize - 1)) / 2)
    }
  }

  protected def visitExists(exists: CountingNode, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement, (maxSize - nbTrue)));
      val newParams = (newDomainSizes, predicateWeights)
      val childWeight = visit(exists.child, newParams)
      val binomialCoeff = coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
    }
    logWeight
  }
  
  // special cache for bigint factorials (cf. Binomial class)
  private[this] val factorialCache = new collection.mutable.ArrayBuffer[BigInt] ++ List(one, one)

  def factorial(n: Int): BigInt = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): BigInt = factorial(n) / factorial(k) / factorial(n - k)


  protected def visitForallNode(forall: IndependentPartialGroundingNode, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, params: (DomainSizes, PredicateWeights)): BigInt = {
    val plus1lwmc = visit(ie.plus1, params)
    val plus2lwmc = visit(ie.plus2, params)
    val minlwmc = visit(ie.min, params)
    plus1lwmc + plus2lwmc - minlwmc
  }

  protected def visitOrNode(or: Or, params: (DomainSizes, PredicateWeights)): BigInt = {
    val llwmcc = visit(or.l, params)
    val rlwmcc = visit(or.r, params)
    llwmcc + rlwmcc
  }

  protected def visitAndNode(and: And, params: (DomainSizes, PredicateWeights)): BigInt = {
    val llwmcc = visit(and.l, params)
    if (llwmcc == zero) zero
    else {
      val rlwmcc = visit(and.r, params)
      llwmcc * rlwmcc
    }
  }

  protected def visitRefNode(ref: Ref, params: (DomainSizes, PredicateWeights)): BigInt = {
    visit(ref.nnfNode, params)
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    BigInt((decimalPrecision*weights.negWPlusPosWDouble).toInt).pow(nbGroundings)
  }

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(leaf: UnitLeaf, params: (DomainSizes, PredicateWeights)): BigInt = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) one
    else if (leaf.positive) BigInt((decimalPrecision*weights.posWDouble).toInt).pow(nbGroundings)
    else BigInt((decimalPrecision*weights.negWDouble).toInt).pow(nbGroundings)
  }

  protected def visitGroundingNode(leaf: GroundingNode, params: (DomainSizes, PredicateWeights)): BigInt = {
    throw new UnsupportedOperationException
  }
  

  protected def visitFalse(params: (DomainSizes, PredicateWeights)): BigInt = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights)): BigInt = one

}
