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

package edu.ucla.cs.starai.forclift.rcr

import scala.collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference._
import scala.util.Random._
import util.KLD._
import constraints._
import breeze.math._
import edu.ucla.cs.starai.forclift.util.SignLogDouble
import edu.ucla.cs.starai.forclift.util.SignLogDouble._

class Compensation(
  val eq: CoShatteredEquivalence,
  val origMarginal: MarginalCircuits,
  val copyMarginal: MarginalCircuits,
  var accumulatedTotalCompensationsKLD: Double) {

  def mapMarginals(
    mappedOrigMarginals: Map[MarginalCircuits, MarginalCircuits],
    mappedCopyMarginals: Map[MarginalCircuits, MarginalCircuits]) = {
    new Compensation(eq, mappedOrigMarginals(origMarginal), mappedCopyMarginals(copyMarginal), accumulatedTotalCompensationsKLD)
  }

  def dZdthetaCopy(weights: PredicateWeights) = {
    copyMarginal.marginal / weights(eq.thetaCopy).posW
  }

  def dZdthetaNegCopy(weights: PredicateWeights) = {
    copyMarginal.negMarginal / weights(eq.thetaCopy).negW
  }

  def dZdthetaOrig(weights: PredicateWeights) = {
    origMarginal.marginal / weights(eq.thetaOrig).posW
  }

  def dZdthetaNegOrig(weights: PredicateWeights) = {
    origMarginal.negMarginal / weights(eq.thetaOrig).negW
  }

  def logConverged(a: SignLogDouble, b: SignLogDouble, logPrecision: Double) = {
    val c = a-b
//    println(s"$a - $b = $c < $logPrecision")
    !c.pos || c.logToDouble < logPrecision // TODO: can become negative, correct?
  }

  def updateThetaCopy(weights: PredicateWeights, logDamping: (SignLogDouble, SignLogDouble), logPrecision: Double) = {
    val newLogW = dZdthetaOrig(weights)
    val newNegLogW = dZdthetaNegOrig(weights)
    val norm = newNegLogW + newLogW
    val normLogW = ((logDamping._1 * weights(eq.thetaCopy).posW) + (logDamping._2 * (newLogW / norm)))
    val normnegW = ((logDamping._1 * weights(eq.thetaCopy).negW) +
      (logDamping._2 * (newNegLogW / norm)))
    //    println("Changing " + math.exp(weights(eq.thetaCopy).logPosW) + " to " + math.exp(normLogW))
    //    println("Changing " + math.exp(weights(eq.thetaCopy).negW) + " to " + math.exp(normnegW))
    require(normLogW > zero)
    require(normnegW > zero)
    val converged = (logConverged(normLogW, weights(eq.thetaCopy).posW, logPrecision) ||
      logConverged(normnegW, weights(eq.thetaCopy).negW, logPrecision))
    (weights.update(eq.thetaCopy, WeightsFromLog(normLogW, normnegW)), converged)
  }

  def updateThetaOrig(weights: PredicateWeights, logDamping: (SignLogDouble, SignLogDouble), logPrecision: Double) = {
    val newLogW = dZdthetaCopy(weights)
    val newNegLogW = dZdthetaNegCopy(weights)
    val norm = (newNegLogW + newLogW)
    val normLogW = ((logDamping._1 * weights(eq.thetaOrig).posW) +
      (logDamping._2 * (newLogW / norm)))
    val normnegW = ((logDamping._1 * weights(eq.thetaOrig).negW) +
      (logDamping._2 * (newNegLogW / norm)))
    //    println("Changing " + math.exp(weights(eq.thetaOrig).logPosW) + " to " + math.exp(normLogW))
    //    println("Changing " + math.exp(weights(eq.thetaOrig).negW) + " to " + math.exp(normnegW))
    require(normLogW > zero)
    require(normnegW > zero)
    val converged = (logConverged(normLogW, weights(eq.thetaOrig).posW, logPrecision) ||
      logConverged(normnegW, weights(eq.thetaOrig).negW, logPrecision))
    //      if (verbose) println("Gradient for " + eq.thetaCopy + " = [" + math.exp(origLogGradient) + " , " + math.exp(negOrigLogGradient) + "]")
    (weights.update(eq.thetaOrig, WeightsFromLog(normLogW,normnegW)), converged)
  }

  def stepThetaParams(weights: PredicateWeights, logDamping: (SignLogDouble, SignLogDouble), logPrecision: Double) = {
    val (weights1, converged1) = updateThetaOrig(weights, logDamping, logPrecision)
    val (weights2, converged2) = updateThetaCopy(weights1, logDamping, logPrecision)
    (weights2, converged1 && converged2)
  }

  def singleCompensationKLD(weights: PredicateWeights) = {
    val trueParams = (weights(eq.thetaCopy).posW * weights(eq.thetaOrig).posW)
    val falseParams = (weights(eq.thetaCopy).negW * weights(eq.thetaOrig).negW)
    val norm = (trueParams + falseParams)
    val trueKLD = symmetricKld(origMarginal.marginal, copyMarginal.marginal, (trueParams / norm)).toDouble
    val falseKLD = symmetricKld(origMarginal.negMarginal, copyMarginal.negMarginal, (falseParams / norm)).toDouble
    val sum: Double = trueKLD + falseKLD
    //    // changed to absolute error
    //    def logdiff(a: Double,b:Double) = if(a>b) logminusexp(a,b) else logminusexp(b,a)
    //    import math.exp
    //    val sum = exp(logdiff(origMarginal.logMarginal,copyMarginal.logMarginal)) + exp(logdiff(origMarginal.logMarginal,logdivexp(trueParams, norm))) + exp(logdiff(copyMarginal.logMarginal,logdivexp(trueParams, norm)))
    require((!double2Double(sum).isNaN) && sum >= 0, s"Sum is not a valid number: $sum = $trueKLD + $falseKLD")
    sum
  }

  def addToAccumulatedTotalCompensationsKLD(weights: PredicateWeights) {
    accumulatedTotalCompensationsKLD += totalCompensationsKLD(weights)
  }

  def totalCompensationsKLD(weights: PredicateWeights) = {
    eq.nbGroundEquivalences * singleCompensationKLD(weights)
  }

  def partitionFunctionFactor(weights: PredicateWeights) = {
    // Z is multiplied with (w_x,wnotx) for each smoothed theta and with w_x*w_y + w_notx*w_noty
    val nbSmoothedThetas = eq.nbGroundThetas - eq.nbGroundEquivalences
    val smoothingFactor = (weights(eq.thetaOrig).negWPlusPosW * weights(eq.thetaCopy).negWPlusPosW)
    						.pow(nbSmoothedThetas)
    val compensationFactor = (
      ((weights(eq.thetaOrig).posW * weights(eq.thetaCopy).posW) +
        (weights(eq.thetaOrig).negW * weights(eq.thetaCopy).negW))
        .pow(eq.nbGroundEquivalences))
    (smoothingFactor * compensationFactor)
  }

  override def toString = eq.toString + " compensation"

}

class CoShatteredEquivalence(
  val copy: Atom, val orig: Atom,
  val constrs: Constraints,
  domainSizes: DomainSizes) {

  val copyConstrs = constrs.project(copy.variables)
  val origConstrs = constrs.project(orig.variables)

  val copyCAtom = (new PositiveUnitClause(copy, copyConstrs)).standardizeApart
  val origCAtom = (new PositiveUnitClause(orig, origConstrs)).standardizeApart

  val thetaCopy = copy.predicate.copy(name = Symbol("\\theta_{" + copyCAtom + "==" + origCAtom + "}"))
  val thetaOrig = copy.predicate.copy(name = Symbol("\\theta_{" + origCAtom + "==" + copyCAtom + "}"))

  val thetaCopyDef = new CNF(List(
    Clause(List(thetaCopy(copy.args: _*)), List(copy), copyConstrs).standardizeApart,
    Clause(List(copy), List(thetaCopy(copy.args: _*)), copyConstrs).standardizeApart))

  val thetaOrigDef = new CNF(List(
    // this needs the same arity as the copy theta, to make compesations equal 
    Clause(List(thetaOrig(copy.args: _*)), List(orig), copyConstrs).standardizeApart,
    Clause(List(orig), List(thetaOrig(copy.args: _*)), copyConstrs).standardizeApart))

  val thetaDefs = thetaCopyDef ++ thetaOrigDef

  def maintainsArity = copy.variables.size == orig.variables.size

  // don't use equivalence clauses, substitute copies instead for efficiency!!
  //  def equivalenceDef = new CNF(List(
  //    Clause(List(orig), List(copy), ineqConstrs, elemConstrs).standardizeApart,
  //    Clause(List(copy), List(orig), ineqConstrs, elemConstrs).standardizeApart))

  def substituteCopy(cnf: CNF): CNF = {
    val substClauses = cnf.clauses.map { clause =>
      def substituteAtom(atom: Atom): Atom = {
        if (atom.unifies(copy, copyConstrs, clause.constrs)) {
          val variableMapping = (copy.args zip atom.args).toMap
          orig.substitute { variableMapping(_) }
        } else atom
      }
      val substPosLits = clause.posLits.map { substituteAtom(_) }
      val substNegLits = clause.negLits.map { substituteAtom(_) }
      Clause(substPosLits, substNegLits, clause.constrs)
    }
    new CNF(substClauses)
  }

  def smoothingDef = new CNF(List(Clause(List(orig), List(orig), origConstrs).standardizeApart))

  // copy always has most of the groundings
  val nbGroundEquivalences = copyCAtom.nbGroundings(domainSizes)

  def nbGroundOrigAtoms = origCAtom.nbGroundings(domainSizes)

  def nbGroundThetas = {
    assume(thetaCopy.toAtom.nbGroundings(domainSizes) == thetaOrig.toAtom.nbGroundings(domainSizes))
    thetaCopy.toAtom.nbGroundings(domainSizes)
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val literalStr = copy.toString(nameSpace) + " == " + orig.toString(nameSpace)
    val constrStr = constrs.toString(nameSpace)
    List(literalStr, constrStr).filter { _.nonEmpty }.mkString(", ")
  }

}
