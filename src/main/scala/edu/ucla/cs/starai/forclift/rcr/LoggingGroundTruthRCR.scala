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
import edu.ucla.cs.starai.forclift.compiler._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference._
import scala.util.Random._
import util.KLD._
import java.io._

class LoggingGroundTruthRCR(
  weightedCNF: WeightedCNF,
  logsDir: File = new File("experiments/rcr/exp?"),
  rcrCompilerBuilder: Compiler.Builder = Compiler.Builder.default,
  exactCompilerBuilder: Compiler.Builder = Compiler.Builder.defaultWithGrounding,
  verbose: Boolean = true) extends GroundTruthRCR(weightedCNF, rcrCompilerBuilder, exactCompilerBuilder, verbose) {

  //  // call to avoid messing up timings ????????
  //  val dummy = exactMarginalCircuits

  // log for console
  lazy val consoleLog: PrintWriter = new PrintWriter(new FileWriter(new File(logsDir, "console.log")));

  def closeConsoleLog() {
    if (consoleLog != null) consoleLog.close()
  }

  override def println(str: Object) {
    super.println(str)
    consoleLog.println(str)
    consoleLog.flush
  }

  override def print(str: Object) {
    super.print(str)
    consoleLog.print(str)
    consoleLog.flush
  }

  var start: Double = 0;

  override def onFullRelaxationStart() {
    start = System.currentTimeMillis()
  }

  def pad(s: Any) = s.toString.padTo(20, " ").mkString

  def closeAll() {
    closeAllCompensationIterationsLog()
    closeApproximationLog()
    closeConsoleLog()
  }

  // log for all compensation iterations
  var allCompensationIterationsLog: PrintWriter = null
  var nbAllCompensationIterationPoints = 0

  def closeAllCompensationIterationsLog() {
    if (allCompensationIterationsLog != null) allCompensationIterationsLog.close()
  }

  // log for converged marginals
  var approximationLog: PrintWriter = null

  def closeApproximationLog() {
    if (approximationLog != null) approximationLog.close()
  }

  var nbTotalEquivalences = -1

  override def onCompensateRecoverStart(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {

    nbTotalEquivalences = compensations.map { _.eq.nbGroundEquivalences.toInt }.sum

    allCompensationIterationsLog = new PrintWriter(new FileWriter(new File(logsDir, "compensations.dat")));
    allCompensationIterationsLog.println(
      pad("ITERATION") +
        pad("TIME") +
        pad("APPROX-KLD") +
        pad("SINGLE-COMP-KLD") +
        pad("TOTAL-COMP-KLD") +
        pad("SINGLE-RELAXATIONS") +
        pad("TOTAL-RELAXATIONS") +
        pad("%-RELAXATIONS"))
    allCompensationIterationsLog.flush()

    approximationLog = new PrintWriter(new FileWriter(new File(logsDir, "approximations.dat")));
    approximationLog.println(
      pad("TIME") +
        pad("RELAXATIONS") +
        pad("%RELAXATIONS") +
        pad("COMPENSATION-KLD") +
        pad("APPROX-KLD") +
        pad("%BP-APPROX-KLD"))
    approximationLog.flush()

  }

  //      //log for one compensation iteration
  //      var compensationIterationLog: PrintWriter = null
  //      var nbCompensationIterations = 0
  //
  //      def closeCompensationIterationLog() {
  //        if (compensationIterationLog != null) compensationIterationLog.close()
  //      }

  //      override def onStartCompensation(
  //        initialWeightFunction: PredicateWeights,
  //        compensations: List[Compensation],
  //        marginalCircuitsSet: MarginalCircuitsSet) {
  //        closeCompensationIterationLog()
  //        nbCompensationIterations += 1
  //        compensationIterationLog = new PrintWriter(new FileWriter("experiments/rcr/compensations" + nbCompensationIterations + ".dat"));
  //        compensationIterationLog.println(pad("ITERATION") + pad("APPROX-KLD") + pad("SINGLE-COMP-KLD") + pad("TOTAL-COMP-KLD"))
  //        compensationIterationLog.flush()
  //      }

  override def onCompensationIteration(
    i: Int,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet,
    weights: PredicateWeights) {
    // this compensation iteration log
    val approxKLD = symmetricKLDFromTruth(marginalCircuitsSet.origMarginals)
    val sCompKLDs = compensations.map { _.singleCompensationKLD(weights) }
    val tCompKLDs = compensations.map { _.totalCompensationsKLD(weights) }
    //        compensationIterationLog.println(pad(i) + pad(approxKLDs.sum.max(logPrecision).toFloat) + pad(sCompKLDs.sum.max(logPrecision).toFloat) + pad(tCompKLDs.sum.max(logPrecision).toFloat))
    //        compensationIterationLog.flush()
    // all compensations log
    nbAllCompensationIterationPoints += 1
    val bnTotalRelaxations = compensations.map { _.eq.nbGroundEquivalences }.sum
    allCompensationIterationsLog.println(
      pad(nbAllCompensationIterationPoints) +
        pad((System.currentTimeMillis() - start) / 1000F) +
        pad(approxKLD.toFloat) +
        pad(sCompKLDs.sum.toFloat) +
        pad(tCompKLDs.sum.toFloat) +
        pad(compensations.size) +
        pad(bnTotalRelaxations))
    allCompensationIterationsLog.flush()
  }

  var bpApprox = Double.NegativeInfinity
  override def onEndCompensation(
    weights: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {
    val approxKLD = symmetricKLDFromTruth(marginalCircuitsSet.origMarginals)
    if (bpApprox == Double.NegativeInfinity) bpApprox = approxKLD
    val tCompKLDs = compensations.map { _.totalCompensationsKLD(weights) }
    val bnTotalRelaxations = compensations.map { _.eq.nbGroundEquivalences }.sum
    approximationLog.println(
      pad((System.currentTimeMillis() - start) / 1000F) +
        pad(bnTotalRelaxations) +
        pad(bnTotalRelaxations * 100.0 / nbTotalEquivalences) +
        pad(tCompKLDs.sum.toFloat) +
        pad(approxKLD.toFloat) +
        pad((approxKLD / bpApprox * 100).toFloat))
    approximationLog.flush()
  }

}
