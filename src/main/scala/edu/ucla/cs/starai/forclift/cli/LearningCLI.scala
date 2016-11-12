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

import java.io.File
import org.clapper.argot.ArgotParser
import org.clapper.argot.ArgotConverters._
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.learning.structure.StructureLearner
import edu.ucla.cs.starai.forclift.learning.WeightLearning
import edu.ucla.cs.starai.forclift.learning.Likelihood
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import java.io.FileWriter
import scala.io.Source
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import scala.collection.JavaConverters._
import edu.ucla.cs.starai.forclift.languages.FileFormat

/**
 * Handle all learning logic for CLI
 */
class LearningCLI(
    argumentParser: ArgotParser,
    debugCLI: DebugCLI,
    inputCLI: InputCLI) {

  /* LEARNING FLAGS */

  //TODO cleanup flags to simplify CLI

  val doWeightLearningFlag = argumentParser.flag[Boolean](
    List("wl"),
    "Learn weights (only for MLN).")
  def doWeightLearning = doWeightLearningFlag.value.getOrElse(false)

  val doStructureLearningFlag = argumentParser.flag[Boolean](
    List("sl"),
    "Learn structure and weights (only for MLN).")
  def doStructureLearning = doStructureLearningFlag.value.getOrElse(false)

  val doDBLikelihoodFlag = argumentParser.flag[Boolean](
    List("ll"),
    "Database likelihood (only for MLN).")
  def doDBLikelihood = doDBLikelihoodFlag.value.getOrElse(false)

  val doDBPseudoLikelihoodFlag = argumentParser.flag[Boolean](
    List("pll"),
    "Database pseudo likelihood (only for MLN and performed ground, only for verification purposes).");
  def doDBPseudoLikelihood = doDBPseudoLikelihoodFlag.value.getOrElse(false)

  val doNormalizeLHFlag = argumentParser.flag[Boolean](
    List("normalizell"),
    "Normalize likelihood and pseudo-likelihood.")
  def doNormalizeLH = doNormalizeLHFlag.value.getOrElse(false)

  val slOutputDirFlag = argumentParser.option[File](
    List("sl-output"),
    "filename",
    "Output directory for structure learning files. The default directory is /tmp.") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def slOutputDir = slOutputDirFlag.value.getOrElse(new File(System.getProperty("java.io.tmpdir")))

  val slComplexityPenaltyFlag = argumentParser.option[Double](
    List("sl-complexity"),
    "double",
    "Complexity penalty for structure learning. The default penalty is 0.01.")
  def slComplexityPenalty = slComplexityPenaltyFlag.value.getOrElse(0.01)

  val slTimeoutFlag = argumentParser.option[Int](
    List("sl-timeout"),
    "integer",
    "Time-out for structure learning. The default time-out is 900 seconds.")
  def slTimeout = slTimeoutFlag.value.getOrElse(900)

  val slThreadsFlag = argumentParser.option[Int](
    List("sl-threads"),
    "integer",
    "Number of additional threads for structure learning. The default is 1 thread.")
  def slThreads = slThreadsFlag.value.getOrElse(1)

  val slStrategyFlag = argumentParser.option[Int](
    List("sl-strategy"),
    "integer",
    "0 = greedy search; 1 = stepped search.")
  def slStrategy = slStrategyFlag.value.getOrElse(0)

  val slClausesFlag = argumentParser.option[Int](
    List("sl-clauses"),
    "integer",
    "Maximum number of clauses in the learned theory for structure learning. The default maximum is 15 clauses.")
  def slClauses = slClausesFlag.value.getOrElse(15)

  val slNormalizeFlag = argumentParser.flag[Boolean](
    List("normalizesl"),
    "Normalize likelihood and circuit sizes in objective function.")
  def slNormalize = slNormalizeFlag.value.getOrElse(false)

  val slStepSizeFlag = argumentParser.option[Double](
    List("sl-stepsize"),
    "double",
    "Step size for the stepped search strategy. The default step size is 0.01.")
  def slStepSize = slStepSizeFlag.value.getOrElse(0.01)

  def runLearning() {
    if (inputCLI.inputFileFormat != FileFormat.MLN) {
      argumentParser.usage("Learning only supports MLN input structures.")
    }
    if (doWeightLearning && doStructureLearning) {
      argumentParser.usage("Specifying --wl and --sl simultaneously is not allowed.")
    } else if (doWeightLearning) {
      runWeightLearning()
    } else if (doStructureLearning) {
      runStructureLearning()
    }
    if (doDBLikelihood) runLikelihoodEvaluation()
    if (doDBPseudoLikelihood) runPseudoLikelihoodEvaluation()
  }

  def runWeightLearning() {
    if (inputCLI.trainDbFiles.isEmpty) {
      argumentParser.usage("No training database files given for weight learning.")
    }
    val learnedMLN = WeightLearning.learnWeights(
      inputCLI.modelStructure.asInstanceOf[MLN],
      inputCLI.trainDbMlns,
      verbose = debugCLI.verbose,
      normalizeLH = doNormalizeLH,
      testdbMlns = inputCLI.testDbMlns,
      doLL = doDBLikelihood,
      doPLL = doDBPseudoLikelihood,
      skolemize = true) //TODO what does it even mean to turn off Skolemization for learning?

    // Write learned MLN to file
    val learnedMLNstr = learnedMLN.toStringFull
    val inputFilePath = inputCLI.inputFile.getAbsolutePath()
    val prefix = inputFilePath.substring(0, inputFilePath.length - inputCLI.inputFileExtension.length - 1)
    val learnedfile = new File(prefix + "_learned." + inputCLI.inputFileExtension)

    val out = new FileWriter(learnedfile)
    out.write(learnedMLNstr)
    println("Wrote learned model to " + learnedfile)
    out.close
  }

  def runStructureLearning() {
    if (inputCLI.trainDbMlns.isEmpty) {
      argumentParser.usage("No training database files given for structure learning.")
    }
    val mlnCandidateClauses = inputCLI.modelStructure.asInstanceOf[MLN]
    // structure learning requires unparsed files as input :-(
    val structureLearner = new StructureLearner(
      inputCLI.trainDbFiles.asJava,
      mlnCandidateClauses.toStringHeader,
      mlnCandidateClauses.toStringLearnFormulas,
      doNormalizeLH,
      slNormalize,
      slStrategy,
      slComplexityPenalty,
      slClauses,
      slStepSize,
      slTimeout,
      slThreads,
      slOutputDir);
    structureLearner.run();
  }

  def runLikelihoodEvaluation() {
    if (inputCLI.testDbMlns.isEmpty) {
      argumentParser.usage("No test database files given for likelihood evaluation.")
    }
    val mln = inputCLI.modelStructure.asInstanceOf[MLN]
    Likelihood.mlnLikelihood(
      mln,
      inputCLI.testDbMlns,
      verbose = debugCLI.verbose,
      normalizell = doNormalizeLH)
  }

  def runPseudoLikelihoodEvaluation() {
    if (inputCLI.testDbMlns.isEmpty) {
      argumentParser.usage("No test database files given for pseudolikelihood evaluation.")
    }
    val mln = inputCLI.modelStructure.asInstanceOf[MLN]
    Likelihood.mlnPseudoLikelihood(
      mln,
      inputCLI.testDbMlns,
      verbose = debugCLI.verbose,
      normalizepll = doNormalizeLH)
  }

}
