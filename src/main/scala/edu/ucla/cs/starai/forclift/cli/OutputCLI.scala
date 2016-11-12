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
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.ArgotParser
import org.clapper.argot.FlagOption
import org.clapper.argot.SingleValueOption
import org.clapper.argot.SingleValueParameter
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.languages.FileFormat
import edu.ucla.cs.starai.forclift.languages.FastInfFormat
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import java.io.FileWriter
import scala.io.Source
import edu.ucla.cs.starai.forclift.languages.ProbLogFormat
import edu.ucla.cs.starai.forclift.languages.focnf.FormatMLNAsFOCNF
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNFParser
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.languages.mln.MLNGrounder
import edu.ucla.cs.starai.forclift.languages.mln.FormatFOCNFAsMLN
import edu.ucla.cs.starai.forclift.languages.ModelConverters._
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNF
import edu.ucla.cs.starai.forclift.util.Output

/**
 * Handle all output logic for CLI
 */
class OutputCLI(
    argumentParser: ArgotParser, 
    debugCLI: DebugCLI,
    inputCLI: InputCLI) {
  
  /* OUTPUT/CONVERSION FLAGS */

  val mlnFileFlag = argumentParser.option[File](
    List("mln-out"),
    "filename",
    s"Output file for MLN (ground if ${debugCLI.showGroundingFlag} is on)") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def mlnFile = mlnFileFlag.value

  val problogFileFlag = argumentParser.option[File](
    List("problog-out"),
    "filename",
    "Output file for ProbLog file") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def problogFile = problogFileFlag.value

  val focnfFileFlag = argumentParser.option[File](
    List("focnf-out"),
    "filename",
    "Output file for First-Order CNF (FOCNF) file") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def focnfFile = focnfFileFlag.value

  val dimacsFileFlag = argumentParser.option[File](
    List("dimacs-out"),
    "filename",
    "Output file for ground DIMACS file") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def dimacsFile = dimacsFileFlag.value
  
  val weightedDimacsFileFlag = argumentParser.option[File](
    List("dimacs-out"),
    "filename",
    "Output file for ground weighted DIMACS file (Lifted MCMC format)") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def weightedDimacsFile = weightedDimacsFileFlag.value

  val fastinfFileFlag = argumentParser.option[File](
    List("fastinf-out"),
    "filename",
    "Output file for FastInf") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def fastinfFile = fastinfFileFlag.value

  val fastinfDataFileFlag = argumentParser.option[File](
    List("fastinfdata-out"),
    "filename",
    "Output file for FastInf data") {
      (s, opt) =>
        val file = new File(s)
        file
    }
  def fastinfDataFile = fastinfDataFileFlag.value

  def runOutput(){
    if(mlnFile.nonEmpty) outputMLN()
    if(fastinfFile.nonEmpty) outputFastInf()
    if(problogFile.nonEmpty) outputProbLog()
    if(focnfFile.nonEmpty) outputFOCNF()
    if(dimacsFile.nonEmpty) outputDIMACS()
    if(weightedDimacsFile.nonEmpty) outputWeightedDIMACS()
  }
  
  def outputFastInf(){
    if(inputCLI.inputFileFormat != FileFormat.MLN){
      argumentParser.usage("FastInf output only supports MLN input.")
    }
    println("Translating MLN to FastInf format")
    val mln = inputCLI.model.asInstanceOf[MLN]
    
    val fif = new FastInfFormat(mln,debugCLI.verbose)

    // Write theory file
    Output.writeToFile(fastinfFile.get,fif.toString,"Wrote theory to " + fastinfFile.get)

    if (fastinfDataFile.nonEmpty) {
      // Write training data file
      Output.writeToFile(fastinfDataFile.get,fif.dataToString(inputCLI.trainDbMlns),"Wrote training data to " + fastinfDataFile.get)
      // Write test data file
      val outtestfile = new File(fastinfDataFile.get.getAbsolutePath.replaceAll(".data", "") + ".test.data")
      Output.writeToFile(outtestfile,fif.dataToString(inputCLI.testDbMlns),"Wrote test data to " + outtestfile)
    }
  }
  
  def outputProbLog(){
    if(inputCLI.inputFileFormat != FileFormat.MLN){
      argumentParser.usage("ProbLog output only supports MLN input.")
    }
    println("Translating MLN to ProbLog format")
    val mln = inputCLI.model.asInstanceOf[MLN]
    val problog = new ProbLogFormat(mln,debugCLI.verbose)
    Output.writeToFile(problogFile.get,problog.toString,"Wrote theory to " + problogFile.get)
  }
  
  def outputFOCNF(){
    if(inputCLI.inputFileFormat != FileFormat.MLN){
      argumentParser.usage("FO-CNF output only supports MLN input.")
    }
    println("Translating MLN to FO-CNF format")
    val mln = inputCLI.model.asInstanceOf[MLN]
    val fmt = FormatMLNAsFOCNF(mln,debugCLI.verbose)
    val focnf = fmt.focnf
    val focnfStr = focnf+"\nc Translation to FO-CNF from Markov Logic using WFOMC\n"
    Output.writeToFile(focnfFile.get,focnfStr,"Wrote theory to " + focnfFile.get)
  }
  
  def outputDIMACS(){
    println(s"Translating ${inputCLI.inputFileFormat} to DIMACS format")
    val wcnf: WeightedCNF = inputCLI.model
    val dimacsStr = wcnf.toSmoothDimacsCNF.basicCnf.toString
    Output.writeToFile(dimacsFile.get,dimacsStr,"Wrote theory to " + dimacsFile.get)
  }
  
  def outputWeightedDIMACS(){
    if(inputCLI.inputFileFormat != FileFormat.MLN){
      argumentParser.usage("Weighted DIMACS output only supports MLN input.")
    }
    println("Translating MLN to Weighted DIMACS format")
    val mln = inputCLI.model.asInstanceOf[MLN]
    MLNGrounder.ground(mln, None, weightedDimacsFile)
  }
      
  def outputMLN(){
    val mln = inputCLI.inputFileFormat match{
      case FileFormat.MLN => inputCLI.asInstanceOf[MLN]
      case FileFormat.FOCNF => {
        println("Translating FO-CNF to MLN format")
        val focnf = inputCLI.model.asInstanceOf[FOCNF]
        val fmt = FormatFOCNFAsMLN(focnf)
        fmt.mln
      }
      case _ => argumentParser.usage(s"MLN output does not support ${inputCLI.inputFileFormat} input.")
    }
    if(debugCLI.showGrounding){
      MLNGrounder.ground(mln, mlnFile, None)
    }else{
      Output.writeToFile(mlnFile.get,mln.toStringEquivalent,"Wrote theory to " + mlnFile.get)
    }
  }
  
}
