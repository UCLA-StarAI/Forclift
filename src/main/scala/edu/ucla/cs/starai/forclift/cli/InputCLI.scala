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

import java.io.{FileNotFoundException, IOException, File}

import scala.io.Source
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.ArgotConverters.convertFlag
import org.clapper.argot.ArgotParser
import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.languages.FactorGraph
import edu.ucla.cs.starai.forclift.languages.FactorGraphParser
import edu.ucla.cs.starai.forclift.languages.FileFormat
import edu.ucla.cs.starai.forclift.languages.FileFormat.stringToFileFormat
import edu.ucla.cs.starai.forclift.languages.ModelParser
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNF
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNFParser
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import edu.ucla.cs.starai.forclift.languages.ModelConverters._
import edu.ucla.cs.starai.forclift.PositiveUnitClause
  
/**
 * Handle all input logic for CLI
 */
class InputCLI(argumentParser: ArgotParser, debugCLI: DebugCLI) {
  
  /* INPUT PARSING FLAGS */
  
  val inputFileFlag = argumentParser.parameter[File](
    "input",
    "Input file to read.",
    false) {
      (s, opt) =>
        val file = new File(s)
        if (!file.exists)
          argumentParser.usage("Input file \"" + s + "\" does not exist.")
        file
    }
  def inputFile: File = {
    if (inputFileFlag.value.isEmpty) {
      throw new Exception("No filename given")
    }
    inputFileFlag.value.get 
  }
  
  def inputFileExtension: String = {
    val extregex = """.*\.([^.]+)""".r
    inputFile.toString() match {
      case extregex(e) => e
      case _           => ""
    }
  }
  
  val inputFileFormatFlag = argumentParser.option[FileFormat](
    List("format-in"),
    "file format",
    s"File format of input model: ${FileFormat.FOCNF.extension}, ${FileFormat.MLN.extension}, ${FileFormat.FactorGraph.extension}, or ${FileFormat.WeightedGroupLogic.extension}"
      + s" (for ${FileFormat.FOCNF}, ${FileFormat.MLN}, ${FileFormat.FactorGraph}, or ${FileFormat.WeightedGroupLogic}). When not specified, the file type is detected from the extension.") {
      (s, opt) =>
        val typeOpt: Option[FileFormat] = s
        typeOpt match {
          case Some(t) => t
          case None    => argumentParser.usage("Invalid file type: \"" + s + "\".")
        }
    }
  
  lazy val inputFileFormat: FileFormat = inputFileFormatFlag.value match{
      case Some(explicitFormat) => explicitFormat
      case None => {
        val extensionFormat: Option[FileFormat] = inputFileExtension
        extensionFormat match{
          case Some(fileType) => fileType
          case None => inputFileFlag.parent.usage(
              s"""Could not determine file format of "$inputFile". Please indicate with ${inputFileFormatFlag.names} flag.""")
        }
      }
    }

  val alchemyFlag = argumentParser.flag[Boolean](
    List("alchemy"),
    "Simulate Alchemy semantics: split complex MLN formulas into clauses, distribute the weights, and turn existential quantifiers into disjunctions. (required by weight learning)")
  def alchemy = alchemyFlag.value.getOrElse(false)
  
  val queryFlag = argumentParser.option[String](
    List("q", "query"),
    "atom",
    "Query atom. If omitted, all marginals are calculated.")
    
  lazy val queryOpt: Option[PositiveUnitClause] = {
    queryFlag.value.map(parser.parseAtom(_).toPositiveUnitClause)
  }
  def hasQuery = queryOpt.nonEmpty
  def query = queryOpt.get
  
  val trainDbFilesFlag = argumentParser.multiOption[File](
    List("train"),
    "filename",
    "Database file with data for training.") {
      (s, opt) =>
        val file = new File(s)
        if (!file.exists)
          argumentParser.usage("Data file \"" + s + "\" does not exist.")
        file
    }
  def trainDbFiles: Seq[File] = trainDbFilesFlag.value

  val testDbFilesFlag = argumentParser.multiOption[File](
    List("test"),
    "filename",
    "Database file with data for testing.") {
      (s, opt) =>
        val file = new File(s)
        if (!file.exists)
          argumentParser.usage("Data file \"" + s + "\" does not exist.")
        file
    }
  def testDbFiles = testDbFilesFlag.value
  
  
  /**
   * Read in the model using command line flags
   */
  lazy val (model,parser): (StatRelModel,ModelParser) = {

    val theoryFile = Source.fromFile(inputFile)
    try {
      val theoryStr = theoryFile.mkString
      if (theoryStr.length == 0) {
        inputFileFlag.parent.usage(s"Input file $inputFile is empty.")
      }

      println(s"Reading model using $inputFileFormat syntax.")
      val (model, parser) = inputFileFormat match {
        case FileFormat.FOCNF => parseFOCNF(theoryStr)
        case FileFormat.MLN => parseMLN(theoryStr)
        case FileFormat.FactorGraph => parseFactorGraph(theoryStr)
        case _ => throw new UnsupportedOperationException(s"Could not find parser for $inputFileFormat")
      }
      if (debugCLI.verbose) {
        println("Done parsing model:")
        println(model)
      }
      (model, parser)
    } catch {
      case e: Exception => throw new Exception(s"can't read file $inputFile: ${e.getMessage}")
    }
    finally {
      theoryFile.close()
    }
  }
  
  lazy val wcnfModel: WeightedCNF = model
  
  def parseMLN(theoryStr: String): (MLN,ModelParser) = {
    val parser = new MLNParser
    val mln = parser.parseMLN(theoryStr + "\n")
    mln.setAlchemySemantics(alchemy)
    (mln,parser)
  }
  
  def parseFactorGraph(theoryStr: String): (FactorGraph,FactorGraphParser) = {
    val parser = new FactorGraphParser
    val model = parser.parseModel(theoryStr + "\n")
    (model,parser)
  }
  
  def parseFOCNF(theoryStr: String): (FOCNF,FOCNFParser) = {
    val parser = new FOCNFParser
    val model = parser.parseModel(theoryStr + "\n")
    (model,parser)
  }
  
  //TODO add parser for WeightedGroupLogic
  
  /**
   * Read in the model structure (for learning) using command line flags
   */
  lazy val (modelStructure,structureParser): (StatRelModel,ModelParser) = {

    val theoryFile = Source.fromFile(inputFile)
    try {
      val theoryStr = theoryFile.mkString
      if (theoryStr.length == 0) {
        inputFileFlag.parent.usage(s"Input file $inputFile is empty.")
      }

      println(s"Reading model structure using $inputFileFormat syntax.")
      val (modelStructure, structureParser) = inputFileFormat match {
        case FileFormat.MLN => parseMLNStructure(theoryStr)
        case _ => throw new UnsupportedOperationException(s"Could not find parser for $inputFileFormat structures (for learning)")
      }
      if (debugCLI.verbose) {
        println("Done parsing model structure:")
        println(modelStructure)
      }
      (modelStructure, structureParser)
    } catch {
      case e: IOException => throw new Exception(s"Problem with the file $inputFile: " + e.getMessage)
      case e: FileNotFoundException => throw new Exception(s"Can't find file $inputFile")
    }
    finally {
      theoryFile.close()
    }
  }
  
  def parseMLNStructure(theoryStr: String): (MLN,ModelParser) = {
    val parser = new MLNParser
    // set learning models to parse a structure only
    parser.setLearnModus(true)
    val mln = parser.parseMLN(theoryStr + "\n")
    mln.setAlchemySemantics(alchemy)
    (mln,parser)
  }
  
  lazy val (trainDbMlns, testDbMlns) = inputFileFormat match{
      case FileFormat.MLN => parseMLNDatabases()
      case _ => throw new UnsupportedOperationException(s"Could not find parser for $inputFileFormat databases (for learning)")
    }
  
  def parseMLNDatabases()= {
    val mlnStructParser = structureParser.asInstanceOf[MLNParser]
    val trainDbMlns = trainDbFiles.map { file =>
      val trainFile = Source.fromFile(file)
      try {
        mlnStructParser.parseDB(trainFile.mkString)
      } catch {
        case e:Exception => throw new Exception(s" something wrong with file $file")
      }
      finally {
        trainFile.close()
      }
    }
      if (debugCLI.verbose) {
        for (dbMln <- trainDbMlns) {
          println("Parsed training db with domains:")
          println(dbMln.domainSizes)
          println
        }
      }
    val testDbMlns = testDbFiles.map { file =>
      val testFile = Source.fromFile(file)
      try {
        mlnStructParser.parseDB(testFile.mkString)
      } catch {
        case e: Exception => throw new Exception(s"something wrong with file $file")
      }
      finally {
        testFile.close()
      }
    }
      if (debugCLI.verbose) {
        for (dbMln <- testDbMlns) {
          println("Parsed testing db with domains:")
          println(dbMln.domainSizes)
          println
        }
      }
      (trainDbMlns, testDbMlns)
  }
    
}
