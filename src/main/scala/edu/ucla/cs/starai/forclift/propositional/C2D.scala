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

package edu.ucla.cs.starai.forclift.propositional

import scala.io.Source
import java.io.File
import System._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import breeze.math._
import edu.ucla.cs.starai.forclift.util.SignLogDouble

class C2DError(msg: String) extends Exception(msg)

abstract class AbstractC2D {

  val dir = new File("external/tmp/")
  dir.mkdir()
  val random = new util.Random()
  var c2dInputFile = s"external/tmp/wmcproblem.${random.nextLong}.cnf"
  while ((new File(c2dInputFile)).exists()) {
	  c2dInputFile = s"external/tmp/wmcproblem.${random.nextLong}.cnf"
  }
  //print("Creating file: %s" format c2dInputFile)

  def weightedModelCount(cnf: DimacsCNF): SignLogDouble = {
    writeScript(cnf)
    runC2D(cnf)
    val wmc = propagate(cnf)
    cleanup()
    wmc
  }

  def probability(cnf: DimacsCNF, query: Int): SignLogDouble = {
    writeScript(cnf)
    runC2D(cnf)
    val prob = propagateProbability(cnf, query)
    cleanup()
    prob
  }

  /**
   * Run the external c2d tool from A. Darwiche.
   * http://reasoning.cs.ucla.edu/c2d/
   */
  def runC2D(cnf: DimacsCNF) {
    ExternalBinaries.checkC2DAvailable
    val pb = new ProcessBuilder(ExternalBinaries.c2dCmd, /*"-reduce",*/ "-in", c2dInputFile, "-smooth_all")
    val process = pb.start()
    val thread = new Thread {
      override def run() {
        process.destroy()
      }
    }
    Runtime.getRuntime.addShutdownHook(thread)
    process.waitFor()
    Runtime.getRuntime.removeShutdownHook(thread)
    System.err.print(Source.fromInputStream(process.getErrorStream).getLines().mkString("\n"))
    if (process.exitValue != 0) {
      val output = (Source.fromInputStream(process.getInputStream).getLines().mkString("\n"))
      throw new C2DError(s"C2D exited with status ${process.exitValue}:\n $output")
    }
    //		println(Source.fromInputStream(process.getInputStream).getLines().mkString("\n"))
    //		println
  }

  def writeScript(cnf: DimacsCNF) = {
    //		println("Writing C2D input to "+c2dInputFile+".")
    val scriptFile = new java.io.FileWriter(c2dInputFile)
    try {
      scriptFile.write(cnf.basicCnf.toString)
      scriptFile.close
    } finally { scriptFile.close }
  }

  val FirstLine = """nnf ([0-9]+) ([0-9]+) ([0-9]+)""".r

  val Leaf = """L ([-]?[1-9][0-9]*)""".r

  val And = """A ([1-9][0-9]*) (.*)""".r
  val Or = """O ([0-9]+) ([1-9][0-9]*) (.*)""".r

  val True = """A 0""".r
  val False = """O 0 0""".r

  def cleanup() {
    (new java.io.File(c2dInputFile)).delete
    (new java.io.File(c2dInputFile + ".nnf")).delete
  }

  def propagate(cnf: DimacsCNF): SignLogDouble

  def propagateProbability(cnf: DimacsCNF, query: Int): SignLogDouble

}

class LogC2D extends AbstractC2D {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._

  override def probability(cnf: DimacsCNF, query: Int): SignLogDouble = {
    if (cnf.pline.nbVars == 1 && cnf.pline.nbClauses == 1 && cnf.clauseLines.head.vars.size == 1) {
      // c2d crashes on this case
      require(query == 1)
      if (cnf.clauseLines.head.vars == List(1)) one
      else if (cnf.clauseLines.head.vars == List(-1)) zero
      else throw new IllegalStateException
    } else if (cnf.pline.nbVars == 0) {
      val numerator = (cnf.weights(query)._1)
      val denominator = (cnf.weights(query)._1) + (cnf.weights(query)._2)
      numerator / denominator
    } else {
      super.probability(cnf, query)
    }
  }

  override def weightedModelCount(cnf: DimacsCNF): SignLogDouble = {
    if (cnf.pline.nbVars == 1 && cnf.pline.nbClauses == 1 && cnf.clauseLines.head.vars.size == 1) {
      // c2d crashes on this case
      if (cnf.clauseLines.head.vars == List(1)) (cnf.weights(1)._1)
      else if (cnf.clauseLines.head.vars == List(-1)) (cnf.weights(1)._2)
      else throw new IllegalStateException
    } else if (cnf.pline.nbVars == 0) {
      throw new C2DError("Without variables, model count is undefined.") // can be 0 or 1, depends!
    } else if (cnf.isTautology) {
      cnf.weights.map { case (w, nw) => (w) + (nw)}.foldLeft(one) { _ * _ }
    } else {
      super.weightedModelCount(cnf)
    }
  }

  def propagate(cnf: DimacsCNF): SignLogDouble = {
    //		println("Propagating weights")
    val src = Source.fromFile(c2dInputFile + ".nnf")
    val lines = src.getLines
    val firstLine = lines.next
    val weight = firstLine match {
      case FirstLine(nbNodes, _, _) => {
        var cache = new Array[SignLogDouble](nbNodes.toInt)
        var lineIndex = 0
        for (line <- lines) {
          val prob = line match {
            case Or(_, nbChildren, children) => {
              children.split(" ").map { child => cache(child.toInt) }.reduceLeft { _ + _ }
            }
            case And(nbChildren, children) => {
              children.split(" ").map { child => cache(child.toInt) }.reduceLeft { _ * _ }
            }
            case Leaf(l) => {
              val variable = l.toInt
              if (variable < 0) (cnf.weights(-variable)._2)
              else if ((variable > 0)) (cnf.weights(variable)._1)
              else throw new IllegalStateException
            }
            case False() => zero
            case True() => one
            case _ => {
              throw new IllegalStateException(line + " did not parse.")
            }
          }
          cache(lineIndex) = prob
          lineIndex += 1
        }
        // debug (0 to cache.size zip cache) foreach { case(l,p) => println(l+": "+p)}
        cache(lineIndex - 1)
      }
      case _ => throw new IllegalStateException(firstLine + " did not parse as " + FirstLine)
    }
    src.close
    weight
  }

  def propagateProbability(cnf: DimacsCNF, query: Int): SignLogDouble = {
    val Z = propagate(cnf)
    val qWeight = propagateForQuery(cnf, query)
    qWeight - Z
  }

  def propagateForQuery(cnf: DimacsCNF, query: Int): SignLogDouble = {
    //		println("Propagating weights")
    val src = Source.fromFile(c2dInputFile + ".nnf")
    val lines = src.getLines
    val firstLine = lines.next
    val weight = firstLine match {
      case FirstLine(nbNodes, _, _) => {
        var cache = new Array[SignLogDouble](nbNodes.toInt)
        var lineIndex = 0
        for (line <- lines) {
          val prob = line match {
            case Or(_, nbChildren, children) => {
              children.split(" ").map { child => cache(child.toInt) }.reduceLeft {_ + _}
            }
            case And(nbChildren, children) => {
              children.split(" ").map { child => cache(child.toInt) }.reduceLeft {_ * _}
            }
            case Leaf(l) => {
              val variable = l.toInt
              if (variable < 0 && query == variable.abs) zero
              else if (variable < 0) (cnf.weights(-variable)._2)
              else if ((variable > 0)) (cnf.weights(variable)._1)
              else throw new IllegalStateException
            }
            case False() => zero
            case True() => one
            case _ => throw new IllegalStateException(line + " did not parse.")
          }
          cache(lineIndex) = prob
          lineIndex += 1
        }
        // debug (0 to cache.size zip cache) foreach { case(l,p) => println(l+": "+p)}
        cache(lineIndex - 1)
      }
      case _ => throw new IllegalStateException(firstLine + " did not parse as " + FirstLine)
    }
    src.close
    weight
  }

}
