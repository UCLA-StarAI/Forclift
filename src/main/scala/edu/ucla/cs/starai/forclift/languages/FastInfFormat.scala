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

package edu.ucla.cs.starai.forclift.languages

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.languages.mln._
import java.io._
import scala.io._

case class FastInfClique(
  val id: Int,
  val variables: Seq[Int],
  val neighbours: Seq[Int] = Nil) {

  override def toString = {
    "cliq%d\t".format(id) +
      "%d\t".format(variables.size) +
      variables.mkString(" ") + "\t" +
      "%d\t".format(neighbours.size) +
      neighbours.mkString(" ")
  }
}

case class FastInfMeasure(
  val id: Int,
  val variables: Seq[Int],
  val potential: Seq[Double]) {

  override def toString = {
    "meas%d\t".format(id) +
      "%d\t".format(variables.size) +
      variables.map(v => 2).mkString(" ") + "\t" +
      potential.mkString(" ")
  }
}

class FastInfFormat(
  val mln: MLN,
  useparamtying: Boolean = true,
  verbose: Boolean = false) {
  println("Starting FastInfFormat conversion")

  private val varMap = new mutable.HashMap[Atom, Int]
  private var lastVar = -1
  private var varToClique: List[(Int, Int)] = Nil
  private var cliques: List[FastInfClique] = Nil
  private var measures: List[FastInfMeasure] = Nil
  private var cliqueToMeasures: List[(Int, Int)] = Nil
  private var cliqueToVariables: List[(Int, List[Int])] = Nil

  def getVar(key: Atom): Int = varMap.getOrElseUpdate(key, {
    lastVar += 1
    varMap(key) = lastVar
    lastVar
  })

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }

  def buildPotential(atoms: List[Atom], wf: WeightedFormula): List[Double] = {
    buildPotentialInt(atoms, Nil, wf)
  }

  def buildPotentialInt(atoms: List[Atom], values: List[(Atom, Boolean)], wf: WeightedFormula): List[Double] = atoms match {
    case Nil => {
      if (verbose) println("values:" + values)
      wf.formula.evaluate({ atom =>
        //val atomuc = new PositiveUnitClause(atom)
        values.find {
          case (a, v) =>
            //val auc = new PositiveUnitClause(a)
            //auc.subsumes(atomuc)
            // Assuming that we are using ground formulas
            // Trick to avoid writing an equivalence function
            a.unify(atom).nonEmpty
        } match {
          case None => throw new IllegalStateException("Atom in formula not in value assignment: " + atom)
          case Some((a, v)) => Some(v)
        }
      }) match {
        case f: TrueFormula => math.exp(wf.weight) :: Nil
        case f: FalseFormula => 1 :: Nil
        case f => throw new IllegalStateException("Evaluated formula is not true or false.\n" + f.toString)
      }
    }
    case h :: t => buildPotentialInt(t, (h, false) :: values, wf) :::
      buildPotentialInt(t, (h, true) :: values, wf)
  }

  // CoShatter MLN
  //val smln = mln.coShatter
  val smln = mln

  //println(smln.domainSizes.toStringVerbose)

  // Process al formulas to build data structures
  var measureCnt = 0
  var cliqueCnt = 0

  smln.wformulas.foreach { wformula =>
    val foatoms = wformula.atoms.toList
    wformula.groundWithAtoms(mln.domainSizes, foatoms) match {
      case Nil => {
        println("Warning: Empty grounding for " + wformula)
      }
      case groundwformulas => {

        //println("Grounding "+wformula+" to \n"+groundwformulas.mkString("\n"))

        // Create measure (if param tying)
        if (useparamtying) {
          val headatoms = groundwformulas.head._2.toSeq
          if (verbose) println("Build potential for" + groundwformulas.head._1)
          measures = FastInfMeasure(measureCnt,
            headatoms.map { a => getVar(a) }.toSeq.sorted,
            buildPotential(headatoms.toList,
              groundwformulas.head._1)) :: measures
        }

        groundwformulas.foreach {
          case (groundwformula, groundatoms) =>
            // Create clique
            val variables = groundatoms.map { a => getVar(a) }
            varToClique = variables.toList.map(v => (v, cliqueCnt)) ::: varToClique
            cliqueToVariables = (cliqueCnt, variables) :: cliqueToVariables

            // Create measure (if no param tying)
            if (!useparamtying) {
              val headatoms = groundatoms.toSeq
              if (verbose) println("Build potential for" + groundwformula)
              measures = FastInfMeasure(measureCnt,
                headatoms.map { a => getVar(a) }.toSeq.sorted,
                buildPotential(headatoms.toList,
                  groundwformula)) :: measures
            }

            // Clique to measure
            cliqueToMeasures = (cliqueCnt, measureCnt) :: cliqueToMeasures

            if (verbose) {
              println("Clique " + cliqueCnt + ": " + groundwformula)
              println("  atoms: " + groundatoms.mkString(" "))
              println("  vars:  " + variables.mkString(" "))
            }

            cliqueCnt += 1
            if (!useparamtying) measureCnt += 1
        }
        if (useparamtying) measureCnt += 1
      }
    }
  }

  // Create cliques
  val varToCliques: Map[Int, Set[Int]] = varToClique.groupBy { case (v, c) => v }
    .mapValues { _.map(_._2).toSet }
  cliques = cliqueToVariables.map { ctv =>
    {
      val nbrs: List[Int] = ctv._2.flatMap { v => varToCliques.getOrElse(v, Nil) }
      //println(nbrs.getClass)
      FastInfClique(ctv._1, ctv._2.toSeq, nbrs.filterNot(_ == ctv._1).toSeq.sorted)
    }
  }

  lazy val variableStrings = {
    "@Variables" ::
      varMap.toList.sortBy(_._2).map(_._1.toString + "\t2") :::
      "@End" ::
      "" ::
      Nil
  }

  def variablesToFile(out: FileWriter) = {
    out.write("@Variables\n")
    varMap.toList.sortBy(_._2).foreach { v => out.write(v._1.toString.replaceAll("[(,)]", "_") + "\t2\n") }
    out.write("@End\n\n")
  }

  /**
   * Clique format:
   * cliq0\t2\t0 1\t1\t0
   *
   * Columns:
   * - Clique name
   * - Number of vars in clique
   * - List of var indices in clique
   * - Number of neighbours
   * - List of neighbouring cliques indices
   */
  lazy val cliqueStrings = {
    "@Cliques" ::
      cliques.sortBy(_.id).map(_.toString) :::
      "@End" ::
      "" ::
      Nil
  }

  def cliquesToFile(out: FileWriter) = {
    out.write("@Cliques\n")
    cliques.sortBy(_.id).foreach { c => out.write(c.toString + '\n') }
    out.write("@End\n\n")
  }

  /**
   * For use in parameter estimation cases where you wish to fix some entries
   * in a table during learning.
   *
   * Idle Param format:
   * ip1\t1\tidle param for trial
   *
   * Columns:
   * - Name of parameter
   * - Fixed parameter value
   * - Description
   */
  lazy val idleParamStrings = {
    "@IdleParams" ::
      "@End" ::
      "" ::
      Nil
  }

  def idleParamsToFile(out: FileWriter) = {
    out.write("@IdleParams\n@End\n\n")
  }

  /**
   * For use in parameter estimation cases where you wish to  tie some entries
   * in the table to the same parameter.
   *
   * Shared Param format:
   * ip1\t1\tidle param for trial
   *
   * Columns:
   * - Name of parameter
   * - Initial parameter value
   * - Description
   */
  lazy val sharedParamStrings = {
    "@SharedParams" ::
      "@End" ::
      "" ::
      Nil
  }

  def sharedParamsToFile(out: FileWriter) = {
    out.write("@SharedParams\n@End\n\n")
  }

  /**
   * Measure format:
   * meas0\t2\t76 76\t2.1e-10 0 sp1 0.1 ip1 ...
   *
   * Columns:
   * - Measure name
   * - Number of vars in measure
   * - Vars card (domain sizes of vars ?)
   * - Table of potentials (doubles or a idle or a shared parameter name)
   *   Right side changes fastest: 000 001 010 011 100 ...
   */
  lazy val measureStrings = {
    "@Measures" ::
      measures.sortBy(_.id).map(_.toString) :::
      "@End" ::
      "" ::
      Nil
  }

  def measuresToFile(out: FileWriter) = {
    out.write("@Measures\n")
    measures.sortBy(_.id).foreach { m => out.write(m.toString + "\n") }
    out.write("@End\n\n")
  }

  /**
   * CliqueToMeasure format:
   * 0\t1
   *
   * Columns:
   * - Clique index
   * - Measure index
   */
  lazy val cliqueToMeasureStrings = {
    "@CliqueToMeasure" ::
      cliqueToMeasures.sorted.map { case (c, m) => "%d\t%d".format(c, m) } :::
      "@End" ::
      "" ::
      Nil
  }

  def cliqueToMeasuresToFile(out: FileWriter) = {
    out.write("@CliqueToMeasure\n")
    cliqueToMeasures.sorted.foreach { case (c, m) => out.write("%d\t%d\n".format(c, m)) }
    out.write("@End\n\n")
  }

  /** Unsure what this does. **/
  lazy val directedMeasuresStrings = {
    "@DirectedMeasures" ::
      "@End" ::
      "" ::
      Nil
  }

  def directedMeasuresToFile(out: FileWriter) = {
    out.write("@DirectedMeasures\n@End\n\n")
  }
  
  @deprecated("use toString instead","3.0")
  def toFile(out: FileWriter) = {
    out.write("# Translation to FastInf from Markov Logic using WFOMC\n#\n\n")
    variablesToFile(out)
    idleParamsToFile(out)
    sharedParamsToFile(out)
    cliquesToFile(out)
    measuresToFile(out)
    cliqueToMeasuresToFile(out)
    directedMeasuresToFile(out)
  }

  /**
   * Returns a string containing a FastInf model equivalent to the given MLN
   */
  override def toString = {
    ("# Translation to FastInf from Markov Logic using WFOMC" ::
      "# " ::
      variableStrings :::
      idleParamStrings :::
      sharedParamStrings :::
      cliqueStrings :::
      measureStrings :::
      cliqueToMeasureStrings :::
      directedMeasuresStrings :::
      "" ::
      Nil).mkString("\n")
  }

  /**
   * Returns a string containing FastInf data equivalent to the given database
   */
  def dataToString(dbs: Seq[MLN]): String = {
    //varMap.toList.sortBy(_._2)
    val evidence = Array.fill[Byte](lastVar + 1)(0)
    dbs.map { db =>
      for (i <- evidence.indices) {
        evidence(i) = 0
      }
      db.evidence.foreach {
        case af: LiteralFormula => {
          val atom = af.atom
          evidence(varMap(atom)) = 1
        }
        case _: NegFormula => {
          // default is already 0
        }
        case f => {
          println("Unexpected formula type in database:\n" + f)
        }
      }
      "( " + evidence.mkString(" ") + " )"
    }.mkString("\n")
  }
}
