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

package edu.ucla.cs.starai.forclift.languages.mln

import collection._
import java.io._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift._
import scala.Array.canBuildFrom

object MLNGrounder {

  def ground(mln: MLN, groundMlnFile: Option[File], groundDimacsFile: Option[File]) {
    if (groundDimacsFile.isEmpty) {
      val groundMln = mln.ground(mln.domainSizes)
      println("Ground MLN:")
      val str = groundMln.toMLNFileString
      println(str)
      if (groundMlnFile.nonEmpty) {
        val out = new PrintWriter(groundMlnFile.get, "UTF-8")
        try {
          out.print(str + "\n")
        } finally {
          out.close
        }
      }
    }
    if (groundDimacsFile.nonEmpty) {
      // ground MLN in nnf form
      val groundMln = mln.toMLNasNNF.ground(mln.domainSizes)
      println("Ground MLN for DIMACS:")
      val mlnStr = groundMln.toMLNFileString
      println(mlnStr)

      // convert to DIMACS
      val variableMap = new mutable.HashMap[String, Int]
      var maxId = 0;
      def getId(atom: Atom): Int = {
        val string = atom.toString
        variableMap.getOrElseUpdate(string, { maxId += 1; maxId })
      }
      // first assign an ID to each atom in order to have interpretable numbers
      for (p <- mln.predicates) {
        for (atom <- p.toAtom.ground(mln.domainSizes)) getId(atom.toPositiveUnitClause.atom)
      }
      val clauses = groundMln.wformulas.map { formula =>
        val (posLits, negLits) = formula.disjunctionToList
        val literals = negLits.map { "-" + getId(_) } ++ posLits.map { getId(_) } ++ List("0")
        val weight = if (formula.isHard) "inf" else formula.weight
        ("c " + formula.toString + "\n" +
          weight + " " + literals.mkString(" "))
      }
      // describe mappings
      val mappings = variableMap.iterator.toArray.sortBy { _._2 }.map { case (str, int) => "c " + int + " " + str }
      val dimacsStr = (
        "p wcnf " + variableMap.size + " " + clauses.size + "\n" +
        "c variable mappings:" + "\n" +
        mappings.mkString("\n") + "\n" +
        "c clauses:" + "\n" +
        clauses.mkString("\n") + "\n")
      println("Ground DIMACS:")
      println(dimacsStr)

      // write DIMACS
      val out = new PrintWriter(new FileWriter(groundDimacsFile.get));
      try {
        out.print(dimacsStr)
      } finally {
        out.close
      }

      // hack to also have integer weights for saucy
      val weightMap = new mutable.HashMap[Double, Int]
      var maxWeightId = if (groundMln.wformulas.exists(_.isHard)) 2 else 1;
      def getWeightId(weight: Double) = {
        weightMap.getOrElseUpdate(weight, { maxWeightId += 1; maxWeightId })
      }
      case class SaucyClause(w: Int, literals: Set[Int]) {
        def isTautology = literals.exists { l => literals.contains(-l) }
        override def toString = w + " " + literals.toArray.sortWith(_ < _).mkString(" ") + " 0"
      }
      val clauses2 = groundMln.wformulas.map { formula =>
        val (posLits, negLits) = formula.disjunctionToList
        val literals = negLits.map { -getId(_) } ++ posLits.map { getId(_) }
        val weight = if (formula.isHard) 2 else getWeightId(formula.weight)
        SaucyClause(weight, literals.toSet)
      }.filterNot { _.isTautology }.toSet
      val dimacs2Str = (
        "p wcnf " + variableMap.size + " " + clauses2.size + "\n" +
        clauses2.toArray.sortWith(_.w < _.w).mkString("\n") + "\n")
      println("Ground DIMACS 2:")
      println(dimacs2Str)

      // write DIMACS2
      val file2 = new File(groundDimacsFile.get.getAbsolutePath + ".saucy")
      val out2 = new PrintWriter(file2, "UTF-8")
      try {
        out2.print(dimacs2Str)
      } finally {
        out2.close
      }
    }
  }

}
