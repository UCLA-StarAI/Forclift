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

import collection._
import edu.ucla.cs.starai.forclift.util.SignLogDouble

sealed trait DimacsCNFLine

case class Comment(comment: String) extends DimacsCNFLine {

  override def toString = "c " + comment
}

case class PLine(nbVars: Int, nbClauses: Int) extends DimacsCNFLine {

  override def toString = "p cnf " + nbVars + " " + nbClauses

}

case class ClauseLine(vars: List[Int]) extends DimacsCNFLine {

  // bad assumption, can have empty clause for false!
  //	assume(vars.nonEmpty)

  def isTautology = vars.exists { v => vars.contains { -v } }

  override def toString = vars.mkString(" ") + " 0"

}

case class WeightLine(variable: Int, weight: SignLogDouble, weightNeg: SignLogDouble) extends DimacsCNFLine {

  assume(variable > 0, "Weights are for positive literals.")

  override def toString = "w " + variable + " " + weight + " " + weightNeg

}

case class DimacsCNF(lines: List[DimacsCNFLine]) {

  lazy val pline: PLine = lines.view.find { _.isInstanceOf[PLine] }.map { _.asInstanceOf[PLine] }.getOrElse {
    throw new IllegalArgumentException("CNF does not contain a p-line.")
  }

  def basicCnf = {
    val newPLine = PLine(pline.nbVars, clauseLines.size)
    DimacsCNF(newPLine :: clauseLines)
  }

  lazy val clauseLines = lines.collect { case cl: ClauseLine => cl }

  lazy val weights: IndexedSeq[(SignLogDouble, SignLogDouble)] = {
    val array = Array.fill[(SignLogDouble, SignLogDouble)](pline.nbVars + 1)((0.5, 0.5))
    val tuples = for (WeightLine(v, w, wneg) <- lines) yield {
      array(v) = (w, wneg)
    }
    array
  }

  def isTautology = lines.collect { case cl: ClauseLine => cl }.forall { _.isTautology }

  override def toString = lines.mkString("\n")

}

class DimacsCNFBuilder[VarType](val weightFunction: (VarType => (Double, Double))) {

  private val varMap = new mutable.HashMap[VarType, Int]
  private var lastVar = 0
  private var weightLines: List[WeightLine] = Nil

  def getVar(key: VarType): Int = varMap.getOrElseUpdate(key, {
    lastVar += 1
    val (w, wneg) = weightFunction(key)
    varMap(key) = lastVar
    weightLines = WeightLine(lastVar, w, wneg) :: weightLines
    lastVar
  })

  def declare(key: VarType) {
    getVar(key)
  }

  private var clauses: List[ClauseLine] = Nil

  def addClause(posLiterals: List[VarType], negLiterals: List[VarType]) {
    val newClause = ClauseLine(posLiterals.map { getVar(_) } ++ negLiterals.map { -getVar(_) })
    clauses = newClause :: clauses
  }

  def toDimacsCNF: DimacsCNF = {
    val pLine = PLine(lastVar, clauses.size)
    DimacsCNF(pLine :: (weightLines.reverse) ::: (clauses.reverse))
  }
}
