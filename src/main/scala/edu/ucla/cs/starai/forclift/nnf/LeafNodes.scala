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

package edu.ucla.cs.starai.forclift.nnf

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import scala.sys.process._
import System._
import collection.mutable.ListBuffer

import breeze.math._

class GroundingNode(val cnf: CNF, val explanation: String = "") extends NNFNode {

  def size = 1

  lazy val domains = cnf.domains

  def evalOrder = throw new UnsupportedOperationException

  def smooth = {
    val counted = removeSubsumed(cnf.toPositiveUnitClauses)
    (this, counted)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]): NNFNode = throw new UnsupportedOperationException

  def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=red!40",texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n"), "")
  }

}

object TrueNode extends NNFNode {

  def size = 1

  lazy val domains = Set.empty[Domain]

  def evalOrder = 0

  val smooth = (this, Set[PositiveUnitClause]())

  def condition(pos: Set[Atom], neg: Set[Atom]) = this

  val cnf = CNF()

  def explanation: String = ""

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=green!40",texlbl="""" + fontsize + """ $\top$"];""" + "\n"), "")
  }
}

object FalseNode extends NNFNode {

  def size = 1

  lazy val domains = Set.empty[Domain]

  def evalOrder = 0

  val smooth = (this, Set[PositiveUnitClause]())

  def condition(pos: Set[Atom], neg: Set[Atom]) = this

  val cnf = CNF()

  def explanation: String = ""

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=red!40",texlbl="""" + fontsize + """ $\bot$"];""" + "\n"), "")
  }
}

class ContradictionLeaf(val cnf: CNF, val clause: ContradictionClause, val positive: Boolean, val explanation: String = "") extends NNFNode {

  def size = 1

  def evalOrder = 0
  
  lazy val domains = clause.domains

  val smooth = (this, Set.empty[PositiveUnitClause])

  def condition(pos: Set[Atom], neg: Set[Atom]) = this

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=red!40",texlbl="""" + fontsize + """ """ + cnf.toLatex(true) + """"];""" + "\n"), "")
  }

}

class UnitLeaf(val cnf: CNF, val clause: UnitClause, val positive: Boolean, val explanation: String = "") extends NNFNode {

  require(clause.isUnconditional, "Unit leafs have to be unconditional for smoothing to be correct: "+clause)
  
  def size = 1

  def evalOrder = 0

  lazy val domains = clause.domains

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    if (clause.atom.isGround) {
      if (pos.contains(clause.atom)) TrueNode
      else if (neg.contains(clause.atom)) FalseNode
      else this
    } else this
  }

  val smooth = (this, Set(clause.toPositiveUnitClause))

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false,
    depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=green!20",texlbl="""" + fontsize + """ """ + cnf.toLatex(true) + """"];""" + "\n"), "")
  }

}

class SmoothingNode(val clause: PositiveUnitClause) extends NNFNode {

  def size = 1

  lazy val domains = clause.domains

  def evalOrder = 0

  lazy val cnf = CNF(Clause(List(clause.atom), List(clause.atom), clause.constrs))

  def explanation = ""

  val smooth = (this, Set(clause.toPositiveUnitClause))

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    if (clause.atom.isGround) {
      if (pos.contains(clause.atom) || neg.contains(clause.atom)) TrueNode
      else this
    } else this
  }

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (("  " + getName(nameSpace) + """ [style="fill=blue!20",texlbl="""" + fontsize + """ """ + cnf.toLatex(true) + """"];""" + "\n"), "")
  }

}
