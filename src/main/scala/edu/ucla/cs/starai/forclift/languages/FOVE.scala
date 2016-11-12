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

import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.CNF
import edu.ucla.cs.starai.forclift.Clause
import edu.ucla.cs.starai.forclift.Predicate
import edu.ucla.cs.starai.forclift.VarNameSpace
import edu.ucla.cs.starai.forclift.compiler.Compiler
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.inference.PredicateWeights.map2PredicateWeights
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.inference.Weights
import edu.ucla.cs.starai.forclift.util.NameSpace

class ResNameSpace extends NameSpace[BrazFactor, String] {

  var nbPredicates = 0

  override def createName(f: BrazFactor) = {
    nbPredicates += 1
    "f_" + nbPredicates
  }

}

abstract class BrazFactor {

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF

}

case class Query(literal: (Boolean, Atom)) extends BrazFactor {

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF = {
    val atom = literal._2
    val clauses = if (literal._1) List(Clause(List(atom), List()).standardizeApart)
    else List(Clause(List(), List(atom)).standardizeApart)
    val cnf = new CNF(clauses)
    WeightedCNF(cnf, DomainSizes.empty, PredicateWeights.empty)
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val atomName = literal._2.toString(nameSpace, literal._1)
    atomName
  }

}

case class ConjunctiveFactor(literals: List[(Boolean, Atom)], w: Double, negW: Double) extends BrazFactor {

  def isHardClause = (w == 1 && negW == 0)

  require((!w.isInfinity && !negW.isInfinity), "Weight are not in log space! Use (1,0) for hard clauses.")

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF = {
    if (isHardClause) {
      val clauses = literals.map {
        case (sign, atom) =>
          Clause.empty.addLiteral(true, atom).standardizeApart
      }
      WeightedCNF(new CNF(clauses), DomainSizes.empty, PredicateWeights.empty)
    } else {
      val prefix = nameSpace.getName(this)
      val vars = literals.flatMap { _._2.variables }.toSet.toList
      val domains = vars.map { v =>
        val atom = literals.find { _._2.args.contains(v) }.get._2
        atom.domain(v)
      }
      val res = new Predicate(Symbol(prefix + "}"), vars.size, domains)
      val f = res(vars: _*)

      var c0 = Clause.empty
      c0 = c0.addLiteral(true, f)
      for ((s, a) <- literals) {
        c0 = c0.addLiteral(!s, a)
      }
      val otherLiterals = for ((s, a) <- literals) yield {
        var cn = Clause.empty
        cn = cn.addLiteral(false, f)
        cn = cn.addLiteral(s, a)
        cn
      }
      val clauses = (c0 :: otherLiterals).map { _.standardizeApart }
      val cnf = new CNF(clauses)
      val predWeights = PredicateWeights.empty + (res -> Weights(w, negW))
      WeightedCNF(cnf, DomainSizes.empty, predWeights)
    }
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val atomNames = literals.map { literal => literal._2.toString(nameSpace, literal._1) }
    atomNames.mkString(" and ") + " " + w
  }

}

case class DisjunctiveFactor(literals: List[(Boolean, Atom)], w: Double, negW: Double) extends BrazFactor {

  def isHardClause = (w == 1 && negW == 0)

  require((!w.isInfinity && !negW.isInfinity), "Weight are not in log space! Use (1,0) for hard clauses.")

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF = {
    if (isHardClause) {
      val (posLiterals, negLiterals) = literals.partition { case (sign, atom) => sign }
      val posLiterals2 = posLiterals.map { case (sign, atom) => atom }
      val negLiterals2 = negLiterals.map { case (sign, atom) => atom }
      val clause = (new Clause(posLiterals2, negLiterals2)).standardizeApart
      WeightedCNF(new CNF(List(clause)), DomainSizes.empty, PredicateWeights.empty)
    } else {
      val prefix = nameSpace.getName(this)
      val vars = literals.flatMap { _._2.variables }.toSet.toList
      val domains = vars.map { v =>
        val atom = literals.find { _._2.args.contains(v) }.get._2
        atom.domain(v)
      }
      val res = new Predicate(Symbol(prefix + "}"), vars.size, domains)
      val f = res(vars: _*)

      var c0 = Clause.empty
      c0 = c0.addLiteral(false, f)
      for ((s, a) <- literals) {
        c0 = c0.addLiteral(s, a)
      }
      val otherLiterals = for ((s, a) <- literals) yield {
        var cn = Clause.empty
        cn = cn.addLiteral(true, f)
        cn = cn.addLiteral(!s, a)
        cn
      }
      val clauses = (c0 :: otherLiterals).map { _.standardizeApart }
      val cnf = new CNF(clauses)
      val predWeights = PredicateWeights.empty + (res -> Weights(w, negW))
      WeightedCNF(cnf, DomainSizes.empty, predWeights)
    }
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val atomNames = literals.map { literal => literal._2.toString(nameSpace, literal._1) }
    atomNames.mkString(" or ") + " " + w
  }

}

case class IfThen(ifl: (Boolean, Atom), thenl: (Boolean, Atom), w: Double) extends BrazFactor {

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF = {
    val prefix = nameSpace.getName(this)
    val ifs = ifl._1
    val thens = thenl._1
    val ifa = ifl._2
    val thena = thenl._2
    val vars = (ifa.variables union thena.variables).toList
    val domains = vars.map { v =>
      val atom = List(ifa, thena).find { _.args.contains(v) }.get
      atom.domain(v)
    }
    val res1 = new Predicate(Symbol(prefix + ",1}"), vars.size, domains)
    val res2 = new Predicate(Symbol(prefix + ",2}"), vars.size, domains)
    val res3 = new Predicate(Symbol(prefix + ",3}"), vars.size, domains)
    val f1 = res1(vars: _*)
    val f2 = res2(vars: _*)
    val f3 = res3(vars: _*)

    var c1_1 = Clause.empty
    c1_1 = c1_1.addLiteral(ifs, ifa)
    c1_1 = c1_1.addLiteral(false, f1)
    var c1_2 = Clause.empty
    c1_2 = c1_2.addLiteral(thens, thena)
    c1_2 = c1_2.addLiteral(false, f1)
    var c1_3 = Clause.empty
    c1_3 = c1_3.addLiteral(!ifs, ifa)
    c1_3 = c1_3.addLiteral(!thens, thena)
    c1_3 = c1_3.addLiteral(true, f1)

    var c2_1 = Clause.empty
    c2_1 = c2_1.addLiteral(ifs, ifa)
    c2_1 = c2_1.addLiteral(false, f2)
    var c2_2 = Clause.empty
    c2_2 = c2_2.addLiteral(!thens, thena)
    c2_2 = c2_2.addLiteral(false, f2)
    var c2_3 = Clause.empty
    c2_3 = c2_3.addLiteral(!ifs, ifa)
    c2_3 = c2_3.addLiteral(thens, thena)
    c2_3 = c2_3.addLiteral(true, f2)

    var c3_1 = Clause.empty
    c3_1 = c3_1.addLiteral(ifs, ifa)
    c3_1 = c3_1.addLiteral(true, f3)
    var c3_2 = Clause.empty
    c3_2 = c3_2.addLiteral(!ifs, ifa)
    c3_2 = c3_2.addLiteral(false, f3)
    val clauses = List(c1_1, c1_2, c1_3, c2_1, c2_2, c2_3, c3_1, c3_2).map { _.standardizeApart }
    val cnf = new CNF(clauses)
    val predWeights = PredicateWeights.empty ++ List(res1 -> Weights(w, 1), res2 -> Weights(1 - w, 1), res3 -> Weights(0.5, 1))
    WeightedCNF(cnf, DomainSizes.empty, predWeights)
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val ifName = ifl._2.toString(nameSpace, ifl._1)
    val thenName = thenl._2.toString(nameSpace, thenl._1)
    "if " + ifName + " then " + thenName + " " + w
  }

}

case class IfThenElse(ifl: (Boolean, Atom), thenl: (Boolean, Atom), p: Double, q: Double) extends BrazFactor {

  def toWeightedCNF(nameSpace: ResNameSpace): WeightedCNF = {
    val wmc1 = IfThen(ifl, thenl, p).toWeightedCNF(nameSpace)
    val wmc2 = IfThen((!ifl._1, ifl._2), thenl, q).toWeightedCNF(nameSpace)
    wmc1 ++ wmc2
  }

  override def toString = {
    val nameSpace = new VarNameSpace
    val ifName = ifl._2.toString(nameSpace, ifl._1)
    val thenName = thenl._2.toString(nameSpace, thenl._1)
    "if " + ifName + " then " + thenName + " " + p + " else " + q
  }

}

case class FactorGraph(
  factors: List[BrazFactor],
  val domainSizes: DomainSizes,
  val predicateWeights: PredicateWeights) extends StatRelModel{

  def toWeightedCNF(compilerBuilder: Compiler.Builder = Compiler.Builder.default): WeightedCNF = {
    val nameSpace = new ResNameSpace
    val wcnfs = factors.map(_.toWeightedCNF(nameSpace))
    val startWcnf = WeightedCNF(CNF(), domainSizes, predicateWeights, compilerBuilder = compilerBuilder)
    val wcnf = wcnfs.foldLeft(startWcnf) { _ ++ _ }
    wcnf
  }

  override def toString = (domainSizes :: predicateWeights :: factors).mkString("\n")

}
