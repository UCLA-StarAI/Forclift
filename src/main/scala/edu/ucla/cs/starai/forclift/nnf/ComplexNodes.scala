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
import edu.ucla.cs.starai.forclift.nnf.visitors.SafeSignLogDoubleWmc

class Ref(val cnf: CNF, val nnfNode: NNFNode, val explanation: String = "") extends NNFNode {

  def size = 0

  assume(nnfNode != null)

  lazy val smooth = {
    val (n, v) = nnfNode.smooth
    (new Ref(cnf, n, explanation), v)
  }
  
  lazy val domains = nnfNode.domains

  lazy val evalOrder = nnfNode.evalOrder

  def condition(pos: Set[Atom], neg: Set[Atom]) = new Ref(cnf, nnfNode.condition(pos, neg), explanation)

  override def getName(nameSpace: NameSpace[NNFNode, String]) = nnfNode.getName(nameSpace)

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    nameSpace.forceName(this, nameSpace.getName(nnfNode))
    ("", "")
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    ""
  }

}

class And(val cnf: CNF, val l: NNFNode, val r: NNFNode, val explanation: String = "") extends NNFNode {

  def size = l.size + r.size + 1

  lazy val domains = l.domains union r.domains

  lazy val evalOrder = {
    val rOrder = r.evalOrder
    val lOrder = l.evalOrder
    rOrder max lOrder
  }

  lazy val smooth = {
    val (lSmooth, lVars) = l.smooth
    val (rSmooth, rVars) = r.smooth
    val thisSmooth = new And(cnf, lSmooth, rSmooth, explanation)
    val thisVars = lVars union rVars
    val thisNotSubsumedVars = thisVars //removeSubsumed(thisVars) -- should be independent
    (thisSmooth, thisNotSubsumedVars)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new And(cnf, l.condition(pos, neg), r.condition(pos, neg), explanation)

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = l.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (nr, er) = r.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "and" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + l.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + r.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val llwmc = try { wmcVisitor.visit(l.smooth._1, (domainSizes, predicateWeights)) } catch { case e: UnsupportedOperationException => Complex.nan }
        val rlwmc = try { wmcVisitor.visit(r.smooth._1, (domainSizes, predicateWeights)) } catch { case e: UnsupportedOperationException => Complex.nan }
        "  " + getName(nameSpace) + " -> " + "and" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "and" + getName(nameSpace) + " -> " + l.getName(nameSpace) + """ [""" + edgeLabel(" $ " + llwmc + " $ ") + """];""" + "\n" +
          "  " + "and" + getName(nameSpace) + " -> " + r.getName(nameSpace) + """ [""" + edgeLabel(" $ " + rlwmc + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl + nr)
      val edges = (myEdges + el + er)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + l.getName(nameSpace) + " Λ " + r.getName(nameSpace) + "\n" +
      "\n" +
      l.toString(nameSpace) +
      "\n" +
      r.toString(nameSpace))
  }

}

class Or(val cnf: CNF, val l: NNFNode, val r: NNFNode, val explanation: String = "") extends NNFNode {

  def size = l.size + r.size + 1

  lazy val domains = l.domains union r.domains

  lazy val evalOrder = {
    val rOrder = r.evalOrder
    val lOrder = l.evalOrder
    rOrder max lOrder
  }

  lazy val smooth = {
    val (lSmooth, lVars) = l.smooth
    val (rSmooth, rVars) = r.smooth
    val lMissing = rVars.flatMap { _.minus(lVars) }
    val rMissing = lVars.flatMap { _.minus(rVars) }
    val lSmoothAll = lSmooth.smoothWith(lMissing)
    val rSmoothAll = rSmooth.smoothWith(rMissing)
    val thisSmooth = new Or(cnf, lSmoothAll, rSmoothAll, explanation)
    val lVarsAll = lVars union lMissing
    val rVarsAll = rVars union rMissing
    val thisVars = if (lVarsAll.size < rVarsAll.size) lVarsAll else rVarsAll
    (thisSmooth, thisVars)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new Or(cnf, l.condition(pos, neg), r.condition(pos, neg), explanation)

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = l.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (nr, er) = r.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\lor$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "or" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\lor$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + l.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + r.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val llwmc = try { wmcVisitor.visit(l.smooth._1,(domainSizes, predicateWeights)) } catch { case e: UnsupportedOperationException => Complex.nan }
        val rlwmc = try { wmcVisitor.visit(r.smooth._1,(domainSizes, predicateWeights)) } catch { case e: UnsupportedOperationException => Complex.nan }
        "  " + getName(nameSpace) + " -> " + "or" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "or" + getName(nameSpace) + " -> " + l.getName(nameSpace) + """ [""" + edgeLabel(" $ " + llwmc + " $ ") + """];""" + "\n" +
          "  " + "or" + getName(nameSpace) + " -> " + r.getName(nameSpace) + """ [""" + edgeLabel(" $ " + rlwmc + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl + nr)
      val edges = (myEdges + el + er)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + l.getName(nameSpace) + " v " + r.getName(nameSpace) + "\n" +
      "\n" +
      l.toString(nameSpace) +
      "\n" +
      r.toString(nameSpace))
  }

}

class InclusionExclusion(val cnf: CNF, val plus1: NNFNode, val plus2: NNFNode, val min: NNFNode, val explanation: String = "") extends NNFNode {

  def size = plus1.size + plus2.size + min.size + 1

  //    def logwmcc(domainSizes: DomainSizes, predicateWeights: PredicateWeights): Double = {
  //        val plus1lwmc = plus1.logwmcc(domainSizes, predicateWeights)
  //        val plus2lwmc = plus2.logwmcc(domainSizes, predicateWeights)
  //        val minlwmc = min.logwmcc(domainSizes, predicateWeights)
  //        val logsum = logsumexp(plus1lwmc, plus2lwmc)
  //        logminusexp(logsum, minlwmc)
  //    }


  lazy val domains = plus1.domains union plus2.domains union min.domains

  lazy val evalOrder = {
    val plus1Order = plus1.evalOrder
    val plus2Order = plus2.evalOrder
    val minOrder = min.evalOrder
    plus1Order max plus2Order max minOrder
  }

  lazy val smooth = {
    val (plus1Smooth, plus1Vars) = plus1.smooth
    val (plus2Smooth, plus2Vars) = plus2.smooth
    val (minSmooth, minVars) = min.smooth
    val plus1Missing = removeSubsumed(plus2Vars union minVars).flatMap { _.minus(plus1Vars) }
    val plus2Missing = removeSubsumed(plus1Vars union minVars).flatMap { _.minus(plus2Vars) }
    val minMissing = removeSubsumed(plus1Vars union plus2Vars).flatMap { _.minus(minVars) }
    val plus1SmoothAll = plus1Smooth.smoothWith(plus1Missing)
    val plus2SmoothAll = plus2Smooth.smoothWith(plus2Missing)
    val minSmoothAll = minSmooth.smoothWith(minMissing)
    val thisSmooth = new InclusionExclusion(cnf, plus1SmoothAll, plus2SmoothAll, minSmoothAll, explanation)
    val plus1VarsAll = plus1Vars union plus1Missing
    val plus2VarsAll = plus2Vars union plus2Missing
    val minVarsAll = minVars union minMissing
    val bestOf2 = if (plus1VarsAll.size < plus2VarsAll.size) plus1VarsAll else plus2VarsAll
    val bestOf3 = if (minVarsAll.size < bestOf2.size) minVarsAll else bestOf2
    (thisSmooth, bestOf3)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new InclusionExclusion(cnf, plus1.condition(pos, neg), plus2.condition(pos, neg), min.condition(pos, neg), explanation)

  def ieSymbol = """$\begin{tikzpicture}[scale=0.08] \draw (0,0) circle (1.6cm); \draw (0:2cm) circle (1.6cm); \end{tikzpicture}$"""

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (n1, e1) = plus1.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (n2, e2) = plus2.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (n3, e3) = min.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + ieSymbol + """", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + ieSymbol + """", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + plus1.getName(nameSpace) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + getName(nameSpace) + " -> " + plus2.getName(nameSpace) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + getName(nameSpace) + " -> " + min.getName(nameSpace) + """ [""" + edgeLabel("-") + """];""" + "\n"
      } else {
        "  " + getName(nameSpace) + " -> " + "ie" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + plus1.getName(nameSpace) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + plus2.getName(nameSpace) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + min.getName(nameSpace) + """ [""" + edgeLabel("-") + """];""" + "\n"
      }
      val nodes = (myNodes + n1 + n2 + n3)
      val edges = (myEdges + e1 + e2 + e3)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + plus1.getName(nameSpace) +
      ", " + plus2.getName(nameSpace) +
      ", " + min.toString(nameSpace) + "\n" +
      "\n" +
      plus1.toString(nameSpace) +
      "\n" +
      plus2.toString(nameSpace) +
      "\n" +
      min.toString(nameSpace))
  }

}
