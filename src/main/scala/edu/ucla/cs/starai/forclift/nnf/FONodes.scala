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

import scala.collection._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf.visitors.SafeSignLogDoubleWmc
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries

class IndependentPartialGroundingNode(val cnf: CNF, val child: NNFNode, val c: Constant, val ineqs: Set[Constant], val d: Domain, val explanation: String = "") extends NNFNode {

  lazy val smooth = {
    val (childSmoothed, childvars) = child.smooth
    val ungroundedChildVars = childvars.map { _.inverseSubstitution(c, ineqs, d) }
    (new IndependentPartialGroundingNode(cnf, childSmoothed, c, ineqs, d, explanation), ungroundedChildVars)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new IndependentPartialGroundingNode(cnf, child.condition(pos, neg), c, ineqs, d, explanation)

  def size = child.size + 1

  lazy val domains = child.domains + d

  lazy val evalOrder = child.evalOrder

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false,
    depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val subscript = ((c.toString + """ \in """ + d) :: ineqs.map { c.toString + """ \neq """ + _.toString }.toList).mkString(""" \land """)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "exp" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val childlwmc = wmcVisitor.visit(child.smooth._1, (domainSizes, predicateWeights))
        "  " + getName(nameSpace) + " -> " + "exp" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "exp" + getName(nameSpace) + " -> " + child.getName(nameSpace) + """ [""" + edgeLabel(" $ " + childlwmc.exp + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + child.getName(nameSpace) + " ^ |" + d + "|" + ineqs.map { """X \neq """ + _.toString }.mkString(" , ") + "\n" +
      "\n" +
      child.toString(nameSpace))
  }

}

class CountingNode(val cnf: CNF, val child: NNFNode,
  val domain: Domain, val subdomain: SubDomain,
  val explanation: String = "") extends NNFNode {

  def size = child.size + 1

  def excludedConstants = subdomain.excludedConstants

  lazy val domains = (child.domains - subdomain - subdomain.complement) + domain

  lazy val evalOrder = child.evalOrder + 1

  def smooth = {
    val (childSmooth, childVars) = child.smooth
    // this is fine, but does not mean the result will be non-overlapping
    // two catoms might overlap but not one subsumes the other
    val countedSubdomainParents = removeSubsumed(childVars.map { _.reverseDomainSplitting(domain, subdomain) })
    val disjCounted = makeDisjoint(countedSubdomainParents.toList)
    val childMissing = disjCounted.flatMap { _.minus(childVars) }
    val childSmoothAll = childSmooth.smoothWith(childMissing.toSet)
    val thisSmoothed = new CountingNode(cnf, childSmoothAll, domain, subdomain, explanation)
    (thisSmoothed, countedSubdomainParents)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new CountingNode(cnf, child.condition(pos, neg), domain, subdomain, explanation)

  def minDomainSizeString = "|" + subdomain + "| = 0"
  def maxDomainSizeString = "|" + domain + "|"

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "count" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"

      } else {
        "  " + getName(nameSpace) + " -> " + "count" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "count" + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = count " + subdomain + " from " + domain + " " + child.getName(nameSpace) + "\n" +
      "\n" +
      child.toString(nameSpace))
  }

}

class DomainRecursionNode(val cnf: CNF, val mixedChild: IndependentPartialGroundingNode, val groundChild: NNFNode, val c: Constant, val ineqs: Set[Constant], val domain: Domain,
  val explanation: String = "") extends NNFNode {

  def size = mixedChild.size + groundChild.size + 1

  // assumptions to speed up inference
  require(mixedChild.child.cnf.isGround)


  lazy val domains = mixedChild.domains union groundChild.domains + domain

  lazy val evalOrder = mixedChild.evalOrder // assume constant eval

  def smooth = {
    val (mixedChildSmoothed, mixedChildVars) = mixedChild.smooth
    val (groundChildSmoothed, groundChildVars) = groundChild.smooth
    val ungroundedMixedChildvars = mixedChildVars.map { _.inverseSubstitution(c, ineqs, domain) }
    val ungroundedGroundChildVars = groundChildVars.map { _.inverseSubstitution(c, ineqs, domain) }
    val allVars = ungroundedMixedChildvars ++ ungroundedGroundChildVars
    (new DomainRecursionNode(cnf, mixedChildSmoothed, groundChildSmoothed, c, ineqs, domain, explanation), allVars)
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = new DomainRecursionNode(
    cnf,
    mixedChild.condition(pos, neg),
    groundChild.condition(pos, neg),
    c, ineqs, domain, explanation)

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (n1, e1) = mixedChild.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (n2, e2) = groundChild.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + mixedChild.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + groundChild.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val groundChildWmc = wmcVisitor.visit(groundChild.smooth._1,(domainSizes, predicateWeights))
        val mixedChildWmc = wmcVisitor.visit(mixedChild.smooth._1,(domainSizes - domain, predicateWeights))
        "  " + getName(nameSpace) + " -> " + "domainrec" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.getName(nameSpace) + """ [""" + edgeLabel(" $ " + mixedChildWmc.exp + " $ ") + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + groundChild.getName(nameSpace) + """ [""" + edgeLabel(" $ " + groundChildWmc.exp + " $ ") + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      }
      val nodes = (myNodes + n1 + n2)
      val edges = (myEdges + e1 + e2)
      (nodes, edges)
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (super.toString(nameSpace) +
      getName(nameSpace) + " = domainrec " + c + " from " + domain + " " + mixedChild.getName(nameSpace) + " " + groundChild.getName(nameSpace) + "\n" +
      "\n" +
      mixedChild.toString(nameSpace) +
      "\n" +
      groundChild.toString(nameSpace))
  }

}
