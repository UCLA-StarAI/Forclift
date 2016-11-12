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

import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.languages.mln._

class ProbLogFormat(
  val mln: MLN, 
  verbose: Boolean = false) {
  
  def startLowerCase(name:String) = {
    name.toString.substring(0,1).toLowerCase+name.toString.substring(1)
  }
  
  def startUpperCase(name:String) = {
    name.toString.substring(0,1).toUpperCase+name.toString.substring(1)
  }
  
  lazy val (literals,formulas) = mln.wformulas.partition {_.formula.isLiteral}
  
  lazy val domainStrings = {
//    mln.domainSizes
    mln.domains.flatMap {domain =>
      val constants = domain.constants(mln.domainSizes)
      constants.map{cnst =>
        startLowerCase(domain.name)+"("+startLowerCase(cnst.toString)+")."
      }
    }.toList
  }
  
  lazy val predicateStrings = {
    val explicit_predicates = literals.flatMap{lit => lit.formula.atoms}.map{atom => atom.predicate}.toSet
    val default_predicates = mln.predicates diff explicit_predicates
    
    val preds1 = default_predicates.map {pred =>
      val nameSpace = new VarNameSpace
      val atom = pred((0 until pred.arity).map{i=>new Var()}: _*)
      val domStrings = atom.variables.map{v =>
        val domain = atom.domain(v)
        startLowerCase(domain.name)+"("+nameSpace.getName(v)+")"
      }
      val domString = if (domStrings.size == 0) {
        ""
      } else {
        " :- "+domStrings.mkString(", ")
      }
      "0.5::"+startLowerCase(atom.toString(nameSpace))+domString+"."
    }.toList
    
    val preds2 = literals.map{wf => 
      val nameSpace = new VarNameSpace
      val atom = wf.formula match {
        case f:LiteralFormula => {
          f.atom
        }
        case _ => {
           throw new IllegalStateException("Expected a literal.")
        }
      }
      val domStrings = atom.variables.map{v =>
        val domain = atom.domain(v)
        startLowerCase(domain.name)+"("+nameSpace.getName(v)+")"
      }
      val domString = if (domStrings.size == 0) {
        ""
      } else {
        " :- "+domStrings.mkString(", ")
      }
      val prob = Math.exp(wf.weight)/(Math.exp(wf.weight)+1)
      prob+"::"+startLowerCase(atom.toString(nameSpace))+domString+"."
    }
    
    preds1 ::: preds2
  }
  
  lazy val formulaStrings = {
   val wf_ns = new WFNameSpace
   val exist_ns = new EFNameSpace
   
   formulas.flatMap {wf =>
     wf.toWeightedCNFWithoutSplitting(wf_ns, exist_ns, skolemize=false) match {
       case (wcnf,Some(atom)) => {
         val varNameSpace = new VarNameSpace
         val w = Math.log(wcnf.predicateWeights(atom.predicate).posWDouble)
         val prob = Math.exp(w)/(Math.exp(w)+1)
         val domStrings = atom.variables.map{v =>
           val domain = atom.domain(v)
           startLowerCase(domain.name)+"("+varNameSpace.getName(v)+")"
         }
         val domString = if (domStrings.size == 0) {
           ""
         } else {
           " :- "+domStrings.mkString(", ")
         }
         val fact = prob.toString+"::"+atom.toString(varNameSpace)+domString+"."
         
         val cnf = wcnf.cnf.map{clause =>
           val nameSpace = new VarNameSpace
           val posLits = clause.posLits
           val negLits = clause.negLits
           val literalStr = (negLits.map {lit => startLowerCase(lit.toString(nameSpace)) } union posLits.map {lit => "\\+"+startLowerCase(lit.toString(nameSpace)) }).mkString(", ")
//           List(literalStr, clause.constrs.toString(nameSpace)).filter { _.nonEmpty }.mkString(", ")
           val varsDomOnlyInNeg = (posLits.flatMap{lit =>
             lit.variables.map{v => (v,lit.domain(v))}
           } diff negLits.flatMap{lit =>
             lit.variables.map{v => (v,lit.domain(v))}
           }).toSet
           val domStrings = if (varsDomOnlyInNeg.size == 0) {""} else {
             varsDomOnlyInNeg.map{case (v,d) =>
               startLowerCase(d.name)+"("+nameSpace.getName(v)+")"            
             }.mkString("",", ",", ")
           }
           "ev :- "+domStrings+literalStr+"."
         }.toList
         
         cnf ::: fact :: Nil
       }
       case (wcnf,None) => {
         wcnf.toString
       }
     }
     
   }.toList 
  }
  
  lazy val usageStrings = {
    """
    |
    |% Compute probabilities by adding query statements:
    |% > query(predicate(constant)).
    |%
    |% Compute marginals by (1) computing the probability of evidence:
    |% > %evidence(ev,false).  % comment out evidence first
    |% > query(ev).
    |% and (2) denormalizing the result:
    |% Multiply the result of query(ev) by (1+exp(w)) for every ground fact w::fact.
    |""".stripMargin ::
    Nil
  }
  
  lazy val evidenceStrings = {
    "evidence(ev,false)." :: Nil
  }
  
  /**
   * Returns a string containing a ProbLog model equivalent to the given MLN
   */
  override def toString = {
    ("% Translation to ProbLog from Markov Logic using WFOMC\n" ::
      domainStrings :::
      predicateStrings :::
      formulaStrings :::
      evidenceStrings :::
      usageStrings :::
      "" ::
      Nil).mkString("\n").replace("{","").replace("}","")
  }
}
