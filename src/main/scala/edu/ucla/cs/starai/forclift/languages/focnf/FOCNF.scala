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

package edu.ucla.cs.starai.forclift.languages.focnf

import scala.io._
import scala.math._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.formulas.visitor.Skolemization
import edu.ucla.cs.starai.forclift.formulas.visitor.Skolemizer
import edu.ucla.cs.starai.forclift.languages.StatRelModel

/** Default namespace for FOCNF predicates. */
class FOCNFNameSpace extends NameSpace[Any, String] {

  var lastUsedVar = -1;

  override protected def createName(obj: Any) = {
    obj match {
      case variable: Var => {
        lastUsedVar += 1
        val n = variable.asInstanceOf[Var].toString(lastUsedVar)
        n(0).toLower + n.substring(1, n.length)
      }
      case constant: Constant => {
        val n = constant.toString
        n.capitalize
      }
      case pred: Predicate => {
        val n = pred.toString
        n(0).toLower + n.substring(1, n.length)
      }
      case _ => obj.toString
    }
  }

}

class FOCNFSkolemNameSpace extends EFNameSpace {

  override def createName(f: Formula): String = {
    nbPredicates += 1
    "ef_" + nbPredicates + ""
  }
}

object FOCNFSyntax extends FormulaSyntax {
  override val kwName = "FO-CNF"
  override val kwNegation = "-"
  override val kwConjunction = "*"
  override val kwDisjunction = " "
  override val kwImplication = "*"
  override val kwImplicationRev = "*"
  override val kwEquivalence = "*"
  override val kwWildcardNegation = "*"
  override val kwExistential = "?"
  override val kwUniversal = "!"
  override val kwQuantSep = " "
  override val kwComment = "c "
  override val kwParentheses = "" :: "" :: Nil
}

//TODO Can be framed as a more general data structure for arbitrary formulas
case class FOCNF(
    val formulas: List[Formula] = List(),
    val predicates: Set[Predicate] = Set(),
    val domainSizes: DomainSizes = DomainSizes.empty,
    val predicateWeights: PredicateWeights = PredicateWeights.empty,
    val atomWeights: List[(Atom,Weights)] = List()
) extends StatRelModel {
  
  lazy val domains = predicates.flatMap{_.domains}.toSet
  
  def weightedCNF(skolemize:Boolean=true):WeightedCNF = {
    val focnf_sk = (if (skolemize) {
      this.skolemize
    } else {
      val formulas2 = formulas.map{formula =>
        // assumes NNF
        formula.existToDisj
      }
      copy(formulas=formulas2)
    })
    
    val focnf_aw = focnf_sk.atomWeightsToFormula
    
    val disjuncts = focnf_aw.formulas.flatMap{formula =>
      formula.implFree.nnf().cnf match {
        case cf:ConjFormula => cf.conjunctsToList
        case df:DisjFormula => List(df)
        case lf:LiteralFormula => List(lf)
        case f => throw new IllegalStateException(s"ERROR: CNF is not a conjunction of disjuncts: $f")
      }
    }
    
    val clauses = disjuncts.map{formula =>
      val (pos_lits, neg_lits) = formula.cnf match {
        case df:DisjFormula => df.disjunctsToList.foldLeft((Nil,Nil):(List[Atom],List[Atom])){(r,c) =>
          c match {
            case lit: LiteralFormula if  lit.sign => (lit.atom :: r._1, r._2)
            case lit: LiteralFormula if !lit.sign => (r._1, lit.atom :: r._2)
            case f => throw new IllegalStateException(s"ERROR: Clause in CNF is not a disjunction: $f")
          }
        }
        case lit: LiteralFormula if  lit.sign => (lit.atom :: Nil, Nil)
        case lit: LiteralFormula if !lit.sign => (Nil, lit.atom :: Nil)
        case f => throw new IllegalStateException(s"ERROR: Clause in CNF is not a disjunction: $f")
      }
      Clause(pos_lits, neg_lits)
    }
    val cnf = new CNF(clauses)
    val all_predicateWeights = focnf_aw.predicateWeights ++ predicates.filterNot(focnf_aw.predicateWeights.contains(_)).map { (_ -> Weights(1, 1)) }
    WeightedCNF(cnf,
                domainSizes,
                all_predicateWeights)
  }
  
  override def toString: String = {
    val domain_strings = domains.map{d=>
      val domainSize = domainSizes.getOrElse(d,
          throw new IllegalStateException(s"No domain size for domain $d")
      )
      s"d ${d.toString} ${domainSize.size} "+d.knownConstants.mkString(" ")
    }.toList
    
    val relation_strings = predicates.map("r "+_.toStringFull).toList
    
    val pred_weight_strings = predicateWeights.map{case (pred,ws) =>
      s"w $pred ${ws.posWDouble} ${ws.negWDouble}"
    }.toList
    
    val atom_weight_strings = atomWeights.map{case(atom,ws) =>
      s"w $atom ${ws.posWDouble} ${ws.negWDouble}"
    }.toList
    
    val formula_strings = formulas.map{formula =>
      val ns = new FOCNFNameSpace
      formula.toString(ns, FOCNFSyntax)
    }
    
    ("p fo-cnf" ::
    domain_strings :::
    relation_strings :::
    pred_weight_strings :::
    atom_weight_strings :::
    formula_strings :::
    Nil).mkString("\n")
  }
  
  def skolemize: FOCNF = {
    val exist_ns = new FOCNFSkolemNameSpace
    val skolemizer = Skolemizer()
    
    val (sk_formulas, sk_weights) = formulas.foldLeft(List[Formula](),PredicateWeights.empty){(r,formula) =>
      val sk_result = skolemizer.skolemize(formula, exist_ns)
      val (sk_formula, sk_formulas, sk_preds, sk_weights) = sk_result
      // Constraints can be ignored, not supported in FO-CNF
      (sk_formula :: sk_formulas.map{_._1} ::: r._1,
       sk_weights ++ r._2)
    }
    
    copy(formulas=sk_formulas,
         predicateWeights=predicateWeights++sk_weights)
  }
  
  def groundAtom(atom:Atom): List[Atom] = {
    if (atom.variables.isEmpty) {
      List(atom)
    } else {
      val v = atom.variables.head
      val domain = atom.domain(v)
      val constants = domain.constants(domainSizes)
      val groundedOnce = constants.map{c => atom.substitute{vv: Var => if (vv == v) c else vv}}
      groundedOnce.flatMap{groundAtom(_)}
    }
  }
  
  /**
   * Translate atom weights to weighted predicates and formulas.
   */
  def atomWeightsToFormula: FOCNF = {
    // Exhaustively add all groundings if one of the predicate weights is zero
    val atoms = atomWeights.map{case (atom,ws) => atom}.toSet
    val zero_pred = predicateWeights.filter{case (pred,ws) => ws.posWDouble == 0 || ws.negWDouble == 0}.toList
    val new_atomWeights = zero_pred.flatMap{case (pred,ws) =>
      val atom = pred((0 until pred.arity).map(i => new Var):_*)
      val ground_atoms = groundAtom(atom).toSet
      val new_atoms = ground_atoms -- atoms
      new_atoms.map((_,ws))
    }
    
    // Add all the atom weights as f <=> atom.
    val (new_formulas, new_weights, new_preds) = (atomWeights++new_atomWeights).foldLeft(List[Formula](),PredicateWeights.empty,List[Predicate]()){(r,c) =>
      val (atom,ws) = c  
      val default_ws = predicateWeights.get(atom.predicate) match {
        case Some(w) => w
        case None => Weights(1,1)
      }
      val pos_w = (if (default_ws.posWDouble == 0) ws.posWDouble else ws.posWDouble/default_ws.posWDouble)
      val neg_w = (if (default_ws.negWDouble == 0) ws.negWDouble else ws.negWDouble/default_ws.negWDouble)
      
      if (pos_w == 1 && neg_w == 1) {
        r
      } else {
        val new_pred = new Predicate(Symbol(s"f_${atom.predicate.toString}_"+atom.args.mkString("_")), 0, Nil)
        val new_formula = EqFormula(LiteralFormula(new_pred()), LiteralFormula(atom))
        val new_ws = Weights(pos_w,neg_w)
        (new_formula :: r._1,
         r._2 + (new_pred,new_ws),
         new_pred  :: r._3)
      }
    }
    
    val new_pred_ws = (predicateWeights -- zero_pred.map(_._1)) ++ new_weights
    
    copy(formulas=formulas ::: new_formulas,
        predicates=predicates ++ new_preds,
        predicateWeights=new_pred_ws,
        atomWeights=Nil)
  }
}
