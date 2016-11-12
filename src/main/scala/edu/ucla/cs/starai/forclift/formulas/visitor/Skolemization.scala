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

package edu.ucla.cs.starai.forclift.formulas.visitor

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.inference.Weights
import edu.ucla.cs.starai.forclift.constraints.IneqConstr
import edu.ucla.cs.starai.forclift.constraints.ElemConstr

object Skolemizer {
  def apply(): Skolemization = {
    new Skolemization
  }
}

protected class Skolemization extends FormulaVisitor[(EFNameSpace,Constraints), (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate])] {
  
  /** Transform the given list of formulas.
    *
    * @param  formula
    * @param  ef_ns
    * 		  Namespace for existential formulas
    * @param  constraints
    *         Constraints that applies to the given formula
    * @return Tuple with:
    *         - Original formula, qunatifier free
    *         - List of tuple of additional quantifier free formulas and additional constraints
    *         - List of Skolem and Tseitin predicates
    *         - New Predicate weights for the Skolem and Tseitin predicates
    */
  def skolemize(formula: Formula, ef_ns: EFNameSpace, constraints: Constraints = Constraints.empty): (Formula,List[(Formula,Constraints)],List[Predicate],PredicateWeights) = {
    val context = (ef_ns, constraints)
    val (new_formula, new_formulas, pred_s, pred_z) = visit(formula, context)
    
    val predw_s = pred_s.foldLeft(PredicateWeights.empty)((r, c) => r + (c -> Weights(1, -1)))
    val predw_sz = pred_z.foldLeft(predw_s)((r, c) => r + (c -> Weights(1, 1)))
    
    (new_formula,
     new_formulas,
     pred_s ::: pred_z,
     predw_sz)
  }
  
  protected def visitTrueFormula(formula: TrueFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    (formula, Nil, Nil, Nil)
  }
  
  protected def visitFalseFormula(formula: FalseFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    (formula, Nil, Nil, Nil)
  }
  
  protected def visitLiteralFormula(formula: LiteralFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    (formula, Nil, Nil, Nil)
  }
  
  protected def visitNegFormula(formula: NegFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (nf, nfs, sp, zp) = visit(formula.formula, context)
    if (nf == formula.formula) {
      (formula, nfs, sp, zp)
    } else {
      (NegFormula(nf), nfs, sp, zp)
    }
  }
  
  protected def visitConjFormula(formula: ConjFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (lf, lfs, lsp, lzp) = visit(formula.left, context)
    val (rf, rfs, rsp, rzp) = visit(formula.right, context)
    if (lf == formula.left && rf == formula.right) {
      (formula, lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    } else {
      (ConjFormula(lf, rf), lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    }
  }
  
  protected def visitDisjFormula(formula: DisjFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (lf, lfs, lsp, lzp) = visit(formula.left, context)
    val (rf, rfs, rsp, rzp) = visit(formula.right, context)
    if (lf == formula.left && rf == formula.right) {
      (formula, lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    } else {
      (DisjFormula(lf, rf), lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    }
  }
  
  protected def visitImplFormula(formula: ImplFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (lf, lfs, lsp, lzp) = visit(formula.condition, context)
    val (rf, rfs, rsp, rzp) = visit(formula.consequent, context)
    if (lf == formula.condition && rf == formula.consequent) {
      (formula, lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    } else {
      (ImplFormula(lf, rf), lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    }
  }
  
  protected def visitEqFormula(formula: EqFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (lf, lfs, lsp, lzp) = visit(formula.left, context)
    val (rf, rfs, rsp, rzp) = visit(formula.right, context)
    if (lf == formula.left && rf == formula.right) {
      (formula, lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    } else {
      (EqFormula(lf, rf), lfs ::: rfs, lsp ::: rsp, lzp ::: rzp)
    }
  }
  
  /**
   * Apply first-order Skolemization.
   *
   * {{{
   * ... ∃y p(x,y) ...
   * }}}
   * to
   * {{{
   * ... z(x) ...
   * z(x)    v !p(x,y)
   * z(x)    v s(x)
   * !p(x,y) v s(x)
   * }}}
   * 
   * If any of the variables in the z or s atom are bound by a domain
   * constraint, extra rules are added for the complementary domain.
   * This guarantees that no compensation (-1) is added that is not linked
   * to a quantifier.
   * 
   * {{{
   * ... ∃y p(x,y) ..., x ∈ D
   * }}}
   * adds additionally
   * {{{
   * z(x), x ∈ D_comp
   * s(x), x ∈ D_comp
   * }}}
   * 
   */
  protected def visitExistFormula(formula: ExistFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (ef_ns, constraints) = context
    
    // Deepest quantifier first
    val (pf, sk_formulas, sp, zp) = visit(formula.formula, context)
    // @post: p is quantifier free
    
    val other_vars = pf.atoms.flatMap { _.variables }.filterNot(formula.vars.contains(_)).toList
    val domains = other_vars.map { v =>
      val atom = formula.atoms.find { _.args.contains(v) }.get
      atom.domain(v)
    }
    
    val prefix = ef_ns.getName(formula)
    val z  = new Predicate(Symbol("z" + prefix), other_vars.size, domains)
    val zf = LiteralFormula(z(other_vars: _*))
    val s  = new Predicate(Symbol("s" + prefix), other_vars.size, domains)
    val sf = LiteralFormula(s(other_vars: _*))
    
    val pf_neg = NegFormula(pf)
    
    val new_sk_formulas = (DisjFormula(zf,     pf_neg), Constraints.empty) ::
    				      (DisjFormula(zf,     sf),     Constraints.empty) ::
    				      (DisjFormula(pf_neg, sf),     Constraints.empty) :: Nil

    // Alternative subdomains
    val alt_sk_formulas = constraints.elemConstrs.
      filter{case (curvar, curdom) => other_vars.contains(curvar)}.toList.
      flatMap{case (curvar, curdom) =>
        val new_constraints = Constraints(IneqConstr(), ElemConstr((curvar,curdom.complement)))
        (sf, new_constraints) ::
        (zf, new_constraints) :: Nil
      }
    				  
    (zf, sk_formulas ::: new_sk_formulas ::: alt_sk_formulas, s :: sp, z :: zp)
  }
  
  /**
   * Apply first-order Skolemization.
   *
   * {{{
   * ... ∀y p(x,y) ...
   * ... !(∃y !p(x,y)) ...
   * }}}
   * to
   * {{{
   * ... !z(x) ...
   * z(x)   v p(x,y)
   * z(x)   v s(x)
   * p(x,y) v s(x)
   * }}}
   * 
   * @note if outer level, the quantifier can be ignored (should be done
   *       before skolemization).
   */
  protected def visitForallFormula(formula: ForallFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    val (ef_ns, constraints) = context
    
    // Deepest quantifier first
    val (pf, sk_formulas, sp, zp) = visit(formula.formula, context)
    // @post: p is quantifier free 
    
    val other_vars = pf.atoms.flatMap { _.variables }.filterNot(formula.vars.contains(_)).toList
    val domains = other_vars.map { v =>
      val atom = formula.atoms.find { _.args.contains(v) }.get
      atom.domain(v)
    }
    
    val prefix = ef_ns.getName(formula)
    val z  = new Predicate(Symbol("z" + prefix), other_vars.size, domains)
    val zf = LiteralFormula(z(other_vars: _*))
    val s  = new Predicate(Symbol("s" + prefix), other_vars.size, domains)
    val sf = LiteralFormula(s(other_vars: _*))
    
    val new_sk_formulas = (DisjFormula(zf, pf), Constraints.empty) ::
    				      (DisjFormula(zf, sf), Constraints.empty) ::
    				      (DisjFormula(pf, sf), Constraints.empty) :: Nil
    
    // Alternative subdomains
    val alt_sk_formulas = constraints.elemConstrs.
      filter{case (curvar, curdom) => other_vars.contains(curvar)}.toList.
      flatMap{case (curvar, curdom) =>
        val new_constraints = Constraints(IneqConstr(), ElemConstr((curvar,curdom.complement)))
        (sf, new_constraints) ::
        (zf, new_constraints) :: Nil
      }
    				      
    (NegFormula(zf), sk_formulas ::: new_sk_formulas ::: alt_sk_formulas, s :: sp, z :: zp)
  }
  
  protected def visitEmptyFormula(formula: EmptyFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
	(formula, Nil, Nil, Nil)
  }
  
  protected def visitWildNegFormula(formula: WildNegFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
	throw new IllegalStateException("Skolemization expects a formula without wildcard negation (*)")
    (formula, Nil, Nil, Nil)
  }

  protected def visitAtomicExpFormula(formula: AtomicExpFormula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
    throw new IllegalStateException("Skolemization expects a formula without function expansion (+)")
    (formula, Nil, Nil, Nil)
  }
  
  protected def visitIndivisibleSubformula(formula: IndivisibleSubformula, context: (EFNameSpace,Constraints)): (Formula,List[(Formula,Constraints)],List[Predicate],List[Predicate]) = {
	val (nf,nfs, sp, zp) = visit(formula.formula, context)
	if (nf == formula.formula) {
	  (formula, nfs, sp, zp)
	} else {
  	  (IndivisibleSubformula(nf), nfs, sp, zp)
	}
  }
}
