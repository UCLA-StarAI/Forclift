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

package edu.ucla.cs.starai.forclift

import collection._
import util._

trait FormulaSyntax {
  val kwName = "WFOMC"
  val kwNegation = "!"
  val kwConjunction = "&"
  val kwDisjunction = "|"
  val kwImplication = "=>"
  val kwImplicationRev = "<="
  val kwEquivalence = "<=>"
  val kwWildcardNegation = "*"
  val kwExistential = "EXIST"
  val kwUniversal = "FORALL"
  val kwQuantSep = ""
  val kwComment = "#"
  val kwParentheses = "(" :: ")" :: Nil 
}

object FormulaSyntaxDefault extends FormulaSyntax {
}

object AlchemySyntax extends FormulaSyntax {
  override val kwName = "Alchemy"
  override val kwNegation = "!"
  override val kwConjunction = "^"
  override val kwDisjunction = "v"
  override val kwImplication = "=>"
  override val kwImplicationRev = "<="
  override val kwEquivalence = "<=>"
  override val kwWildcardNegation = "*"
  override val kwExistential = "EXIST"
  override val kwUniversal = "FORALL"
  override val kwQuantSep = ""
  override val kwComment = "//"
}

class EFNameSpace extends NameSpace[Formula, String] {

  var nbPredicates = 0

  override def createName(f: Formula): String = {
    nbPredicates += 1
    "ef_{" + nbPredicates + "}"
  }
}

sealed trait Formula {

  def substitute(substitution: Var.Substitution): Formula
  def needsParenthesis: Boolean = true
  def isLiteral: Boolean = false

  /**
   * Removes all implications and equivalences in the theory.
   * {{{
   * a <=> b === a => b ^ b => a
   * a => b  === !a v b
   * }}}
   */
  def implFree: Formula

  /**
   * Creates a NNF theory that has only atomic negation.
   *
   * @note    Precondition:
   *          The formula is implication and equivalence free
   */
  def nnf(negate: Boolean = false): Formula

  /**
   * Negate the formula.
   *
   * negated <-> non-negated
   */
  def negate = nnf(true)

  /**
   * Transforms the formula into Prenex Normal Form (PNF) by moving all
   * quantifiers outwards.
   *
   * A theory in prenex normal form consists of formulas
   * Q_1 x_1, ..., Q_n x_n ϕ where each Q_i is either a universal or
   * existential quantifier, and ϕ is quantifier-free.
   *
   * {{{
   * P ⋀ (∀x Q(x)) ⟺  ∀x (P ⋀ Q(x))
   * P ⋁ (∀x Q(x)) ⟺  ∀x (P ⋁ Q(x))
   * P ⋁ (∃x Q(x)) ⟺  ∃x (P ⋁ Q(x))
   * P ⋁ (∃x Q(x)) ⟺  ∃x (P ⋁ Q(x))
   * }}}
   *
   * @note   Precondition:
   *         The formula is in nnf.
   */
  def pnf: Formula

  /**
   * Create an equivalent CNF theory
   *
   * @note    Precondition:
   *          Formula is implication free and in NNF
   */
  def cnf: Formula

  /**
   * Flatten indivisible formulas (consider them as regular parentheses).
   * (Alchemy specific syntax)
   */
  def rmSquareBrackets: Formula

  /**
   * Expand wildcard negation (*).
   * Expand variable to constants (+).
   * (Alchemy specific syntax)
   *
   * @return A list to represent all instances covered by the given template.
   *         In case of Alchemy all formulas in this list thus should get the
   *         same weight because they originate from the same template.
   */
  def expandAlchemy: List[Formula]

  /**
   * Expand existential quantifier (exists) in a naive way by replacing it
   * with a disjunction.
   */
  def existToDisj: Formula

  /** Normalize the formula tree to be left-branching. */
  def normalize: Formula

  def predicates: Set[Predicate]
  def atoms: Set[Atom]
  def boundVariables: Set[Var] = Set()
  lazy val variables: Set[Var] = {
    atoms.flatMap(_.variables)
  }

  /**
   * Simplify the formula given Boolean values for all the atoms in the
   * formula.
   */
  def evaluate(values: Atom => Option[Boolean]): Formula

  def standardizeApart: Formula = {
    val map = new mutable.HashMap[Var, Var]()
    substitute(v => {
      assume(!map.values.exists(_ == v))
      map.getOrElseUpdate(v, new Var)
    })
  }
  
  override def toString = toString(ToStringNameSpace, FormulaSyntaxDefault)
  def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax): String
  def toStringAlchemy(nameSpace: NameSpace[Any, String]) = toString(nameSpace, AlchemySyntax)
}

/** True is a formula */
sealed case class TrueFormula() extends Formula {
  override def toString(nameSpace: NameSpace[Any, String], syntax: FormulaSyntax) = "true"
  override def needsParenthesis = false
  override def isLiteral = true

  def substitute(substitution: Var.Substitution) = this
  def implFree = this
  def nnf(negate: Boolean = false) = this
  def pnf = this
  def cnf = this
  def rmSquareBrackets = this
  def expandAlchemy = List(this)
  def existToDisj = this
  def normalize = this
  def predicates = Set()
  def atoms = Set()

  def evaluate(values: Atom => Option[Boolean]): Formula = this
}

/** False is a formula */
sealed case class FalseFormula() extends Formula {
  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = "false"
  override def needsParenthesis = false
  override def isLiteral = true

  def substitute(substitution: Var.Substitution) = this
  def implFree = this
  def nnf(negate: Boolean = false) = this
  def pnf = this
  def cnf = this
  def rmSquareBrackets = this
  def expandAlchemy = List(this)
  def existToDisj = this
  def normalize = this
  def predicates = Set()
  def atoms = Set()

  def evaluate(values: Atom => Option[Boolean]): Formula = this
}

/** A literal is a formula. */
sealed case class LiteralFormula(
  atom: Atom,
  sign: Boolean = true) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    (if (!sign) syntax.kwNegation else "") + atom.toString(nameSpace)
  }
  override def needsParenthesis = false
  override def isLiteral = true

  def substitute(substitution: Var.Substitution) = {
    atom.substitute(substitution) match {
      case `atom` => this
      case a => this.copy(atom = a)
    }
  }

  def implFree = this

  def nnf(negate: Boolean = false) = {
    if (negate) {
      this.copy(sign = !sign)
    } else {
      this
    }
  }

  def pnf = this
  def cnf = this
  def rmSquareBrackets = this
  def expandAlchemy = List(this)
  def existToDisj = this

  def normalize = this

  def predicates = Set(atom.predicate)
  def atoms = atom.predicate match {
    case Predicate.eq => Set()
    case _ => Set(atom)
  }

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    values(atom) match {
      case None => this
      case Some(true) => if (sign) TrueFormula() else FalseFormula()
      case Some(false) => if (sign) FalseFormula() else TrueFormula()
    }
  }
}

/** A negated formula is a formula. */
sealed case class NegFormula(
  formula: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    syntax.kwNegation + (if (formula.needsParenthesis) syntax.kwParentheses(0) + formula.toString(nameSpace, syntax) + syntax.kwParentheses(1) else formula.toString(nameSpace, syntax))
  }
  override def needsParenthesis = false
  //override def isLiteral = formula match {
  //case f:AtomicFormula => true
  //case _ => false
  //}

  def substitute(substitution: Var.Substitution) = {
    new NegFormula(formula.substitute(substitution))
  }

  def implFree = formula.implFree match {
    case `formula` => this
    case fn => NegFormula(fn)
  }

  def nnf(negate: Boolean = false) = {
    formula.nnf(!negate)
  }

  def pnf = this
  def cnf = this

  def rmSquareBrackets = formula.rmSquareBrackets match {
    case `formula` => this
    case nf => NegFormula(nf)
  }

  def expandAlchemy = formula.expandAlchemy.map { f => NegFormula(f) }

  def existToDisj = {
    formula match {
      case f: ExistFormula => NegFormula(f.formula.existToDisj)
      case f => NegFormula(f.existToDisj)
    }
  }

  def normalize = this

  def predicates = formula.predicates
  def atoms = formula.atoms
  override def boundVariables = formula.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    formula.evaluate(values) match {
      case f: TrueFormula => FalseFormula()
      case f: FalseFormula => TrueFormula()
      case `formula` => this
      case f => NegFormula(f)
    }
  }
}

/** A conjunction of formulas is a formula. */
sealed case class ConjFormula(
  left: Formula,
  right: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    syntax.kwParentheses(0) + left.toString(nameSpace, syntax) + " " + syntax.kwConjunction + " " + right.toString(nameSpace, syntax) + syntax.kwParentheses(1)
  }
  override def needsParenthesis = false

  def substitute(substitution: Var.Substitution) = {
    new ConjFormula(left.substitute(substitution), right.substitute(substitution))
  }

  def implFree = ConjFormula(left.implFree, right.implFree)

  def nnf(negate: Boolean = false) = {
    if (negate) {
      DisjFormula(left.nnf(true), right.nnf(true))
    } else {
        (left.nnf(false), right.nnf(false)) match {
          case (`left`, `right`) => this
          case (l, r) => ConjFormula(l, r)
        }
    }
  }

  def pnf = {
    (left.pnf, right.pnf) match {
      case (l, r: ForallFormula) => r.copy(formula = ConjFormula(l, r.formula)).pnf
      case (l, r: ExistFormula) => r.copy(formula = ConjFormula(l, r.formula)).pnf
      case (l: ForallFormula, r) => l.copy(formula = ConjFormula(l.formula, r)).pnf
      case (l: ExistFormula, r) => l.copy(formula = ConjFormula(l.formula, r)).pnf
      case (`left`, `right`) => this
      case (l, r) => ConjFormula(l, r)
    }
  }

  def cnf = ConjFormula(left.cnf, right.cnf)

  def rmSquareBrackets = (left.rmSquareBrackets, right.rmSquareBrackets) match {
    case (`left`, `right`) => this
    case (l, r) => ConjFormula(l, r)
  }

  def expandAlchemy = {
    val l = left.expandAlchemy
    val r = right.expandAlchemy
    l.flatMap { lf =>
      r.map { rf =>
        ConjFormula(lf, rf)
      }
    }
  }

  def existToDisj = {
    ConjFormula(left.existToDisj, right.existToDisj)
  }

  //def splitClauses: List[Formula] = conjunctsToList

  /** Recursively join the conjuctions making up a sequence */
  def conjunctsToList: List[Formula] = {
    (left, right) match {
      case (l: ConjFormula, r: ConjFormula) => l.conjunctsToList ::: r.conjunctsToList
      case (l: ConjFormula, r) => l.conjunctsToList ::: List(r)
      case (l, r: ConjFormula) => l :: r.conjunctsToList
      case (l, r) => l :: r :: Nil
    }
  }

  def normalize = {
    right match {
      case cf: ConjFormula => ConjFormula(ConjFormula(left.normalize, cf.left.normalize), cf.right.normalize)
      case _ => this
    }
  }

  def predicates = left.predicates union right.predicates
  def atoms = left.atoms union right.atoms
  override def boundVariables = left.boundVariables ++ right.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    (left.evaluate(values), right.evaluate(values)) match {
      case (f: FalseFormula, _) => f
      case (_, f: FalseFormula) => f
      case (f: TrueFormula, fr) => fr
      case (fl, f: TrueFormula) => fl
      case (fl, fr) => ConjFormula(fl, fr)
    }
  }
}

/** A disjunction of formulas is a formula. */
sealed case class DisjFormula(
  left: Formula,
  right: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    syntax.kwParentheses(0) + left.toString(nameSpace, syntax) + " " + syntax.kwDisjunction + " " + right.toString(nameSpace, syntax) + syntax.kwParentheses(1)
  }
  override def needsParenthesis = false

  def substitute(substitution: Var.Substitution) = {
    new DisjFormula(left.substitute(substitution), right.substitute(substitution))
  }

  def implFree = DisjFormula(left.implFree, right.implFree)

  def nnf(negate: Boolean = false) = {
    if (negate) {
      ConjFormula(left.nnf(true), right.nnf(true))
    } else {
        (left.nnf(false), right.nnf(false)) match {
          case (`left`, `right`) => this
          case (l, r) => DisjFormula(l, r)
        }
    }
  }

  /**
   * Distribute over conjunction
   *
   * @note    Precondition:
   *          All disjuncts are in CNF
   * @return  Compute CNF for disjunction
   */
  def distr: Formula = {
    (left, right) match {
      case (l: ConjFormula, r) => ConjFormula(DisjFormula(l.left, r).distr, DisjFormula(l.right, r).distr)
      case (l, r: ConjFormula) => ConjFormula(DisjFormula(l, r.left).distr, DisjFormula(l, r.right).distr)
      case (l, r) => this
    }
  }

  def pnf = {
    (left.pnf, right.pnf) match {
      case (l, r: ForallFormula) => r.copy(formula = DisjFormula(l, r.formula)).pnf
      case (l, r: ExistFormula) => r.copy(formula = DisjFormula(l, r.formula)).pnf
      case (l: ForallFormula, r) => l.copy(formula = DisjFormula(l.formula, r)).pnf
      case (l: ExistFormula, r) => l.copy(formula = DisjFormula(l.formula, r)).pnf
      case (`left`, `right`) => this
      case (l, r) => DisjFormula(l, r)
    }
  }

  def cnf = DisjFormula(left.cnf, right.cnf).distr

  def rmSquareBrackets = (left.rmSquareBrackets, right.rmSquareBrackets) match {
    case (`left`, `right`) => this
    case (l, r) => DisjFormula(l, r)
  }

  def expandAlchemy = {
    val l = left.expandAlchemy
    val r = right.expandAlchemy
    l.flatMap { lf =>
      r.map { rf =>
        DisjFormula(lf, rf)
      }
    }
  }

  def existToDisj = {
    DisjFormula(left.existToDisj, right.existToDisj)
  }

  /** Recursively join the disjunctions making up a sequence */
  def disjunctsToList: List[Formula] = {
    (left, right) match {
      case (l: DisjFormula, r: DisjFormula) => l.disjunctsToList ::: r.disjunctsToList
      case (l: DisjFormula, r) => l.disjunctsToList ::: List(r)
      case (l, r: DisjFormula) => l :: r.disjunctsToList
      case (l, r) => l :: r :: Nil
    }
  }

  def normalize = {
    right match {
      case cf: DisjFormula => DisjFormula(DisjFormula(left.normalize, cf.left.normalize), cf.right.normalize)
      case _ => this
    }
  }

  def predicates = left.predicates union right.predicates
  def atoms = left.atoms union right.atoms
  override def boundVariables = left.boundVariables ++ right.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    (left.evaluate(values), right.evaluate(values)) match {
      case (f: TrueFormula, _) => f
      case (_, f: TrueFormula) => f
      case (f: FalseFormula, fr) => fr
      case (fl, f: FalseFormula) => fl
      case (fl, fr) => ConjFormula(fl, fr)
    }
  }
}

/** An implication is a formula. */
sealed case class ImplFormula(
  condition: Formula,
  consequent: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    (if (condition.needsParenthesis) syntax.kwParentheses(0) + condition.toString(nameSpace, syntax) + syntax.kwParentheses(1)
    else condition.toString(nameSpace, syntax)) + " " + syntax.kwImplication + " " +
      (if (consequent.needsParenthesis) syntax.kwParentheses(0) + consequent.toString(nameSpace, syntax) + syntax.kwParentheses(1)
      else consequent.toString(nameSpace, syntax))
  }

  def substitute(substitution: Var.Substitution) = {
    new ImplFormula(condition.substitute(substitution), consequent.substitute(substitution))
  }

  /** Returns !condition v consequent */
  def implFree = DisjFormula(NegFormula(condition.implFree), consequent.implFree)

  def nnf(negate: Boolean = false) = {
    assume(false, "NNF expects an implication free formula")
    new EmptyFormula
  }

  def pnf = {
    assume(false, "PNF expects an implication free formula")
    new EmptyFormula
  }

  def cnf = {
    assume(false, "CNF expects an implication free formula")
    new EmptyFormula
  }

  def rmSquareBrackets = (condition.rmSquareBrackets, consequent.rmSquareBrackets) match {
    case (`condition`, `consequent`) => this
    case (ncond, ncons) => ImplFormula(ncond, ncons)
  }

  def expandAlchemy = {
    var cond = condition.expandAlchemy
    var cons = consequent.expandAlchemy
    cond.flatMap { condf =>
      cons.map { consf =>
        ImplFormula(condf, consf)
      }
    }
  }

  def existToDisj = {
    ImplFormula(condition.existToDisj, consequent.existToDisj)
  }

  def normalize = {
    consequent match {
      case cf: ImplFormula => ImplFormula(ImplFormula(condition.normalize, cf.condition.normalize), cf.consequent.normalize)
      case _ => this
    }
  }

  def predicates = condition.predicates union consequent.predicates
  def atoms = condition.atoms union consequent.atoms
  override def boundVariables = condition.boundVariables ++ consequent.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    (condition.evaluate(values), consequent.evaluate(values)) match {
      case (f: FalseFormula, _) => TrueFormula()
      case (_, f: TrueFormula) => f
      case (fl: TrueFormula, fr: FalseFormula) => fr
      case (fl, fr) => ImplFormula(fl, fr)
    }
  }
}

/** An equivalence is a formula. */
sealed case class EqFormula(
  left: Formula,
  right: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    (if (left.needsParenthesis) syntax.kwParentheses(0) + left.toString(nameSpace, syntax) + syntax.kwParentheses(1)
    else left.toString(nameSpace, syntax)) + " " + syntax.kwEquivalence + " " +
      (if (right.needsParenthesis) syntax.kwParentheses(0) + right.toString(nameSpace, syntax) + syntax.kwParentheses(1)
      else right.toString(nameSpace, syntax))
  }

  def substitute(substitution: Var.Substitution) = {
    new EqFormula(left.substitute(substitution), right.substitute(substitution))
  }

  /** Returns (!left v right) ^ (!right v left) */
  def implFree = {
    val l2 = left.implFree
    val r2 = right.implFree
    ConjFormula(DisjFormula(NegFormula(l2), r2),
      DisjFormula(NegFormula(r2), l2))
  }

  def nnf(negate: Boolean = false) = {
    throw new IllegalStateException("NNF expects an equivalence free formula: "+this)
    new EmptyFormula
  }

  def pnf = {
    throw new IllegalStateException("PNF expects an equivalence free formula: "+this)
    new EmptyFormula
  }

  def cnf = {
    throw new IllegalStateException("CNF expects an equivalence free formula: "+this)
    new EmptyFormula
  }

  def rmSquareBrackets = (left.rmSquareBrackets, right.rmSquareBrackets) match {
    case (`left`, `right`) => this
    case (l, r) => EqFormula(l, r)
  }

  def expandAlchemy = {
    var l = left.expandAlchemy
    var r = right.expandAlchemy
    l.flatMap { lf =>
      r.map { rf =>
        EqFormula(lf, rf)
      }
    }
  }

  def existToDisj = {
    EqFormula(left.existToDisj, right.existToDisj)
  }

  def normalize = {
    right match {
      case cf: EqFormula => EqFormula(EqFormula(left.normalize, cf.left.normalize), cf.right.normalize)
      case _ => this
    }
  }

  def predicates = left.predicates union right.predicates
  def atoms = left.atoms union right.atoms
  override def boundVariables = left.boundVariables ++ right.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    (left.evaluate(values), right.evaluate(values)) match {
      case (fl: TrueFormula, fr: TrueFormula) => fl
      case (fl: FalseFormula, fr: FalseFormula) => TrueFormula()
      case (fl, fr) => EqFormula(fl, fr)
    }
  }
}

/** An existential quantifier is a formula. */
sealed case class ExistFormula(
  vars: List[Var],
  formula: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    val argStrings = for (arg <- vars) yield {
      arg match {
        case variable: Var => nameSpace.getName(variable)
        case _ => "NoVar"
      }
    }
    syntax.kwExistential + " " + argStrings.mkString(",") + " " + formula.toString(nameSpace, syntax)
  }

  def substitute(substitution: Var.Substitution) = {
    val subvars = vars.map { _.substitute(substitution) }
    val newterms = subvars.filter { _.isInstanceOf[Var] }
    val newvars = newterms.map { _.asInstanceOf[Var] }
    newvars.length match {
      case 0 => formula.substitute(substitution)
      case _ => new ExistFormula(newvars, formula.substitute(substitution))
    }
  }

  //def implFree = {assume(false, "Implication free should be called after remove existential quantifier."); new EmptyFormula}
  def implFree = ExistFormula(vars, formula.implFree)

  def nnf(negate: Boolean = false) = {
    if (negate) {
      val map = vars.map(v => v -> new Var).toMap.withDefault(v => v)
      val newvars = map.values.toList
      ForallFormula(newvars, formula.nnf(true).substitute(map))
    } else {
      formula.nnf(false) match {
        case `formula` => this
        case nf => ExistFormula(vars, nf)
      }        
    }
  }

  def pnf = formula.pnf match {
    case `formula` => this
    case f => this.copy(formula = f)
  }

  def cnf = {
    assume(false, "CNF expects an existential free formula")
    new EmptyFormula
  }

  def rmSquareBrackets = formula.rmSquareBrackets match {
    case `formula` => this
    case f => ExistFormula(vars, f)
  }

  def expandAlchemy: List[Formula] = {
    formula.expandAlchemy.map { f =>
      copy(formula = f)
    }
  }

  def existToDisj = {
    // We cannot perform this operation correctly because the domain sizes are
    // not known when the formula is translated into CNF.
    println("[Warning] Expanding existential quantifier to disjunction of known constants. Domain sizes are ignored!\n"+
            "          Use Skolemization for domains where not all constants are given")
    val map = new mutable.HashMap[Var, Term]()
    val domains = vars.map { v =>
      val atom = atoms.find { _.args.contains(v) }.get
      atom.domain(v)
    }
    val exlist = expandExistentialRec(vars zip domains, map)
    exlist.length match {
      case 0 => {
        println("[Warning] Expanding existential quantifier to disjunction for empty domain leads to empty formula")
        EmptyFormula()
      }
      case 1 => exlist.head
      case _ => {
        val initdisj = DisjFormula(exlist.head, exlist.tail.head)
        exlist.tail.tail.foldLeft(initdisj) { (r, c) => DisjFormula(r, c) }
      }
    }
  }

  private def expandExistentialRec(recvars: List[(Var, Domain)],
    map: mutable.HashMap[Var, Term]): List[Formula] = {
    recvars match {
      case Nil => {
        // Bounded variables should be separated
        val mapt = formula.boundVariables.map(v => v -> new Var).toList
        val map2 = map ++ mapt
        //println("\nNFORM:\n"+nformula+"MAP:\n"+map+"\n"+mapt+"\n"+map2)
        List(formula.substitute(map2.withDefault(v => v)).existToDisj)
        //List(nformula.substitute(v => {
        // If variable is not in map it is not part of the 
        // existential vars and should therefore not be replaced.
        //map.getOrElse(v, v)
        //}))
      }
      case rv => {
        val (curvar, curdom) = rv.head
        // TODO: should be curdom.constants(domainSize) ...
        curdom.knownConstants.flatMap { constant =>
          map.update(curvar, constant)
          expandExistentialRec(recvars.tail, map)
        }
      }
    }
  }

  def normalize = this

  def predicates = formula.predicates
  def atoms = formula.atoms
  override def boundVariables = formula.boundVariables ++ vars

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    ExistFormula(vars, formula.evaluate(values))
  }
}

/** An universal quantifier is a formula. */
sealed case class ForallFormula(
  vars: List[Var],
  formula: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    val argStrings = for (arg <- vars) yield {
      arg match {
        case variable: Var => nameSpace.getName(variable)
        case _ => "NoVar"
      }
    }
    syntax.kwUniversal + " " + argStrings.mkString(",") + " " + formula.toString(nameSpace, syntax)
  }

  def substitute(substitution: Var.Substitution) = {
    val subvars = vars.map { _.substitute(substitution) }
    val newterms = subvars.filter { _.isInstanceOf[Var] }
    val newvars = newterms.map { _.asInstanceOf[Var] }
    newvars.length match {
      case 0 => formula.substitute(substitution)
      case _ => new ForallFormula(newvars, formula.substitute(substitution))
    }
  }

  //def implFree = {assume(false, "Implication free should be called after remove universal quantifier."); new EmptyFormula}
  def implFree = ForallFormula(vars, formula.implFree)

  def nnf(negate: Boolean = false) = {
    //assume(false, "NNF expects an existential free formula")
    //new EmptyFormula
    if (negate) {
      val map = vars.map(v => v -> new Var).toMap.withDefault(v => v)
      val newvars = map.values.toList
      ExistFormula(newvars, formula.nnf(true).substitute(map))
    } else {
      formula.nnf(false) match {
        case `formula` => this
        case nf => ForallFormula(vars, nf)
      }
    }
  }

  def pnf = formula.pnf match {
    case `formula` => this
    case f => this.copy(formula = f)
  }

  def cnf = {
    assume(false, "CNF expects an universal quantifier free formula")
    new EmptyFormula
  }

  def rmSquareBrackets = formula.rmSquareBrackets match {
    case `formula` => this
    case f => ForallFormula(vars, f)
  }

  def expandAlchemy: List[Formula] = {
    formula.expandAlchemy.map { f =>
      copy(formula = f)
    }
  }

  /**
   * @note   Precondition:
   *         Expects formula to be in NNF
   */
  def existToDisj = {
    formula.existToDisj
  }

  def normalize = this

  def predicates = formula.predicates
  def atoms = formula.atoms
  override def boundVariables = formula.boundVariables ++ vars

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    ForallFormula(vars, formula.evaluate(values))
  }
}

/** An empty formula is to be ignored. */
sealed case class EmptyFormula() extends Formula {
  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = "#EmptyFormula"
  def substitute(substitution: Var.Substitution) = { this }

  def implFree = this
  def nnf(negate: Boolean = false) = this
  def pnf = this
  def cnf = this
  def rmSquareBrackets = this
  def expandAlchemy = List(this)
  def existToDisj = this
  def normalize = this
  def predicates = Set.empty
  def atoms = Set.empty
  def evaluate(values: Atom => Option[Boolean]): Formula = this
}

/**
 * @TODO: Add universal quantifier?
 */

//--- MLN extensions to formulas -----------------------------------------------

/** Wildcard negation. */
sealed case class WildNegFormula(
  formula: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    syntax.kwWildcardNegation + (if (formula.needsParenthesis) syntax.kwParentheses(0) + formula.toString(nameSpace, syntax) + syntax.kwParentheses(1)
    else formula.toString(nameSpace, syntax))
  }

  def substitute(substitution: Var.Substitution) = {
    new WildNegFormula(formula.substitute(substitution))
  }

  def implFree = {
    assume(false, "Remove implication expects a formula without wildcard negation")
    new EmptyFormula
  }

  def nnf(negate: Boolean = false) = {
    assume(false, "NNF expects a formula without wildcard negation")
    new EmptyFormula
  }

  def pnf = {
    assume(false, "PNF expects a formula without wildcard negation")
    new EmptyFormula
  }

  def cnf = {
    assume(false, "CNF expects a formula without wildcard negation")
    new EmptyFormula
  }

  def rmSquareBrackets = formula.rmSquareBrackets match {
    case `formula` => this
    case f => WildNegFormula(f)
  }

  def expandAlchemy = {
    formula.expandAlchemy.flatMap { f =>
      List(f, NegFormula(f))
    }
  }

  def existToDisj = {
    WildNegFormula(formula.existToDisj)
  }

  def normalize = WildNegFormula(formula.normalize)
  def predicates = formula.predicates
  def atoms = formula.atoms
  override def boundVariables = formula.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    throw new IllegalStateException("Cannot evaluating an Alchemy formula with Alchemy specific syntax.")
    this
  }
}

/** + operator */
sealed case class AtomicExpFormula(
  atom: Atom,
  exp: List[Boolean]) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    "+" + atom.toString(nameSpace)
  }

  override def needsParenthesis = false
  override def isLiteral = true

  def substitute(substitution: Var.Substitution) = {
    new AtomicExpFormula(atom.substitute(substitution), exp)
  }

  def implFree = this

  def nnf(negate: Boolean = false) = {
    if (negate) {
      NegFormula(this)
    } else {
      this
    }
  }

  def pnf = this
  def cnf = this
  def rmSquareBrackets = this

  def expandAlchemy = {
    val map = new mutable.HashMap[Var, Constant]()
    expandAlchemyRec((atom.args zip exp).toList, map)
  }

  def expandAlchemyRec(vars: List[(Term, Boolean)], map: mutable.HashMap[Var, Constant]): List[Formula] = {
    vars match {
      case Nil => {
        List(LiteralFormula(atom.substitute(v => {
          map.getOrElseUpdate(v, new Constant("error"))
        })))
      }
      case v => {
        val (curterm, curexp) = v.head
        curterm match {
          case curvar: Var if curexp => {
            atom.domain(curvar).knownConstants.flatMap { constant =>
              map.update(curvar, constant)
              expandAlchemyRec(vars.tail, map)
            }
          }
          case _ => {
            expandAlchemyRec(vars.tail, map)
          }
        }
      }
    }
  }

  def existToDisj = this

  def normalize = this

  def predicates = Set(atom.predicate)
  def atoms = Set(atom)

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    throw new IllegalStateException("Cannot evaluating an Alchemy formula with Alchemy specific syntax.")
    this
  }
}

/**
 * Alchemy has two types of parentheses: () and [].
 * The [] are used to change the way how weights are divided over the generated
 * clauses.
 */
sealed case class IndivisibleSubformula(
  formula: Formula) extends Formula {

  override def toString(nameSpace: NameSpace[Any, String],
    syntax: FormulaSyntax) = {
    "[" + formula.toString(nameSpace, syntax) + "]"
  }
  override def needsParenthesis = false

  def substitute(substitution: Var.Substitution) = {
    new IndivisibleSubformula(formula.substitute(substitution))
  }

  def implFree = this // Consider as atomic formula
  def nnf(negate: Boolean = false) = {
    // Consider as atomic formula
    if (negate) {
      NegFormula(this)
    } else {
      this
    }
  }

  def pnf = new IndivisibleSubformula(formula.implFree.nnf(false).pnf) // Consider as atomic formula
  def cnf = new IndivisibleSubformula(formula.implFree.nnf(false).pnf.cnf) // Consider as atomic formula

  def rmSquareBrackets = formula.rmSquareBrackets
  def expandAlchemy = formula.expandAlchemy.map { f => IndivisibleSubformula(f) }

  def existToDisj = {
    IndivisibleSubformula(formula.existToDisj)
  }

  def normalize = IndivisibleSubformula(formula.normalize)
  def predicates = formula.predicates
  def atoms = formula.atoms
  override def boundVariables = formula.boundVariables

  def evaluate(values: Atom => Option[Boolean]): Formula = {
    throw new IllegalStateException("Cannot evaluating an Alchemy formula with Alchemy specific syntax.")
    this
  }
}

//def EvaluateValues(val values:List[(Atom,Boolean)]) = {
//	
//}

/**
 * @TODO: Extend syntax to also allow MLN "+" and "!" modifiers?
 *        This needs to be done in Atom.class
 */
