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
import scala.util.parsing.combinator._
import scala.io._
import scala.math._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.formulas.visitor.Skolemization
import edu.ucla.cs.starai.forclift.formulas.visitor.Skolemizer
import edu.ucla.cs.starai.forclift.languages.StatRelModel

/** Default namespace for MLN predicates. */
class WFNameSpace extends NameSpace[WeightedFormula, String] {

  var nbPredicates = 0

  override def createName(f: WeightedFormula): String = {
    nbPredicates += 1
    "f_" + nbPredicates
  }
}

class MLNNameSpace extends NameSpace[Any, String] {

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
        n(0).toUpper + n.substring(1, n.length)
      }
      case pred: Predicate => {
        val n = pred.toString
        n(0).toLower + n.substring(1, n.length)
      }
      case _ => obj.toString
    }
  }

}

/**
 * Weighted formulas as being used by MLNs.
 *
 * @note MLNs do not allow for constraints in the definition but since WFOMC
 * allows it and it is useful to define something like an eq/2 predicate they
 * are also present in our WeightedFormula class.
 */
case class WeightedFormula(
  formula: Formula,
  weight: Double,
  hard: Boolean = false,
  initialConstrs: Constraints = Constraints.empty) {

  val variables: Set[Var] = formula.variables
  def predicates: Set[Predicate] = formula.predicates.filter { _ != Predicate.eq }
  def atoms: Set[Atom] = formula.atoms

  final val constrs: Constraints = initialConstrs.addMissingConstraints(variables, atoms.toList)

  override def toString: String = {
    val ns = new MLNNameSpace
    (if (hard) "" else weight + " ") +
      formula.toStringAlchemy(ns) +
      (if (hard) "." else "") +
      (if (initialConstrs != Constraints.empty) " , " + initialConstrs.toString(ns) else "")
  }

  /** Disconnect variables in formula from rest of MLN */
  def standardizeApart: WeightedFormula = {
    val map = new mutable.HashMap[Var, Var]()
    substitute(v => {
      assume(!map.values.exists(_ == v))
      map.getOrElseUpdate(v, new Var)
    })
  }

  /** Substitute the given var with the given term. */
  def substitute(from: Var, to: Term): WeightedFormula = {
    substitute { v: Var => if (v == from) to else v }
  }

  def substitute(substitution: Var => Term): WeightedFormula = {
    WeightedFormula(
      formula.substitute(substitution),
      weight,
      hard,
      initialConstrs.substitute(substitution))
  }

  def isEmpty: Boolean = {
    formula match {
      case f: EmptyFormula => true
      case _ => false
    }
  }

  def isHard: Boolean = hard

  /**
   * One clause per weightedformula (based on Alchemy's transformation).
   *
   * The original weight is divided over all atomic formulae in the
   * conjunction. This is applied recursively over MLN's indivisible
   * subformulae.
   * [[http://alchemy.cs.washington.edu/user-manual/4_2MLN_Syntax.html]]
   *
   * Two notable exceptions to simply dividing the weight:
   *
   * 1. Single literals in a conjunction are kept together.
   * {{{
   * 2 a ^ b ^ (c v d)
   *  ||
   * -1 !a v !b
   *  1  c v  d
   * }}}
   *
   * 2. Square brackets indicate an indivisible formula and are split first as
   * a whole.
   * {{{
   * 2 a ^ [ b ^ (c v d) ]
   *  ||
   * 1   a
   * 0.5 b
   * 0.5 c v d
   * }}}
   *
   * @todo What should the following give?
   * {{{
   * 2 (P(x) ^ [ Q(x) ^ (R(x) v S(x)) ]) <=> F(x)
   * }}}
   * The NNF version of this is:
   * {{{
   *    (!F(X) |  P(X))
   *  & (!F(X) |  [Q(X) & (R(X) | S(X))])
   *  & (!P(X) | ![Q(X) & (R(X) | S(X))] | F(X))
   * }}}
   *
   * @todo Should splitted weighted formulas be standardized apart? We
   * should do this when creating clauses so I think it's ok.
   *
   * @note Precondition:
   * Formula is in CNF (indivisible subformulas are seen as a literal but are
   * themselves again in CNF)
   *
   * @return List of weighted formulas equivalent to the current formula
   */
  def splitClauses: List[WeightedFormula] = {
    formula match {
      case cf: ConjFormula => {
        val conjuncts = cf.conjunctsToList
        val (clits, cforms) = conjuncts.partition { _.isLiteral }
        val numsplits = (if (clits.isEmpty) 0 else 1) + cforms.length
        val new_w = (if (hard) weight else weight / numsplits)
        val clitsf: List[WeightedFormula] = clits.length match {
          case 0 => Nil
          case 1 => List(new WeightedFormula(clits.head, new_w, hard, initialConstrs))
          case _ => {
            val fdisj = new DisjFormula(clits(0).negate, clits(1).negate)
            List(new WeightedFormula(
              clits.slice(2, clits.length).foldLeft(fdisj)((acc, kv) =>
                new DisjFormula(acc, kv.negate)),
              -new_w,
              hard,
              initialConstrs))
          }
        }
        val cformsf: List[WeightedFormula] = cforms.flatMap { new WeightedFormula(_, new_w, hard, initialConstrs).splitClauses }
        clitsf ::: cformsf
      }
      case isf: IndivisibleSubformula => {
        (new WeightedFormula(isf.formula, weight, hard, initialConstrs)).splitClauses
      }
      case _ => List(this)
    }
  }

  /**
   * Returns a list of literals if the formula is a disjunction
   *
   * @note Precondition:
   * Formula is a disjunction of literals
   *
   * @return   Tuple of list of positive literals and list of negative literals
   */
  def disjunctionToList: (List[Atom], List[Atom]) = {
    val disjuncts = (formula match {
      case df: DisjFormula => df.disjunctsToList
      case f => List(f)
    })

    disjuncts.foldLeft((Nil, Nil): (List[Atom], List[Atom])) { (r, c) =>
      c match {
        case lit: LiteralFormula => {
          if (lit.sign) {
            (lit.atom :: r._1, r._2)
          } else {
            (r._1, lit.atom :: r._2)
          }
        }
        case i: IndivisibleSubformula => {
          throw new IllegalStateException("Warning: " +
            "Expected a disjunction of literals, found an indivisible " +
            "subformula in a disjunction:" + i)
        }
        case f => {
          throw new IllegalStateException("Warning: " +
            "Expected a disjunction of literals, ignoring formula: " + f)
        }
      }
    }
  }

  /**
   * Transform weighted clause to weighted CNF.
   *
   * Based on Guy's transformation of disjunct Braz factors. But can also be
   * encoded directly by an equivalence between the disjunction and the new
   * predicate representing the weight. (Followed by a toCNF step)
   *
   * @note Precondition:
   * Formula is disjunction of literals (no atomic subformulas)
   *
   * @param    nameSpace
   */
  def toWeightedCNF(nameSpace: WFNameSpace): WeightedCNF = {
    val (posliterals, negliterals) = disjunctionToList

    if (hard) {
      val clause = new Clause(
        posliterals,
        negliterals,
        initialConstrs).eqToConstraints;
      WeightedCNF(
        new CNF(List(clause)),
        DomainSizes.empty,
        PredicateWeights.empty)
    } else {
      // (pl1 v pl2 v !nl1 v !nl2) <=> f
      //
      // pl1 v pl2 v !nl1 v !nl2 v !f  // c0
      // !pl1 ^ f                      // c1
      // !pl2 ^ f
      // nl1 ^ f                       // c2
      // nl2 ^ f

      val prefix = nameSpace.getName(this)
      val literals = (posliterals ::: negliterals).filterNot(_.predicate == Predicate.eq)
      val vars = literals.flatMap { _.variables }.toSet.toList
      val domains = vars.map { v =>
        val atom = literals.find { _.args.contains(v) }.get
        atom.domain(v)
      }
      val res = new Predicate(Symbol(prefix + "}"), vars.size, domains)
      val f = res(vars: _*)

      val c0 = Clause(
        posliterals,
        f :: negliterals,
        initialConstrs).eqToConstraints // c0
      val otherliterals: List[Clause] =
        posliterals.map { a =>
          Clause(f :: Nil,
            a :: Nil,
            initialConstrs).eqToConstraints
        } ::: // c1
          negliterals.map { a =>
            Clause(f :: a :: Nil,
              Nil,
              initialConstrs).eqToConstraints
          } // c2

      val clauses: List[Clause] = (c0 :: otherliterals).map { _.standardizeApart }
      val cnf = new CNF(clauses)
      val predWeights = PredicateWeights.empty + (res -> Weights(exp(weight), 1)) // 1 == exp(0)
      WeightedCNF(cnf, DomainSizes.empty, predWeights)
    }
  }

  /**
   * Transform weighted formula to weighted CNF by introducing a new predicate
   * and an equivalence (or implication) relation with the original formula.
   *
   * {{{
   * w ϕ
   * }}}
   * to
   * {{{
   * ϕ ⟺  P
   * }}}
   * With weight(P) = (e^w,1)
   *
   * Alternative transformations:
   *
   * - Right implication is 'w ϕ' where ϕ is a formula represented as
   *   'ϕ → P' with weight(P) = (w/(1-w),1).
   * - Left implication is 'w ϕ' where ϕ is a formula represented as
   *   'ϕ ← P' with weight(P) = (w-1,1).
   *
   * @param  skolemize
   *         Replace existential quantifiers using Skolemization instead of
   *         a disjunction.
   * @param  transformation
   *         0: Equivalence
   *         1: Left implication
   *         2: Right implication
   * @param  earlySk
   *         Early skolemization. Perform Skolemization before translating
   *         to PNF. This results in more theories that are liftable because
   *         expanding th introducede equivalence results in two quantifiers
   *         which in turn results in extra variables and possibly unliftable
   *         formulas.
   *         (default: true)
   */
  def toWeightedCNFWithoutSplitting(wf_ns: WFNameSpace,
    exist_ns: EFNameSpace,
    skolemize: Boolean = false,
    transformation: Int = 0,
    earlySk: Boolean = true): (WeightedCNF, Option[Atom]) = {

    // Transform formula to include weight
    val (predWeights, nformula, formatom) = if (hard) {
        // Copy hard formula as is
        (PredicateWeights.empty, formula, None)
    } else {
        // Create weighted atom
        val prefix = wf_ns.getName(this)
        val atoms = formula.atoms.filterNot(_.predicate == Predicate.eq)
        val vars = (atoms.flatMap { _.variables }.toSet -- formula.boundVariables).toList
        val domains = vars.map { v =>
          val atom = atoms.find { _.args.contains(v) }.get
          atom.domain(v)
        }

        // Equivalence or implication with new predicate with associated weight
        val res = new Predicate(Symbol(prefix), vars.size, domains)
        val f = res(vars: _*)
        val (predWeights, nformula) = transformation match {
          case 0 => (PredicateWeights.empty + (res -> Weights(exp(weight), 1)), // 1 == exp(0)
            EqFormula(formula, LiteralFormula(f)))
          case 1 => (PredicateWeights.empty + (res -> Weights(exp(weight) / (1 - exp(weight)), 1)),
            ImplFormula(formula, LiteralFormula(f)))
          case 2 => (PredicateWeights.empty + (res -> Weights(exp(weight) - 1, 1)),
            ImplFormula(LiteralFormula(f), formula))
        }
        (predWeights, nformula, Some(f))
    }

    // Transform to CNF formula
    val (cnfl, skPredweights) = if (!skolemize) {
        ((nformula.rmSquareBrackets.implFree.nnf().existToDisj.cnf, Constraints.empty) :: Nil,
          PredicateWeights.empty)
    } else {
        if (earlySk) {
            // Replace all quantifiers immediately (both existential and universal)
            // TODO: This potentially Skolemizes some quantifiers which could
            // have been simply assumed implicit (e.g. forall without negation)
            val clean_formula = nformula.rmSquareBrackets
            val skolemizer = Skolemizer()
            val (sk_formula, sk_formulas, _, sk_weights) = skolemizer.skolemize(clean_formula, exist_ns, initialConstrs)
            ((sk_formula.implFree.nnf().cnf, Constraints.empty) :: sk_formulas.map{t => (t._1.implFree.nnf().cnf, t._2)}, sk_weights)

        } else {
            // When equivalence operators are present, this leads to an
            // increased number of quantifiers which in turn leads to more
            // variables and potentially unliftable formulas.
            // For example
            //    p <=> exist y forall x, q(x,y)
            //    exist y forall x forall yp exist xp, [!p v q(x,y)] ^ [p v !q(xp,yp)]
            // The first Tseitin variable is
            //    Z_1(x,yp,xp)
            // Which are unecessarily many variables and eventually results in
            // a theory that is not compilable.

            //println("First to PNF"+nformula.rmSquareBrackets.implFree.nnf())
            val clean_formula = nformula.rmSquareBrackets.implFree.nnf().pnf
            val skolemizer = Skolemizer()
            val (sk_formula, sk_formulas, _, sk_weights) = skolemizer.skolemize(clean_formula, exist_ns, initialConstrs)
            ((sk_formula.nnf().cnf, Constraints.empty) :: sk_formulas.map{t => (t._1.nnf().cnf, t._2)}, sk_weights)
        }
    }

    // Transform to clauses
    val disjuncts = cnfl.flatMap{ case (formula, constr) =>
      val formulas = formula match {
        case cf: ConjFormula => cf.conjunctsToList
        case df: DisjFormula => List(df)
        case lf: LiteralFormula => List(lf)
        case f => {
          throw new IllegalStateException("Warning: " +
            "CNF is not a conjunction of disjuncts:\n" + f)
        }
      }
      formulas.map((_,constr))
    }

    val clauses = disjuncts.map { case (disjunct, constr) =>
      val (posliterals, negliterals) = disjunct match {
        case df: DisjFormula => df.disjunctsToList.foldLeft((Nil, Nil): (List[Atom], List[Atom])) { (r, c) =>
          c match {
            case lit: LiteralFormula if lit.sign => (lit.atom :: r._1, r._2)
            case lit: LiteralFormula if !lit.sign => (r._1, lit.atom :: r._2)
            case f => {
              throw new IllegalStateException("Warning: " +
                "Clause in CNF is not a disjunction:\n" + f)
            }
          }
        }
        case lf: LiteralFormula if lf.sign => (lf.atom :: Nil, Nil)
        case lf: LiteralFormula if !lf.sign => (Nil, lf.atom :: Nil)
        case f => {
          throw new IllegalStateException("Warning: " +
            "Clause in CNF is not a disjunction:\n" + f)
        }
      }

      Clause(posliterals,
             negliterals,
             initialConstrs.join(constr)).eqToConstraints.standardizeApart
    }

    val cnf = new CNF(clauses)

    (WeightedCNF(cnf, DomainSizes.empty, predWeights ++ skPredweights), formatom)
  }

  def skolemize(exist_ns: EFNameSpace): (List[WeightedFormula], List[Predicate], PredicateWeights) = {
    val clean_formula = formula.rmSquareBrackets
    val skolemizer = Skolemizer()
    val (sk_formula, sk_formulas, sk_preds, sk_weights) = skolemizer.skolemize(clean_formula, exist_ns, initialConstrs)
    val wfs = copy(formula = sk_formula) :: sk_formulas.map { case (form,constr) => WeightedFormula(form, 0, true, initialConstrs.join(constr)) }
    (wfs, sk_preds, sk_weights)
  }

  def normalize: WeightedFormula = WeightedFormula(formula.normalize, weight, hard, initialConstrs)

  def expandAlchemy: List[WeightedFormula] = {
    formula.expandAlchemy.map { f => WeightedFormula(f, weight, hard, initialConstrs) }
  }

  def existFree: List[WeightedFormula] = {
    expandAlchemy.map { wf => copy(formula = wf.formula.existToDisj) }
  }

  def implFree: List[WeightedFormula] = {
    existFree.map { wf => WeightedFormula(wf.formula.implFree, wf.weight, wf.hard, wf.initialConstrs) }
  }

  def nnf: List[WeightedFormula] = {
    implFree.map { wf => WeightedFormula(wf.formula.nnf(false), wf.weight, wf.hard, wf.initialConstrs) }
  }

  /**
   * Transform the formula into a CNF form. The entire CNF is still associated
   * with the one weight (the weight is not spread in any way over the clauses
   * in the conjunction).
   */
  def cnf: List[WeightedFormula] = {
    nnf.map { wf => WeightedFormula(wf.formula.cnf, wf.weight, wf.hard, wf.initialConstrs) }
  }

  /** Domain constraints for all variables in event. */
  //final val elemConstrs: ElemConstr = {
  //val missingVars = variables -- explicitElemConstrs.variables
  //val missingDomains = missingVars.map { v =>
  //val atom = atoms.find { _.args.contains(v) }.get
  //(v -> atom.domain(v))
  //}
  //new ElemConstr(explicitElemConstrs ++ missingDomains)
  //}

  /** Shatter inequalities so that both sides have the same set of groundings */
  def needsIneqDomainShattering: Boolean = constrs.needsIneqDomainShattering

  /** Return a list of all ground clauses based on this weighted formula. */
  def ground(domainSizes: DomainSizes): List[WeightedFormula] = {
    if (variables.isEmpty) {
      List(this)
    } else {
      if (needsIneqDomainShattering) {
        //shatterIneqDomains.flatMap { _.ground(domainSizes) }
        require(false, "Not implemented")
        List(this)
      } else {
        val v = variables.head
        val domain = constrs.domainFor(v)
        //val excludedConstants: Set[Constant] = ineqConstrs(v).collect { case c: Constant => c }
        //val constants = domain.constants(domainSizes, excludedConstants.toSet)
        //val constants = domain.constants(domainSizes, Set())
        val excludedConstants: Set[Constant] = constrs.differentFrom(Set(v)).collect { case c: Constant => c }
        val constants = domain.constants(domainSizes, excludedConstants.toSet)
        val groundedOnce = constants.map { c =>
          substitute(v, c)
        }
        groundedOnce.flatMap { _.ground(domainSizes) }
      }
    }
  }

  /**
   * Ground formula while keeping a set of atoms consistent with the
   * groundings.
   */
  def groundWithAtoms(domainSizes: DomainSizes, atoms: List[Atom]): List[(WeightedFormula, List[Atom])] = {
    //println("GroundWithAtoms using domainSizes:\n"+domainSizes)
    if (variables.isEmpty) {
      List((this, atoms))
    } else {
      if (needsIneqDomainShattering) {
        //shatterIneqDomains.flatMap { _.ground(domainSizes) }
        require(false, "Not implemented")
        List((this, atoms))
      } else {
        val v = variables.head
        val domain = constrs.domainFor(v)
        //println("Working with domain: "+domain)
        //val excludedConstants: Set[Constant] = ineqConstrs(v).collect { case c: Constant => c }
        //val constants = domain.constants(domainSizes, excludedConstants.toSet)
        //val constants = domain.constants(domainSizes, Set())
        val excludedConstants: Set[Constant] = constrs.differentFrom(Set(v)).collect { case c: Constant => c }
        val constants = domain.constants(domainSizes, excludedConstants.toSet)
        //println("Constants: "+constants)
        val groundedOnce = constants.map { c =>
          val subs = (v2: Var) => if (v2 == v) c else v2
          (substitute(subs), atoms.map { _.substitute(subs) })
        }
        groundedOnce.flatMap { case (wf, as) => wf.groundWithAtoms(domainSizes, as) }
      }
    }
  }

}

/**
 * Class representing an MLN.
 *
 * @param  wformulas
 * @param  predicates
 *         Predicates in the MLN. This is given as a separate list because it
 *         is not guaranteed that all predicates appear in a formula. This is
 *         important for learning and likelihood.
 * @param  domainsize
 * @param  evidence
 *         Facts found in database file
 */
case class MLN(
  val wformulas: List[WeightedFormula] = List(),
  val predicates: Set[Predicate] = Set(),
  val domainSizes: DomainSizes = DomainSizes.empty,
  val evidence: List[Formula] = Nil,
  val weights: PredicateWeights = PredicateWeights.empty) extends StatRelModel{
  override def toString: String = wformulas.mkString("\n")

  private[this] var alchemySemantics = false
  
  def setAlchemySemantics(alchemySemantics: Boolean) {
    this.alchemySemantics = alchemySemantics
  }
  
  def getAlchemySemantics() = {
    alchemySemantics
  }
  
  def toStringFull: String = {
    predicates.map(_.toStringFull).mkString("\n", "\n", "\n") +
      wformulas.mkString("\n", "\n", "\n")
  }

  def toStringExt: String = {
    val domains = wformulas.flatMap { _.formula.predicates.flatMap { _.domains.map { d => d.toString + d.knownConstants.mkString("= {", ",", "}") } } }.toSet
    domains.mkString("\nDomains:\n", "\n", "\n") +
      wformulas.mkString("\nTheory:\n", "\n", "\n") +
      evidence.mkString("\nEvidence:\n", "\n", "\n")
  }

  def toStringHeader: String = {
    val domains = wformulas.flatMap { _.formula.predicates.flatMap { _.domains.map { d => d.toString + d.knownConstants.mkString("= {", ",", "}") } } }.toSet
    domains.mkString("\n", "\n", "\n") +
      predicates.map(_.toStringFull).mkString("\n", "\n", "\n")
  }
  
  /** MLN that is completely equivalent with our internal representation of 
   *  the MLN. E.g. weighted predicates are translated into weighted formulae.
   */
  def toStringEquivalent: String = {
    val ns = new MLNNameSpace
    
    val domains = wformulas.flatMap { _.formula.predicates.flatMap { _.domains }}.toSet
    val domains_str = domains.map{ d =>
      d.toString + d.constants(domainSizes).map{_.toString.capitalize}.mkString("= {", ",", "}") //d.knownConstants.mkString("= {", ",", "}")
    }.toList
    
    val predicates_str = predicates.map{_.toStringFull}.toList
    
    val eqweights = weights.map{case (pred,ws) =>
      val atom = pred((0 until pred.arity).map(i => new Var):_*)
      val atomstr = atom.toString(ns)
      ((if (ws.posWLogDouble.isNaN || ws.posWLogDouble.isNegInfinite) {
        s"${ws.posWLogDouble.logToDouble} $atomstr // ERROR: Cannot express numbers <= 0"
      } else {
        s"${ws.posWLogDouble.logToDouble} $atomstr"
      }) ::
      (if (ws.negWLogDouble.isNaN || ws.negWLogDouble.isNegInfinite) {
        s"${ws.negWLogDouble.logToDouble} !$atomstr // ERROR: Cannot express numbers <= 0"
      } else {
        s"${ws.negWLogDouble.logToDouble} !$atomstr"
      }) :: Nil).mkString("\n")
    }.toList
    
    ("/* Domains */" ::
    domains_str :::
    ("" :: "/* Predicates */" :: Nil) :::
    predicates_str :::
    ("" :: "/* (Weighted) formulas */" :: Nil) :::
    eqweights :::
    wformulas).mkString("\n")
  }

  /** Assumes formulas to be disjuncts **/
  def toStringLearnFormulas: String = {
    val wformulas_nnf = wformulas.flatMap { wf =>
      wf.nnf
    }
    wformulas_nnf.map { wf =>
      val ns = new MLNNameSpace
      val (poslitsall, neglitsall) = wf.disjunctionToList
      (poslitsall.map(_.toString(ns)) ::: neglitsall.map("!" + _.toString(ns))).mkString(" v ")
    }.mkString("\n", "\n", "\n")
  }

  def toMLNFileString: String = {
    require(evidence.isEmpty, "evidence not supported by output format")
    val domains = wformulas.flatMap { _.formula.predicates.flatMap { _.domains.map { d => d.toString + d.knownConstants.mkString("= {", ",", "}") } } }.toSet
    domains.mkString("", "\n", "\n") +
      predicates.map(_.toStringFull).mkString("", "\n", "\n") +
      wformulas.mkString("", "\n", "\n")
  }

  def evidenceToString: String = evidence.mkString("\n")

  //def predicates: Set[Predicate] = wformulas.flatMap{_.predicates}.toSet
  lazy val atoms = wformulas.flatMap { _.atoms }.toSet
  lazy val domains = predicates.flatMap { _.domains }.toSet

  /**
   * Return an equivalent MLN without Alchemy specific operators.
   *
   *  - Expand wildcard negation (*).
   *  - Expand variable to constants (+).
   *
   */
  def toMLNasAlchemyOpFree: MLN = {
    val wf_af = wformulas.flatMap { _.expandAlchemy }
    copy(wformulas = wf_af)
  }

  def toMLNasExistFree: MLN = {
    val wf_af = wformulas.flatMap { _.existFree }
    copy(wformulas = wf_af)
  }

  /** Return an equivalent MLN without implications or equivalences */
  def toMLNasIMPLFREE: MLN = {
    val wf_nnf = wformulas.flatMap { _.implFree }
    copy(wformulas = wf_nnf)
  }

  /**
   * Return an equivalent MLN without implications or equivalences and in
   * NNF.
   */
  def toMLNasNNF: MLN = {
    val wf_nnf = wformulas.flatMap { _.nnf }
    copy(wformulas = wf_nnf)
  }

  /** Return an equivalent MLN in CNF as created by Alchemy */
  def toMLNasAlchemyCNF: MLN = {
    val wf_cnf = wformulas.flatMap { _.cnf.flatMap { _.splitClauses } }
    copy(wformulas = wf_cnf)
  }

  /**
   * Return an equivalent weighted CNF to be used by WFOMC by splitting
   * weights.
   *
   * This is equivalent to how Alchemy works.
   */
  def toWeightedCNF(verbose: Boolean = false, skolemize: Boolean = true): WeightedCNF = {
    val nameSpace = new WFNameSpace
    val wf_na = wformulas.flatMap { wf => wf.expandAlchemy }
    val wf_cnf = wf_na.flatMap { wf =>
      {
        //WeightedFormula(
        //wf.formula.implFree.nnf(false).cnf,
        //wf.weight,
        //wf.hard).splitClauses
        wf.copy(formula = wf.formula.implFree.nnf().existToDisj.cnf).splitClauses
      }
    }
    if (verbose) {
      println("Equivalent MLN as CNF:")
      println(wf_cnf)
    }
    val wcnfs = wf_cnf.map(_.toWeightedCNF(nameSpace))
    // include weights for non-reified predicates
    //val predicateWeights = new PredicateWeights(Map.empty ++ predicates.map{(_->Weights(1,1))})
    val predicateWeights = weights ++ predicates.filterNot(weights.contains(_)).map { (_ -> Weights(1, 1)) }
    val startWcnf = WeightedCNF(CNF(), domainSizes, predicateWeights)
    val wcnf = wcnfs.foldLeft(startWcnf) { _ ++ _ }
    wcnf
  }

  /**
   * Return an equivalent weighted CNF to be used by WFOMC without splitting
   * weights.
   *
   * The weights are not split over the clauses in CNF as is done in Alchemy.
   * This is equivalent to how Primula handles MLNs.
   *
   * @param  skolemize
   *         Replace existential quantifiers using Skolemization instead of
   *         a disjunction.
   * @param  transformation
   *         0: Equivalence
   *         1: Left implication
   *         2: Right implication
   */
  def toWeightedCNFWithoutSplitting(verbose: Boolean = false,
    skolemize: Boolean = true,
    transformation: Int = 0,
    earlySk: Boolean = true): WeightedCNF = {

    toWeightedCNFWithoutSplittingInt(verbose,
      skolemize = skolemize,
      transformation = transformation,
      earlySk = earlySk)._1
  }

  def toMLNSkolemized(): MLN = {

    val exist_ns = new EFNameSpace
    val results = wformulas.map { wf => wf.skolemize(exist_ns) }
    val (wfs, preds, predws) = results
    		.foldLeft(List[WeightedFormula](),List[Predicate](),PredicateWeights.empty) { (r, c) =>
    		  (r._1 ::: c._1, r._2 ::: c._2, r._3 ++ c._3)
			}
    copy(wformulas = wfs,
      predicates = predicates ++ preds,
      weights = weights ++ predws)
  }

  def toWeightedCNFWithoutSplittingInt(verbose: Boolean = false,
    skolemize: Boolean = false,
    transformation: Int = 0,
    earlySk: Boolean = true): (WeightedCNF, List[(WeightedFormula, Atom)]) = {

    val wf_ns = new WFNameSpace
    val exist_ns = new EFNameSpace
    val wf_na = wformulas.flatMap { wf => wf.expandAlchemy }
    val (wcnfs, atom2wfs) = wf_na.map { wf =>
      val (wcnf, formatom) = wf.toWeightedCNFWithoutSplitting(wf_ns, exist_ns,
        skolemize = skolemize,
        transformation = transformation,
        earlySk = earlySk)
      (wcnf, (wf, formatom))
    }.unzip
    //val predicateWeights = new PredicateWeights(Map.empty ++ predicates.map{(_->Weights(1,1))})
    val predicateWeights = weights ++ predicates.filterNot(weights.contains(_)).map { (_ -> Weights(1, 1)) }
    val startWcnf = WeightedCNF(CNF(), domainSizes, predicateWeights)
    val wcnf = wcnfs.foldLeft(startWcnf) { _ ++ _ }
    val atom2wfs2 = atom2wfs.filter(_._2.nonEmpty).map { pw => (pw._1, pw._2.get) }
    (wcnf, atom2wfs2)
  }

  def normalize: MLN = copy(wformulas = wformulas.map(_.normalize))

  /** Combine two MLNs in a new one. */
  def +(other: MLN): MLN = {
    MLN(wformulas ++ other.wformulas,
      predicates ++ other.predicates,
      domainSizes.combine(other.domainSizes),
      evidence ++ other.evidence)
  }

  def posEvidence: immutable.Set[Atom] = {
    evidence.collect {
      case LiteralFormula(a, true) => a
    }.toSet
  }

  def negEvidence: immutable.Set[Atom] = {
    // is empty when closed-world assumption is made!!!!
    require(false)
    evidence.collect {
      case LiteralFormula(a, false) => a
    }.toSet
  }

  def ground(domainSizes: DomainSizes): MLN = {
    copy(wformulas = wformulas.flatMap { _.ground(domainSizes) })
  }

  /**
   * CoShatter this MLN
   *
   * For example
   * {{{
   * friends(x,y) ^ smokes(y) => smokes(x)
   * }}}
   * Becomes
   * {{{
   * friends(x,x) ^ smokes(x) => smokes(x)
   * friends(x,y) ^ smokes(y) => smokes(x), x!=y
   * }}}
   */
  def coShatter: MLN = {
    val (wcnf, form2atoms) = toWeightedCNFWithoutSplittingInt()
    val formpreds = form2atoms.map(_._2.predicate)
    val scnf = wcnf.cnf.coShatter
    val unitclauses = scnf.toPositiveUnitClauses
    val sformpucs = unitclauses.foldLeft(List[PositiveUnitClause]()) { (r, c) =>
      if (r.exists(_.equivalent(c))) {
        r
      } else { 
        c :: r
      }
    }.filter(formpreds contains _.atom.predicate)

    var nformulas = List[WeightedFormula]();

    form2atoms.foreach {
      case (formula, atom) =>
        //println("Processing "+formula)
        sformpucs.foreach { sformpuc =>
          atom.unify(sformpuc.atom) match {
            case None => {}
            case Some(ec) => {
              //println("Matched to "+sformpuc)
              //println(ec)
              val map = mutable.HashMap[Var, Term]()
              ec.foreach { ecl =>
                ecl.tail.foreach {
                  case term: Var => map += ((term.asInstanceOf[Var], ecl.head))
                  case term: Constant => {}
                }
              }
              val nformula = formula.copy(initialConstrs = sformpuc.initialConstrs)
                .substitute(v => { map.getOrElse(v, v) })
              nformulas = nformula :: nformulas
            }
          }
        }
    }

    copy(wformulas = nformulas)
  }
}
