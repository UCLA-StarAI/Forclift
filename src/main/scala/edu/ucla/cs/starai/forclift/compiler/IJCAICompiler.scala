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

package edu.ucla.cs.starai.forclift.compiler

import collection._
import edu.ucla.cs.starai.forclift._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import edu.ucla.cs.starai.forclift.nnf._
import constraints._

object IJCAI11Compiler {

  val builder: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new IJCAI11Compiler(sizeHint) with LiftedCompiler

  val builderWithGrounding: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new IJCAI11Compiler(sizeHint) with GroundingCompiler

}

abstract class IJCAI11Compiler(sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_)) extends AbstractCompiler {

  def tryTautology(cnf: CNF) = {
    if (cnf.isTautology) {
      Some(TrueNode)
    } else None
  }

  def tryPositiveUnitClause(cnf: CNF) = {
    // only compile if the clause is unconditional
    // otherwise do shannon decomposition to separate the conditions from the literals, to help with smoothing
    val isPositiveUnit = cnf.isSingleton && cnf.clauses.head.isPositiveUnitClause && cnf.clauses.head.isUnconditional
    if (isPositiveUnit) {
      val unitClause = cnf.clauses.head
      val unitLeaf = new UnitLeaf(cnf, unitClause.toUnitClause, true)
      Some(unitLeaf)
    } else None
  }

  def tryNegativeUnitClause(cnf: CNF) = {
    val isPositiveUnit = cnf.isSingleton && cnf.clauses.head.isNegativeUnitClause && cnf.clauses.head.isUnconditional
    if (isPositiveUnit) {
      val unitClause = cnf.clauses.head
      val unitLeaf = new UnitLeaf(cnf, unitClause.toUnitClause, false)
      Some(unitLeaf)
    } else None
  }

  def tryContradictionClause(cnf: CNF) = {
    val isConditionalContradiction = cnf.clauses.size == 1 && cnf.clauses.head.isConditionalContradiction
    if (isConditionalContradiction) {
      val contradiction = cnf.clauses.head
      val contradictionLeaf = new ContradictionLeaf(cnf, contradiction.toContradictionClause, true)
      Some(contradictionLeaf)
    } else None
  }

  def tryPositiveUnitPropagation(cnf: CNF) = {
    val unitClauseOption = cnf.clauses.find { c => c.isPositiveUnitClause && c.isUnconditional }
    if (unitClauseOption.nonEmpty) {
      val unitClause = unitClauseOption.get
      val unitLiteral = unitClause.atoms.head
      val otherClauses: List[Clause] = cnf.clauses filterNot (_ == unitClause)
      val propagatedClauses = otherClauses.flatMap { _.condition(true, unitLiteral, unitClause.constrs) }
      val branchCnf = new CNF(propagatedClauses)
      val unitCNF = CNF(unitClause)
      val msg = "Unit propagation of $" + unitClause.toLatex() + "$."
      Some(new And(cnf, compile(unitCNF), compile(branchCnf), msg))
    } else None
  }

  def tryNegativeUnitPropagation(cnf: CNF) = {
    val unitClauseOption = cnf.clauses.find { c => c.isNegativeUnitClause && c.isUnconditional }
    if (unitClauseOption.nonEmpty) {
      val unitClause = unitClauseOption.get
      val unitLiteral = unitClause.atoms.head
      val otherClauses: List[Clause] = cnf.clauses filterNot (_ == unitClause)
      val propagatedClauses = otherClauses.flatMap { _.condition(false, unitLiteral, unitClause.constrs) }
      val branchCnf = new CNF(propagatedClauses)
      val unitCNF = CNF(unitClause)
      val msg = "Unit propagation of $" + unitClause.toLatex() + "$."
      Some(new And(cnf, compile(unitCNF), compile(branchCnf), msg))
    } else None
  }

  def tryRemoveDoubleClauses(cnf: CNF): Option[NNFNode] = {
    val newClauses = cnf.clauses.toSet.toList
    if (newClauses.size < cnf.clauses.size) {
      Some(compile(new CNF(newClauses)))
    } else None
  }

  def tryIndependentSubtheories(cnf: CNF): Option[And] = {
    tryIndependentSubtheories(cnf, false)
  }

  def tryIndependentSubtheories(cnf: CNF, afterShattering: Boolean): Option[And] = {
    //TODO use CNF.independentSubtheories
    def partition(depClauses: List[Clause], indepClauses: List[Clause]): (List[Clause], List[Clause]) = {
      if (indepClauses.isEmpty) (depClauses, Nil)
      else depClauses match {
        case clause :: rest => {
          val (indep, dep) = indepClauses.partition(clause.independent(_))
          val (depAll, indepAll) = partition(rest ++ dep, indep)
          (clause :: depAll, indepAll)
        }
        case Nil => (Nil, indepClauses)
      }
    }
    val (dep, indep) = partition(List(cnf.clauses.head), cnf.clauses.tail)
    if (indep.isEmpty) None
    else {
      val msg = if (!afterShattering) "Independence." else "Independence after shattering."
      Some(new And(cnf, compile(new CNF(dep)), compile(new CNF(indep)), msg))
    }
  }

  /**
   * Same as tryIndependentSubtheories but try shattering before.
   */
  def tryIndependentSubtheoriesAfterShattering(cnf: CNF): Option[And] = {
    val shatteredCnf = shatter(cnf)
    if (cnf eq shatteredCnf) None
    else {
      tryIndependentSubtheories(shatteredCnf, true)
    }
  }

  // separate function to aid profiling
  def shatter(cnf: CNF) = cnf.shatter

  def onlyIfGround(f: InferenceRule)(cnf: CNF) = {
    if (cnf.isGround) f(cnf)
    else None
  }

  def tryGroundDecomposition(cnf: CNF): Option[Or] = {
    tryGroundDecomposition(cnf, false)
  }

  def tryGroundDecompositionCountShattered(cnf: CNF): Option[Or] = {
    tryGroundDecomposition(cnf, true)
  }

  def tryGroundDecomposition(cnf: CNF, countShatteredLiterals: Boolean): Option[Or] = {
    val countingCnf = if (countShatteredLiterals) shatter(cnf) else cnf
    val groundLiterals = countingCnf.clauses.flatMap { _.groundLiterals }
    if (groundLiterals.nonEmpty) {
      val groupedAtoms = groundLiterals.groupBy(a => a)
      val atomCounts = groupedAtoms.mapValues(list => list.size)
      val ordering = new Ordering[(Atom, Int)] {
        def compare(t1: (Atom, Int), t2: (Atom, Int)) = {
          t1._2 - t2._2
        }
      }
      val (literal, _) = atomCounts.max(ordering)
      val trueBranch = cnf + Clause(List(literal), List())
      val falseBranch = cnf + Clause(List(), List(literal))
      val msg = "Shannon decomposition on $" + literal.toLatex(new VarNameSpace) + "$."
      Some(new Or(cnf, compile(trueBranch), compile(falseBranch), msg))
    } else None
  }

  def tryInclusionExclusion(cnf: CNF) = {
    val decomposableClauseOption = cnf.clauses.find { _.independentLiterals.nonEmpty }
    if (decomposableClauseOption.nonEmpty) {
      val Some(clause) = decomposableClauseOption
      val otherClauses = cnf.clauses filterNot (_ == clause)
      val Some((cl1, cl2)) = clause.independentLiterals
      val plus1Branch = new CNF(cl1 :: otherClauses)
      val plus2Branch = new CNF(cl2 :: otherClauses)
      val minBranch = new CNF(cl1 :: cl2 :: otherClauses)
      val msg = "Inclusion-exclusion on $" + clause.toLatex() + "$."
      Some(new InclusionExclusion(cnf, compile(plus1Branch), compile(plus2Branch), compile(minBranch), msg))
    } else None
  }

  def tryShatter(cnf: CNF) = {
    val shatteredCnf = shatter(cnf)
    if (shatteredCnf eq cnf) None
    else Some(compile(shatteredCnf))
  }

  case class IndexedConstant(val i: Int) {
    override def toString = "x" + (if (i > 0) ("'" * i) else "")
  }

  def groundingConstant(i: Int, domain: Domain) = {
    val c = new Constant(new IndexedConstant(i))
    c.setDomain(domain)
  }

  def groundingConstantFor(cnf: CNF, domain: Domain) = {
    val existingIndices = cnf.constants.collect { case Constant(IndexedConstant(i)) => i }
    val newIndex = Stream.from(0).find { index => !existingIndices(index) }.get
    groundingConstant(newIndex, domain)
  }

  type ChoiceMap = collection.mutable.Map[Clause, Var]
  
  def tryIndependentPartialGrounding(cnf: CNF): Option[IndependentPartialGroundingNode] = {
    // in the future, this should be implemented  by finding all binding classes and then checking size and root
    if(cnf.clauses.exists(_.rootVars.isEmpty)) return None
    else {
      // every clause has a root variable -- we can try
      val chosenVariables = collection.mutable.Map.empty[Clause, Var]
      val (unaryClauses, multiClauses) = cnf.clauses.partition { _.literalVariables.size == 1 }
      // first choose the root variables of unary clauses. they cannot lead to conflicts
      for (c <- unaryClauses)  chosenVariables(c) = c.literalVariables.head
      var chosenClauses = unaryClauses
      // one by one add all variables that are the only roots in their clause
      val (singletonRoots, multiRoots) = multiClauses.partition { _.rootVars.size == 1 }
      for (c <- singletonRoots)  {
        if(!tryAddingVariable(chosenVariables, chosenClauses, c, c.rootVars.head)) return None
        chosenClauses = c :: chosenClauses
      }
      // now do search for the remainder
      val finalChoice = searchChoices(chosenVariables,chosenClauses,multiRoots.sortBy(_.rootVars.size))
      if(finalChoice.isEmpty) return None
      else{
    	  val solution = finalChoice.get
    	  val rootVars = solution.values.toSet
	      val rootVarDomains = rootVars.flatMap { rootVar =>
	        cnf.clauses.filter { _.literalVariables(rootVar) }.map { _.constrs.domainFor(rootVar) }
	      }.toSet
	      assume(rootVarDomains.size == 1) //because of independence
	      val rootVarDomain = rootVarDomains.head
	      val constant = groundingConstantFor(cnf, rootVarDomain)
	      def substituteRootVars(v: Var): Term = {
	        if (rootVars.contains(v)) constant
	        else v
	      }
	      val invertedClauses = cnf.clauses.map { clause =>
	        val substitutedClause = clause.substitute(solution(clause), constant)
	        substitutedClause
	      }
	      val invertedCNF = new CNF(invertedClauses)
	      val rootVarIneqs = cnf.clauses.flatMap { clause =>
	        clause.constrs.ineqConstrs(solution(clause)).collect { case c: Constant => c }
	      }.toSet
	      val msg = ("""Independent partial grounding of $ X \in """ + rootVarDomain + """ $""" +
	        (if (rootVarIneqs.isEmpty) "." else """, $ """ + rootVarIneqs.map { """X \neq """ + _.toString }.mkString(" , ") + " $."))
	      val inversionNode = new IndependentPartialGroundingNode(cnf, compile(invertedCNF),
	        constant, rootVarIneqs, rootVarDomain, msg)
	      Some(inversionNode)
      }
    }
  }

 /**
  * Optimization to reduce the size of the search tree: propagate choices
  */
 private[this] def searchChoices(choices: ChoiceMap, 
		  					      chosenClauses: List[Clause], 
	  							  otherClauses: List[Clause]): Option[ChoiceMap] = {
    if(otherClauses.isEmpty) Some(choices)
    else{
      val clause :: tailClauses = otherClauses
      val roots = clause.rootVars
      for(root <- roots){
        val newChoices = choices.clone
        if(tryAddingVariable(newChoices,chosenClauses,clause,root)){
          // now also assign variables from clauses that depend on the just assigned clause (requires no search)
          val (dependentClauses, independentClauses) = tailClauses.partition(_ dependent clause)
          val newChosenClausesOption = propagateBindingClass(newChoices, clause, root, 
        		  											 clause :: chosenClauses, dependentClauses)
          if(newChosenClausesOption.nonEmpty){
	          val completeChoices = searchChoices(newChoices,newChosenClausesOption.get,independentClauses)
	          if(completeChoices.nonEmpty) return completeChoices
          }// else try next root
        }// else try next root
      }
      return None
    }
  }
 
  private[this] def propagateBindingClass(choices: ChoiceMap, 
		  								  clause: Clause, root: Var,
		  								  chosenClauses: List[Clause], dependentClauses: List[Clause]
		  								 ): Option[List[Clause]] = {
    var newChosenClauses = chosenClauses
    for(depClause <- dependentClauses){
      val bindingVariable = boundVars(clause, root, depClause)
      require(bindingVariable.size > 0, "Input dependentClauses have to depend on clause")
      if(bindingVariable.size != 1) return None
      val newRoot = bindingVariable.head
      if(!depClause.rootVars(newRoot)) return None
      if(!tryAddingVariable(choices, newChosenClauses, depClause, newRoot)) return None
      newChosenClauses = depClause :: newChosenClauses
    }
    return Some(newChosenClauses)
  }
  
  private[this] def tryAddingVariable(choices: ChoiceMap, chosenClauses: List[Clause], 
		  								clause: Clause, v: Var): Boolean = {
    require(clause.rootVars(v))
    require(!choices.contains(clause))
    // check if v appears in two incompatible positions in clause
    // for example: friends(x,y) => friends(y,x), x != y.
    // where x and y do not unify
    val boundSelfVars = boundVars(clause, v, clause.standardizeApart)
    assume(boundSelfVars.size >= 1, "v should at least unify with its copy v'")
    if (boundSelfVars.size > 1) {
      return false // v unifies with more than its copy v' when standardizing apart
    }
    for(chosenClause <- chosenClauses){
      if(chosenClause dependent clause){
	      val chosenClauseVar = choices(chosenClause)
	      val bindingInNewClause = boundVars(chosenClause, chosenClauseVar, clause)
	      assume(bindingInNewClause.size > 0)
	      if(bindingInNewClause.size > 1 || !bindingInNewClause.contains(v)) {
	        return false // bad bindings in new clause
	      }
	      val bindingInChosenClause = boundVars(clause, v, chosenClause)
	      assume(bindingInChosenClause.size > 0)
	      if(bindingInChosenClause.size > 1 || !bindingInChosenClause.contains(chosenClauseVar)) {
	        return false // bad bindings in already chosen clause
	      }
      }
    }
    choices(clause) = v
    return true
  }

  // function to list all variables bound
  private[this] def boundVars(c1: Clause, c1Var: Var, c2: Clause): Set[Var] = {
    val equivalences = c1.atoms.flatMap { lit: Atom =>
      c2.atoms.flatMap { atom2: Atom =>
        atom2.unifyConstrained(
          lit,
          c1.constrs,
          c2.constrs).getOrElse(List())
      }
    }
    equivalences.filter { eq => eq(c1Var) }.foldLeft(Set[Var]()) { _ union _.variables } - c1Var
  }
  
  def tryCounting(cnf: CNF) = {
    val singletons = cnf.clauses.flatMap { clause =>
      clause.singletonLiterals.map { literal =>
        (clause, literal, clause.constrs)
      }
    }
    if (singletons.nonEmpty) {
      // the heuristic is: split on the atom with highest #occurences - domain size
      val groupedAtoms = singletons.map {
        case (clause1, lit1, constrs1) =>
          // ordering by occurence
          val nbOccurence = singletons.count {
            case (clause2, lit2, constrs2) =>
              (clause1 ne clause2) && lit1.unifies(lit2, constrs2, constrs1)
          }
          val domain = constrs1.domainFor(lit1.variables.head)
          val domainSize = sizeHint(domain.root)
          (lit1, constrs1, nbOccurence - domainSize)
      }
      //			val (bestLit, bestineqs, bestElems, _) = groupedAtoms.sortBy(_._4).last
      val ordering = new Ordering[(Atom, Constraints, Int)] {
        def compare(t1: (Atom, Constraints, Int), t2: (Atom, Constraints, Int)) = {
          t1._3 - t2._3
        }
      }
      val (bestLit, bestConstrs, _) = groupedAtoms.max(ordering)
      val unitConstrs = bestConstrs.project(bestLit.variables) // removed .constantIneqs - why needed?
      val logVar = bestLit.variables.head
      val excludedConstants = unitConstrs.ineqConstrs(logVar).map { _.asInstanceOf[Constant] }
      val domain = unitConstrs.domainFor(logVar)
      val singletonName = Clause(List(bestLit), List(), unitConstrs).toLatex()
      val splitIndex = domain.nbSplits + 1
      val subdomain = domain.subdomain(""" \top """, """ \bot """, splitIndex.toString, splitIndex.toString, excludedConstants.toSet)
      val msg = "Atom counting on $" + singletonName + "$."
      // shatter when needed
      // cnf.shatterDomain(domain, subdomain, subdomain.complement).clauses
      val trueUnitClause = Clause(List(bestLit), List(), unitConstrs.setDomain(logVar, subdomain)).standardizeApart
      val falseUnitClause = Clause(List(), List(bestLit), unitConstrs.setDomain(logVar, subdomain.complement)).standardizeApart
      val childCNF = new CNF(trueUnitClause :: falseUnitClause :: cnf.clauses)
      Some(new CountingNode(cnf, compile(childCNF), domain, subdomain, msg))
    } else None
  }

  def inferenceRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    //			tryRemoveDoubleClauses,
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    //			tryGroundDecompositionCountShattered,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding,
    tryCounting)

}
