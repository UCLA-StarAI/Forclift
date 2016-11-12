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

package edu.ucla.cs.starai.forclift.learning

import edu.ucla.cs.starai.forclift._
import constraints._

/**
 * Count the number of positive and negative groundings for a formula
 * respresented by `res` given the CNF (`cnf`) a database (`db`).
 *
 * @param  db
 * @param  res
 * @param  cnf
 * @param  verbose
 * @param  evidenceAtom
 *         Alter the given database with this atom and its value.
 */
class FormulaCounter(
  db: Database,
  res: Predicate,
  cnf: CNF,
  verbose: Boolean,
  evidenceAtom: Option[(Atom, Boolean)] = None) {

  val nbGroundings = {
    res.toAtom.toPositiveUnitClause.nbGroundings(db.domainSizes)
  }

  val nbTrueGroundings = calcTrueGroundings(evidenceAtom)

  /**
   * Separate function to calculate true groundings to allow for calculating
   * also the true groundings for databases that have one atom differently
   * like for pseudo-likelihood
   *
   * @param  evAtom
   *         If given the list of evidence is extended with the given
   *         evidence atom.
   */
  def calcTrueGroundings(evAtom: Option[(Atom, Boolean)] = None) = {

    //println("res = %s" format res.toString)
    //db.addAdditionalConstants()
    // remove clauses where res is positive, because it cannot be used to prove false groundings 
    val negCNF = cnf.clauses.filter { c =>
      c.negLits.exists { a: Atom =>
        a.predicate == res
      }
    }
    // TODO Move the calculations not depending on evAtom to a lazy val
    //      outside of this function.
    require(negCNF.size == 1, "Assume a clause")
    val negClause = negCNF.head
    //println("negClause = %s" format negClause.toString)
    val resAtom = negClause.negLits.find { _.predicate == res }.get
    //println("resAtom = %s" format resAtom.toString)
    //val domains = negClause.constrs.domains
    val explcDomains = collection.mutable.Map.empty[Var, Set[Constant]]
    for (v <- resAtom.variables) {
      val excludedConstants = negClause.constrs.differentConstants(v)
      //explcDomains(v) = negClause.constrs.domainFor(v).constants(db.domainSizes, excludedConstants).toSet
      explcDomains(v) = db.domainSizes.constants(negClause.constrs.domainFor(v), excludedConstants).toSet
      //if (verbose) {
      //println("    * original domain for " + v + " has size " + explcDomains(v).size)
      //}
    }
    //println("explcDomains = %s" format explcDomains.toString)
    // List of literals that, if they all are false, force the formula to
    // be false.
    val posLits = negClause.posLits.map { l =>
      val subDb = db.subDb(collection.immutable.Set(l.predicate))
      val subDbExt = evAtom match {
        case Some((atom, pos)) if atom.predicate == l.predicate => {
          if (pos) subDb + atom
          else subDb - atom
        }
        case _ => subDb
      }
      // TODO: filter to be checked, can this more efficiently?
      Lit(l, true, subDbExt filter { _.unify(l).nonEmpty })
    }
    val negLits = negClause.negLits.filterNot { _.predicate == res }.map { l =>
      val subDb = db.subDb(collection.immutable.Set(l.predicate))
      val subDbExt = evAtom match {
        case Some((atom, pos)) if atom.predicate == l.predicate => {
          if (pos) subDb + atom
          else subDb - atom
        }
        case _ => subDb
      }
      // TODO: filter to be checked, can this more efficiently?
      Lit(l, false, subDbExt filter { _.unify(l).nonEmpty })
    }
    //println("posLits = %s" format posLits.toString)
    //println("negLits = %s" format negLits.toString)
    val nbFalseGroundings = countFalseGroundings(explcDomains, posLits ++ negLits, negClause.constrs)
    //println("nbFalseGroundings = %d" format nbFalseGroundings)
    assume(nbGroundings >= nbFalseGroundings, "Number of groundings is smaller than the false groundings")
    //remove constants again
    //db.removeAdditionalConstants()
    nbGroundings - nbFalseGroundings
  }

  type Domains = collection.mutable.Map[Var, Set[Constant]]

  case class Lit(atom: Atom, pos: Boolean, evidence: Set[Atom])

  def countFalseGroundings(domains: Domains, lits: List[Lit], constrs: Constraints): GInt = {
    //println("constrs = %s" format constrs.toString)
    if (domains.values.exists(_.isEmpty)) 0
    else if (domains.isEmpty) throw new IllegalArgumentException
    else {
      // first reduce the domains
      for (v <- domains.keySet) {
        // remove ineq constants which might have been added by grounding
        domains(v) --= constrs.differentConstants(v)
      }
      for (Lit(atom, pos, evidence) <- lits) {
        if (pos) {
          if (atom.variables.size == 1) {
            for (i <- 0 until atom.args.size) {
              var relevantEvidence = evidence
              if (atom.args(i).isInstanceOf[Constant]) {
                relevantEvidence = relevantEvidence.filter(_.args(i) == atom.args(i))
              } else { // it's THE Var
                // literal is positive, so to prove not res, it has to be false in the db
                // remove all constants that have no false entry in the db
                // for now, only check arity one
                val v = atom.args(i).asInstanceOf[Var]
                val certainlyTrueForConstant = evidence.map { _.args(i).asInstanceOf[Constant] }
                //                                if (verbose) {
                //                                    println("    * removing from domain for " + v + " because " + atom + " has to be false: " + certainlyTrueForConstant)
                //                                }
                domains(v) --= certainlyTrueForConstant
              }
            }
          }
        } else {
          for (i <- 0 until atom.args.size if atom.args(i).isInstanceOf[Var]) {
            // literal is negative, so to prove not res, it has to be true in the db
            // remove all constants that have no true entry in the db
            val v = atom.args(i).asInstanceOf[Var]
            val possiblyTrueForConstant = evidence.map { _.args(i).asInstanceOf[Constant] }
            //                        if (verbose) {
            //                            println("    * retaining from domain for " + v + " because " + atom + " has to be true: " + possiblyTrueForConstant)
            //                        }
            domains(v) = domains(v) intersect possiblyTrueForConstant
            if (domains(v).exists { !possiblyTrueForConstant(_) }) {
              throw new IllegalStateException
            }
          }
        }
      }
      if (domains.size == 1) {
        //println(" one domain left: "+domains.first)
        domains.head._2.size
      } else {
        val (grVar, grConsts) = domains.minBy { _._2.size }
        var count: GInt = 0;
        for (c <- grConsts) {
          // copy domains
          val newDomains = (domains - grVar)
          val newLits = lits.map {
            case Lit(atom, sign, evidence) =>
              var newEvidence = evidence
              for (i <- atom.args.indices if atom.args(i) == grVar) {
                // clean up evidence
                newEvidence = evidence.filter { e =>
                  val eValue = e.args(i)
                  //                                    if(eValue != c ) println( "removing "+e+" bc "+e.args(i)+"!="+c)
                  eValue == c
                }
              }
              val newAtom = atom.substitute(v => if (v == grVar) c else v)
              //                            println("New evidence for "+newAtom+" = "+newEvidence)
              Lit(newAtom, sign, newEvidence)
          }
          val newConstrs = constrs.substitute(v => if (v == grVar) c else v)
          val subCount = countFalseGroundings(newDomains, newLits, newConstrs)
          //                    println("count for " + newLits.map { l => if (l.pos) l.atom else "!" + l.atom }.mkString(" v "))
          //                    println("is " + subCount)
          count += subCount
        }
        count
      }
    }
  }

  require(nbTrueGroundings <= nbGroundings, nbTrueGroundings + ">" + nbGroundings)
  def nbFalseGroundings = nbGroundings - nbTrueGroundings

}

//class FormulaCounter(db: Database, res: Predicate, cnf: CNF, verbose: Boolean) {
//
//    val nbGroundings = {
//        res.toAtom.toPositiveUnitClause.nbGroundings(db.domainSizes)
//    }
//
//    val nbTrueGroundings = {
//        // Temporarily add constants found only in the database
//        db.addAdditionalConstants()
//        // remove clauses where res is positive, because it cannot be used to prove false groundings 
//        val negCNF = cnf.clauses.filter { c =>
//            c.negLits.exists { a: Atom =>
//                a.predicate == res
//            }
//        }
//        require(negCNF.size == 1, "Assume a clause")
//        val negClause = negCNF.first
//        val resAtom = negClause.negLits.find { _.predicate == res }.get
//        val vars = resAtom.variables
//        val domains = negClause.elemConstrs
//        val explcDomains = collection.mutable.Map.empty[Var, collection.mutable.Set[Constant]]
//        for (v <- vars) {
//            val excludedConstants = negClause.ineqConstrs(v).collect { case c: Constant => c }
//            explcDomains(v) = collection.mutable.Set.empty[Constant]
//            explcDomains(v) = explcDomains(v) ++ negClause.elemConstrs(v).constants(db.domainSizes, excludedConstants).toSet
//            if (verbose) {
//                println("    * original domain for " + v + " has size " + explcDomains(v).size)
//            }
//        }
//        for (posLit <- negClause.posLits if posLit.predicate != res) {
//            val evidence = db.subDb(Set(posLit.predicate))
//            for (i <- 0 until posLit.args.size if posLit.args(i).isInstanceOf[Var]) {
//                // literal is positive, so to prove not res, it has to be false in the db
//                // remove all constants that have no false entry in the db
//                // for now, only check arity one
//                if (posLit.args.size == 1) {
//                    val v = posLit.args(i).asInstanceOf[Var]
//                    val certainlyTrueForConstant = evidence.map { _.args(i).asInstanceOf[Constant] }
////                    if (verbose) {
////		                println("    * removing from domain for " + v + " because " + posLit + " has to be false: " + certainlyTrueForConstant)
////		            }
//                    explcDomains(v) = explcDomains(v) -- certainlyTrueForConstant
//                }
//            }
//        }
//        for (negLit <- negClause.negLits if negLit.predicate != res) {
//            val evidence = db.subDb(Set(negLit.predicate))
//            for (i <- 0 until negLit.args.size if negLit.args(i).isInstanceOf[Var]) {
//                // literal is negative, so to prove not res, it has to be true in the db
//                // remove all constants that have no true entry in the db
//                val v = negLit.args(i).asInstanceOf[Var]
//                val possiblyTrueForConstant = evidence.map { _.args(i).asInstanceOf[Constant] }
//                explcDomains(v) = explcDomains(v) intersect possiblyTrueForConstant
//            }
//        }
//        for (v <- vars) {
//            if (verbose) {
//                println("    * reduced domain for " + v + " has size " + explcDomains(v).size)
//            }
//        }
//        def ground(clauses: List[Clause], vars: Set[Var]): List[Clause] = {
//            if (vars.isEmpty) clauses
//            else {
//                val v = vars.first
//                val rest = vars - v
//                ground(clauses.flatMap { _.ground(v, db.domainSizes, explcDomains(v)) }, rest)
//            }
//        }
//        val groundCNF = ground(negCNF, vars)
//        assume(groundCNF.forall { clause =>
//            clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//        })
//        val specificEvidence = db.subDb(cnf.predicates - res)
//        val conditionedGroundCnf = specificEvidence.foldLeft(new CNF(groundCNF)) {
//            (cnf, atom) => cnf.condition(atom.toPositiveUnitClause)
//        }
//        val falseRes: Set[Atom] = conditionedGroundCnf.clauses.filter { _.negLits.size == 1 }.map { c =>
//            val resAtom: Atom = c.negLits.first
//            assume(resAtom.predicate == res)
//            resAtom
//        }.toSet
//        // this assumes that atom has implemented equality for ground atoms
//        // therefore, convert PositiveUnitClause to Atom
//        val nbFalseGroundings = falseRes.size
//        assume(nbGroundings >= nbFalseGroundings)
//        //remove constants again
//        db.removeAdditionalConstants()
//        nbGroundings - nbFalseGroundings
//    }
//
//    //                        val nbTrueGroundings = {
//    //                            // Temporarily add constants found only in the database
//    //                            db.addAdditionalConstants()
//    //                            val groundCnf = cnf.ground(db.domainSizes)
//    //                            // @todo this assumes there is only positive evidence (because
//    //                            //       of how subDb is defined).
//    //                            val specificEvidence = db.subDb(mlnPredicates)
//    //                            val conditionedGroundCnf = specificEvidence.foldLeft(groundCnf) {
//    //                                (cnf, atom) => cnf.condition(atom.toPositiveUnitClause)
//    //                            }
//    //                            assume(conditionedGroundCnf.clauses.forall { clause =>
//    //                                clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    //                            })
//    //                            // after conditioning on positive literals, a formula is true
//    //                            // if the other literals in the clause are false, 
//    //                            // which means that negLits is empty
//    //                            // this assumes that atom has implemented equality for ground atoms
//    //                            val trueGroundings = conditionedGroundCnf.clauses.filter { _.negLits.isEmpty }.map {
//    //                                _.posLits.find { _.predicate == res }.get
//    //                            }.toSet
//    //                            val nbTrueGroundings = trueGroundings.size
//    //                            assume {
//    //                                val falseGroundings = conditionedGroundCnf.clauses.filter { clause =>
//    //                                    clause.negLits.size == 1 && clause.negLits.first.predicate == res
//    //                                }.map {
//    //                                    _.negLits.head
//    //                                }.toSet
//    //                                (nbGroundings - nbTrueGroundings) == falseGroundings.size
//    //                            }
//    //                            //remove constants again
//    //                            db.removeAdditionalConstants()
//    //                            nbTrueGroundings
//    //                        }
//    //            // new lifted version
//    //            // counts true groundings
//    //            val nbTrueGroundings = {
//    //                // Temporarily add constants found only in the database
//    //                db.addAdditionalConstants()
//    //                if (verbose) {
//    //                    println("-  in " + db)
//    //                }
//    //                // remove clauses where res is negative, because it cannot be used to prove true groundings 
//    //                val positiveCNF = new CNF(cnf.clauses.filter { c =>
//    //                    val positive = c.posLits.exists { a: Atom =>
//    //                        a.predicate == res
//    //                    }
//    ////                    println("    * res "+res)
//    ////                    if(positive) println("    * keeping "+c)
//    ////                    else println("    * removing "+c)
//    //                    positive
//    //                })
//    //                if (verbose) {
//    //                    println("    * reduced " + cnf.size + " clauses to positive CNF with " + positiveCNF.size + " clauses")
//    //                }
//    //                // @todo this assumes there is only positive evidence (because
//    //                //       of how subDb is defined).
//    //                val specificEvidence = db.subDb(mlnPredicates)
//    //                val conditionedPosCNFCnf = specificEvidence.foldLeft(positiveCNF) {
//    //                    (cnf, atom) => cnf.condition(atom.toPositiveUnitClause)
//    //                }
//    //                // after conditioning on positive literals, a formula is true
//    //                // if the other literals in the clause are false, 
//    //                // which means that negLits is empty
//    //                val trueRes: List[PositiveUnitClause] = conditionedPosCNFCnf.clauses.filter { _.negLits.isEmpty }.map { c =>
//    //                    val resAtom: Atom = c.posLits.find { _.predicate == res }.get
//    //                    val trueRes = new PositiveUnitClause(resAtom,c.ineqConstrs,c.elemConstrs)
//    //                    trueRes
//    //                }
//    //                if (verbose) {
//    //                    println("    * true res: "+trueRes.size)//.mkString(", "))
//    //                }
//    ////                        
//    ////                val groundTrueUnitClauses = trueRes.ground(db.domainSizes)
//    ////                assume(groundTrueUnitClauses.forall { clause =>
//    ////                    clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    ////                })
//    ////                // this assumes that atom has implemented equality for ground atoms
//    ////                // therefore, convert PositiveUnitClause to Atom
//    ////                val trueGroundings = groundTrueUnitClauses.map{_.toPositiveUnitClause.atom}.toSet
//    ////                
//    ////                if (verbose) {
//    ////                    println("    * ground true res: "+trueGroundings.size)//.mkString(", "))
//    ////                }
//    //                      
//    //                def makeDisjoint(catoms: List[PositiveUnitClause]): List[PositiveUnitClause] = catoms match {
//    //		            case Nil => Nil
//    //		            case catom :: rest => catom :: makeDisjoint(rest.flatMap { _.minus(catom) })
//    //		        }  
//    //                val disjointTrueRes = makeDisjoint(trueRes)
//    //                if (verbose) {
//    //                    println("    * disjoint true res: "+disjointTrueRes.size)//.mkString(", "))
//    //                }
//    //                val nbTrueGroundings = disjointTrueRes.foldLeft(0){_ + _.nbGroundings(db.domainSizes)}
//    ////                assume {
//    ////                    val falseGroundings = conditionedGroundCnf.clauses.filter { clause =>
//    ////                        clause.negLits.size == 1 && clause.negLits.first.predicate == res
//    ////                    }.map {
//    ////                        _.negLits.head
//    ////                    }.toSet
//    ////                    (nbGroundings - nbTrueGroundings) == falseGroundings.size
//    ////                }
//    //                //remove constants again
//    //                db.removeAdditionalConstants()
//    //                nbTrueGroundings
//    //            }
//    //            // new lifted version
//    //            // counts false groundings
//    //            val nbTrueGroundings = {
//    //                // Temporarily add constants found only in the database
//    //                db.addAdditionalConstants()
//    //                val res.args
//    //                if (verbose) {
//    //                    println("-  in " + db)
//    //                }
//    //                // remove clauses where res is negative, because it cannot be used to prove true groundings 
//    //                val negativeCNF = new CNF(cnf.clauses.filter { c =>
//    //                    val negative = c.negLits.exists { a: Atom =>
//    //                        a.predicate == res
//    //                    }
//    ////                    println("    * res "+res)
//    ////                    if(positive) println("    * keeping "+c)
//    ////                    else println("    * removing "+c)
//    //                    negative
//    //                })
//    //                if (verbose) {
//    //                    println("    * reduced " + cnf.size + " clauses to negative CNF with " + negativeCNF.size + " clauses")
//    //                }
//    //                // @todo this assumes there is only positive evidence (because
//    //                //       of how subDb is defined).
//    //                val specificEvidence = db.subDb(mlnPredicates)
//    //                val conditionedNegCNFCnf = specificEvidence.foldLeft(negativeCNF) {
//    //                    (cnf, atom) => cnf.condition(atom.toPositiveUnitClause)
//    //                }
//    //                // after conditioning on positive literals, a formula is true
//    //                // if the other literals in the clause are false, 
//    //                // which means that negLits is empty (except for res)
//    //                val falseRes: List[PositiveUnitClause] = conditionedNegCNFCnf.clauses.filter { _.negLits.size == 1 }.map { c =>
//    //                    val resAtom: Atom = c.negLits.first
//    //                    assume(resAtom.predicate == res)
//    //                    val falseRes = new PositiveUnitClause(resAtom,c.ineqConstrs,c.elemConstrs)
//    //                    falseRes
//    //                }
//    //                val upperBoundNbFalse = falseRes.foldLeft(0){_ + _.nbGroundings(db.domainSizes)}
//    //                
//    //                if (verbose) {
//    //                    println("    * fo false atoms: "+falseRes.size)//.mkString(", "))
//    //                    println("    * nb false upper bound: "+upperBoundNbFalse)//.mkString(", "))
//    //                }
//    //                
//    ////                 remove clauses where res is negative, because it cannot be used to prove true groundings 
//    //                val positiveCNF = new CNF(cnf.clauses.filter { c =>
//    //                    val positive = c.posLits.exists { a: Atom =>
//    //                        a.predicate == res
//    //                    }
//    ////                    println("    * res "+res)
//    ////                    if(positive) println("    * keeping "+c)
//    ////                    else println("    * removing "+c)
//    //                    positive
//    //                })
//    //                if (verbose) {
//    //                    println("    * reduced " + cnf.size + " clauses to positive CNF with " + positiveCNF.size + " clauses")
//    //                }
//    //                // @todo this assumes there is only positive evidence (because
//    //                //       of how subDb is defined).
//    //                val conditionedPosCNFCnf = specificEvidence.foldLeft(positiveCNF) {
//    //                    (cnf, atom) => cnf.condition(atom.toPositiveUnitClause)
//    //                }
//    //                // after conditioning on positive literals, a formula is true
//    //                // if the other literals in the clause are false, 
//    //                // which means that negLits is empty
//    //                val trueRes: List[PositiveUnitClause] = conditionedPosCNFCnf.clauses.filter { _.negLits.isEmpty }.map { c =>
//    //                    val resAtom: Atom = c.posLits.find { _.predicate == res }.get
//    //                    val trueRes = new PositiveUnitClause(resAtom,c.ineqConstrs,c.elemConstrs)
//    //                    trueRes
//    //                }
//    //                val upperBoundNbTrue = trueRes.foldLeft(0){_ + _.nbGroundings(db.domainSizes)}
//    //                if (verbose) {
//    //                    println("    * fo false atoms: "+trueRes.size)//.mkString(", "))
//    //                    println("    * nb true upper bound: "+upperBoundNbTrue)//.mkString(", "))
//    //                }   
//    //                
//    //                val nbTrueGroundings = if(upperBoundNbTrue<upperBoundNbFalse){
//    //                    val groundTrueUnitClauses = trueRes.flatMap{_.ground(db.domainSizes)}
//    //	                assume(groundTrueUnitClauses.forall { clause =>
//    //	                    clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    //	                })
//    //	                // this assumes that atom has implemented equality for ground atoms
//    //	                // therefore, convert PositiveUnitClause to Atom
//    //	                val trueGroundings = groundTrueUnitClauses.map{_.toPositiveUnitClause.atom}.toSet	                
//    //	                if (verbose) {
//    //	                    println("    * ground true res: "+trueGroundings.size)//.mkString(", "))
//    //	                }
//    //	                trueGroundings.size
//    //                }else{
//    //                    val groundFalseUnitClauses = falseRes.flatMap{_.ground(db.domainSizes)}
//    //	                assume(groundFalseUnitClauses.forall { clause =>
//    //	                    clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    //	                })
//    //	                // this assumes that atom has implemented equality for ground atoms
//    //	                // therefore, convert PositiveUnitClause to Atom
//    //	                val falseGroundings = groundFalseUnitClauses.map{_.toPositiveUnitClause.atom}.toSet	                
//    //	                if (verbose) {
//    //	                    println("    * ground false res: "+falseGroundings.size)//.mkString(", "))
//    //	                }
//    //	                assume(nbGroundings >= falseGroundings.size)
//    //	                nbGroundings - falseGroundings.size                    
//    //                }
//    ////                val groundTrueUnitClauses = trueRes.ground(db.domainSizes)
//    ////                assume(groundTrueUnitClauses.forall { clause =>
//    ////                    clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    ////                })
//    ////                // this assumes that atom has implemented equality for ground atoms
//    ////                // therefore, convert PositiveUnitClause to Atom
//    ////                val trueGroundings = groundTrueUnitClauses.map{_.toPositiveUnitClause.atom}.toSet
//    ////                
//    ////                if (verbose) {
//    ////                    println("    * ground true res: "+trueGroundings.size)//.mkString(", "))
//    ////                }
//    //                
//    //                
//    ////                val groundTrueUnitClauses = trueRes.ground(db.domainSizes)
//    ////                assume(groundTrueUnitClauses.forall { clause =>
//    ////                    clause.ineqConstrs.isEmpty && clause.elemConstrs.isEmpty
//    ////                })
//    ////                // this assumes that atom has implemented equality for ground atoms
//    ////                // therefore, convert PositiveUnitClause to Atom
//    ////                val trueGroundings = groundTrueUnitClauses.map{_.toPositiveUnitClause.atom}.toSet
//    ////                
//    ////                if (verbose) {
//    ////                    println("    * ground true res: "+trueGroundings.size)//.mkString(", "))
//    ////                }
//    ////                      
//    ////                def makeDisjoint(catoms: List[PositiveUnitClause]): List[PositiveUnitClause] = catoms match {
//    ////		            case Nil => Nil
//    ////		            case catom :: rest => catom :: makeDisjoint(rest.flatMap { _.minus(catom) })
//    ////		        }  
//    ////                val disjointFalseRes = makeDisjoint(falseRes)
//    ////                if (verbose) {
//    ////                    println("    * disjoint false res: "+disjointFalseRes.size)//.mkString(", "))
//    ////                }
//    ////                val nbFalseGroundings = disjointFalseRes.foldLeft(0){_ + _.nbGroundings(db.domainSizes)}
//    ////                val nbTrueGroundings = nbGroundings - nbFalseGroundings
//    ////                assume {
//    ////                    val falseGroundings = conditionedGroundCnf.clauses.filter { clause =>
//    ////                        clause.negLits.size == 1 && clause.negLits.first.predicate == res
//    ////                    }.map {
//    ////                        _.negLits.head
//    ////                    }.toSet
//    ////                    (nbGroundings - nbTrueGroundings) == falseGroundings.size
//    ////                }
//    //                //remove constants again
//    //                db.removeAdditionalConstants()
//    //                nbTrueGroundings
//    //            }
//    require(nbTrueGroundings <= nbGroundings, nbTrueGroundings + ">" + nbGroundings)
//    def nbFalseGroundings = nbGroundings - nbTrueGroundings
//
//    if (verbose) {
//        println("    * has true count " + nbTrueGroundings)
//        println("    * has false count " + (nbGroundings - nbTrueGroundings))
//    }
//
//}
