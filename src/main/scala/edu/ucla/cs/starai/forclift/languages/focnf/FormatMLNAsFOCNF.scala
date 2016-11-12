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

import java.io._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import edu.ucla.cs.starai.forclift.languages.mln.MLNNameSpace
import edu.ucla.cs.starai.forclift.util.NameSpace

/**
 * Conversion from MLN to FO-CNF.
 */
case class FormatMLNAsFOCNF (
  val mln: MLN, 
  verbose: Boolean = false) {
  
  // TODO: uses skolemization because there is no transformation to prenex
  //       clauses for general formulas with existential quantifiers
  val wcnf = mln.toWeightedCNFWithoutSplitting(verbose=false, skolemize=true)
  val focnf = {
    val formulas = wcnf.cnf.clauses.map{clause =>
      val lits:List[Formula] = 
        clause.posLits.map(l => LiteralFormula(l)) :::
        clause.negLits.map(l => NegFormula(LiteralFormula(l)))
      val rlits = lits.reverse
      rlits.tail.foldLeft(rlits.head){(r,c) => new DisjFormula(c,r)}
    }
    FOCNF(formulas,
          wcnf.cnf.predicates,
          wcnf.domainSizes,
          wcnf.predicateWeights,
          Nil)
  }
}
