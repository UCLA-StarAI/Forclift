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

package edu.ucla.cs.starai.forclift.examples.models

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.languages._
import edu.ucla.cs.starai.forclift.languages.mln._
import scala.io.Source

trait StringModel {

  def theoryString: String

  val theory: WeightedCNF

}

trait FactorGraphModel extends StringModel {

  def theoryString: String

  val parser = new FactorGraphParser

  def catom(str: String): PositiveUnitClause = {
    theory // make sure domains and predicates are parsed
    val clause = parser.parseAtom(str)
    clause.toPositiveUnitClause
  }

  var compilerBuilder = Compiler.Builder.default

  lazy val theory = parser.parseFactorGraph(theoryString).toWeightedCNF(compilerBuilder)

}


@deprecated("Weighted CNF file format is being removed","3.0")
trait WeightedCNFModel extends StringModel {

  def theoryString: String

  val parser = new WeightedCNFParser
  
  def catom(str: String): PositiveUnitClause = {
    val clause = parser.parseClause(str)
    clause.toPositiveUnitClause
  }

  var compilerBuilder = Compiler.Builder.default

  lazy val theory: WeightedCNF  = parser.parseWeightedCNF(Source.fromString(theoryString), compilerBuilder)

}

trait MLNModel extends StringModel {

  def theoryString: String

  val parser = new MLNParser
  
//  OLD SETTINGS: NO SKOLEMIZATION
//  lazy val theory = parser.parseMLN(theoryString).toWeightedCNF()
  
  lazy val theory = parser.parseMLN(theoryString).toWeightedCNFWithoutSplitting(skolemize=true)

}
