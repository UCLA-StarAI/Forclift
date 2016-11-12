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

package edu.ucla.cs.starai.forclift.bugs

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference.AllMarginalsExact
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNFParser
import edu.ucla.cs.starai.forclift.inference.QueryProbExact
import edu.ucla.cs.starai.forclift.inference.QueryProbC2D
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.languages.ModelConverters._
import edu.ucla.cs.starai.forclift.languages.WeightedCNFParser
import edu.ucla.cs.starai.forclift.languages.FactorGraphParser
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser

@RunWith(classOf[JUnitRunner])
class TestBug22 extends ModelBehaviours {

  describe("Bug22Model") {

    val theoryString = s"""
person = {Alice,Bob,Charlie}
Friends(person,person)
0.5 !Friends(x,y) v Friends(y,x)
"""

    val parser = new MLNParser
    val model: WeightedCNF = parser.parseModel(theoryString + "\n")

    it("An exception should be thrown when posing a non-ground query (lifted)") {
      intercept[IllegalArgumentException] {
        val nonGroundQuery = parser.parseAtom("Friends(x,y)").toPositiveUnitClause
        val algo = new QueryProbExact(false)
        algo.computeQueryProb(model, nonGroundQuery)
      }
    }

    it("An exception should be thrown when posing a non-ground query (propositional)") {
      intercept[IllegalArgumentException] {
        val nonGroundQuery = parser.parseAtom("Friends(x,y)").toPositiveUnitClause
        val algo = new QueryProbC2D(false)
        algo.computeQueryProb(model, nonGroundQuery)
      }
    }


    it("An exception should be thrown when querying with additional constant (lifted)") {
      intercept[IllegalArgumentException] {
        val query = parser.parseAtom("Friends(Alice,Misterx)").toPositiveUnitClause
        val algo = new QueryProbExact(false)
        algo.computeQueryProb(model, query)
      }
    }

    it("An exception should be thrown when querying with additional constant (propositional)") {
      intercept[IllegalArgumentException] {
        val query = parser.parseAtom("Friends(Alice,Misterx)").toPositiveUnitClause
        val algo = new QueryProbC2D(false)
        algo.computeQueryProb(model, query)
      }
    }
  }

}
