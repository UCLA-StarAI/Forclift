/*
 * Copyright 2015 Guy Van den Broeck and Wannes Meert
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

package liftedinference.bugs

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec
import liftedinference._
import liftedinference.examples.models._
import liftedinference.inference.AllMarginalsExact
import liftedinference.languages.focnf.FOCNFParser
import liftedinference.inference.QueryProbExact
import liftedinference.inference.QueryProbC2D
import liftedinference.inference.WeightedCNF
import liftedinference.languages.ModelConverters._
import liftedinference.languages.WeightedCNFParser
import liftedinference.languages.FactorGraphParser
import liftedinference.languages.mln.MLNParser

@RunWith(classOf[JUnitRunner])
class TestBug22 extends ModelBehaviours {

	describe("Bug22Model") {

		val theoryString = s"""
person = {alice, bob,charlie}
friends(person,person)
0.5 !friends(x,y) v friends(y,x)
"""

		val parser = new MLNParser
		val model: WeightedCNF = parser.parseModel(theoryString + "\n")
		val query = parser.parseAtom("friends(alice,misterx)").toPositiveUnitClause


  it("An exception should be thrown when querying with additional constant (lifted)") {
			intercept[IllegalArgumentException] { 
				val algo = new QueryProbExact(false)
				algo.computeQueryProb(model, query)
			}
		}

		it("An exception should be thrown when querying with additional constant (propositional)") {
			intercept[IllegalArgumentException] { 
				val algo = new QueryProbC2D(false)
				algo.computeQueryProb(model, query)
			}
		}
	}

}
