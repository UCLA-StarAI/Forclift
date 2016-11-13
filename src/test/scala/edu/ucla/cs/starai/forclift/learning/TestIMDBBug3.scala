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

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.tagobjects.Slow
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import org.scalatest.Finders

@RunWith(classOf[JUnitRunner])
class TestIMDBBug3 extends FunSpec with Matchers with ResourceParseHelper {
  
  //--------------------------------------------------------------------------
  println("Running from directory:")
  println(System.getProperty("user.dir"))
  var mln  = MLN()
  var db   = MLN()

  //--------------------------------------------------------------------------
  
	describe("IMDB bug3") {

		it("MLN and DB are parsable") {
			parse("/imdb/imdb.bug3.mln","/imdb/imdb.1.db") match {
					case (m,d) => mln=m;db=d
			}
		}
		
//		it("Compilable") {
//			mln.toWeightedCNF(false).showNnfPdf(false, 10, "bug", true)
//		}
		

		it("Learnable", Slow) {
			val learner = new LiftedLearning(mln, Seq(db), verbose=true)
			val learnedMLN = learner.learnParameters()
			println(learnedMLN)
		}
	}

}
