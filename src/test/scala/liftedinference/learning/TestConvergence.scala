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

package liftedinference.learning

import liftedinference._
import liftedinference.languages.mln._
import liftedinference.inference._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec
import java.io._
import scala.io._
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TestConvergence extends FunSpec with Matchers {

  describe("Convergence bug") {

    val theoryStr = Source.fromFile("./src/test/scala/liftedinference/learning/uwcse/convergencetest.mln").mkString
    val dbs = Seq("./src/test/scala/liftedinference/learning/uwcse/uwcse_fold1.db",
      "./src/test/scala/liftedinference/learning/uwcse/uwcse_fold3.db",
      "./src/test/scala/liftedinference/learning/uwcse/uwcse_fold4.db",
      "./src/test/scala/liftedinference/learning/uwcse/uwcse_fold5.db").map(f => Source.fromFile(f).mkString)

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(theoryStr)
    val db = dbs.map(parser.parseDB(_))
      
    var learnedMLN: MLN = null;

    it("should learn an MLN with nonzero weights") {
      learnedMLN = WeightLearning.learnWeights(
        mln,
        db,
        false,
        false)
      println(learnedMLN)
      learnedMLN.wformulas.filter(_.weight != 0) should be('nonEmpty);
    }
  }
}
