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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec
import org.scalatest.FunSpec
import edu.ucla.cs.starai.forclift.cli.CLI

@RunWith(classOf[JUnitRunner])
class TestScalaTest extends FunSpec with Matchers {

  describe("ScalaTest") {

    it("should run correctly") {
      1 + 1 should be(2)
    }

    it("should run not incorrectly") {
      1 + 1 should not be (3)
    }
    
    it("should have assertions enabled in test classes") {
      intercept[AssertionError] {
    	Predef.assert(false, "Assertions are enabled in test classes")
      }
    }
    
    it("should have assertions enabled in main classes") {
      intercept[AssertionError] {
    	CLI.assertFalse
      }
    }
    
  }

}
