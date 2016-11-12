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

@RunWith(classOf[JUnitRunner])
class TestBug16 extends ModelBehaviours {

  //    describe("Bug16Model a") {
  //
  //        def model = new WeightedCNFModel {
  //            def theoryString = """
  //domain Person 3 {p1,p2}
  //    
  //predicate s(Person,Person) 1.9 1
  //                
  //s(X,p2), X≠p1, X≠p2
  //s(X,p2), X≠p2
  //"""
  //        }
  //
  //        it("should shatter correctly") {
  //            println(model.theory.cnf.shatter)
  //            model.theory.cnf.shatter.size should be(3)
  //        }
  //
  //    }

  describe("Bug16Model b") {

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 3 {p1,p2}
    
predicate s(Person,Person) 1.9 1
                
s(p2,p1) 
s(p2,X), X≠p1, X≠p2
s(X,Y), X≠Y
s(X,Y), X≠p2, X≠Y
"""
    }

    it("should shatter correctly") {

      //Shatters to:
      //
      //s(X,Y), X≠p2, Y≠X, Y≠p1, 
      //
      //s(X,p1), X≠p1, X≠p2, 
      //
      //s(p2,X), X≠p1, X≠p2,
      //
      //s(p2,p1), 

      println(model.theory.cnf.shatter)
      model.theory.cnf.shatter.size should be > (model.theory.cnf.size)
    }

  }
}
