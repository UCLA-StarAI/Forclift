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
class TestBug8Basic extends ModelBehaviours {

  /*
     * This test should actually be run both with inclusion-exclusion enabled and disables.
     */

  describe("Bug8Model - Basic") {

    val correctLogWMC = -1.3517419796132306 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
            domain Variable 2 {}
            domain Clause 3 {}
		     
    		predicate p(Variable) 0.5 0.5
                                    
	        predicate cv1(Clause,Variable) 0.5 0.5
	                                
	        predicate cs1(Clause) 0.5 0.5
	        
	        !cv1(C,V1) v !cs1(C) v p(V1)
	        
	        !cv1(C,V1) v !cv1(C,V2), V1!=V2
	"""
    }
    it should behave like verySmallModel(model, correctLogWMC)
  }

}

@RunWith(classOf[JUnitRunner])
class TestBug8Numerator extends ModelBehaviours {
  describe("Bug8Model - Numerator") {

    val correctLogWMC = -21.333553391529524 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
            domain Variable 4 {}
            domain Clause 6 {}
		     
    		predicate p(Variable) 0.5 0.5
	                                
	        predicate cv1(Clause,Variable) 0.5 0.5
	        predicate cv2(Clause,Variable) 0.5 0.5
	        predicate cv3(Clause,Variable) 0.5 0.5
	                                
	        predicate cs1(Clause) 0.5 0.5
	        predicate cs2(Clause) 0.5 0.5
	        predicate cs3(Clause) 0.5 0.5
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v !cs3(C) v p(V1) v p(V2) v p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v cs3(C) v p(V1) v p(V2) v !p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v !cs3(C) v p(V1) v !p(V2) v p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v cs3(C) v p(V1) v !p(V2) v !p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v !cs3(C) v !p(V1) v p(V2) v p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v cs3(C) v !p(V1) v p(V2) v !p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v !cs3(C) v !p(V1) v !p(V2) v p(V3)
	        
	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v cs3(C) v !p(V1) v !p(V2) v !p(V3)
	            
	        !cv1(C,V1) v !cv1(C,V2), V1!=V2
	        !cv2(C,V1) v !cv2(C,V2), V1!=V2
	        !cv3(C,V1) v !cv3(C,V2), V1!=V2
	"""
    }
    it should behave like smallModel(model, correctLogWMC)
  }
}

@RunWith(classOf[JUnitRunner])
class TestBug8Denominator extends ModelBehaviours {
  
  describe("Bug8Model - Denominator") {

    val correctLogWMC = -20.93671457650226 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
            domain Variable 4 {}
            domain Clause 6 {}
		     
    		predicate p(Variable) 0.5 0.5
	                                
	        predicate cv1(Clause,Variable) 0.5 0.5
	        predicate cv2(Clause,Variable) 0.5 0.5
	        predicate cv3(Clause,Variable) 0.5 0.5
	                                
	        predicate cs1(Clause) 0.5 0.5
	        predicate cs2(Clause) 0.5 0.5
	        predicate cs3(Clause) 0.5 0.5
	        
	        !cv1(C,V1) v !cv1(C,V2), V1!=V2
	        !cv2(C,V1) v !cv2(C,V2), V1!=V2
	        !cv3(C,V1) v !cv3(C,V2), V1!=V2
	"""
    }
    it should behave like smallModel(model, correctLogWMC)
  }

}
