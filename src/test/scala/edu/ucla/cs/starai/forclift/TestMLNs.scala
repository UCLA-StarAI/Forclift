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
import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift.languages.mln._
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TestMLNs extends FunSpec with Matchers {

  describe("Smokers friends") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
friends(person,person)
smokes(person)
1 friends(x,y) ^ smokes(x) => smokes(y)
"""
    var mln = MLN()
    var mln2 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = (new MLNParser).parseMLN(mlnString + "\nsmokes(Guy).")
      //println(mln.toStringExt)
    }

    it("MLN grounds") {
      val ground = mln.ground(mln.domainSizes)
      //println("Grounded theory"+ground.toMLNFileString)
    }

    it("Has correct wmc for weightedCNF") {
      val model = mln.toWeightedCNF()
      model.logSmoothWmc.logToDouble should be(44.15889192542563 +- 0.01)
    }

    it("Has correct wmc for weightedCNF with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      //print(model.logWmc)
      model.logSmoothWmc.logToDouble should be(44.15889192542563 +- 0.01)

      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.5 +- 0.01)
    }

    it("Has correct Pr (implication)") {
      val model = mln.toWeightedCNFWithoutSplitting(transformation = 1)
      val model2 = mln2.toWeightedCNFWithoutSplitting(transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.5 +- 0.01)
    }
  }

  describe("Smokers friends small") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima}
friends(person,person)
smokes(person)
1 friends(x,y) ^ smokes(x) => smokes(y)
"""
    var mln = MLN()
    var mln2 = MLN()
    var pr = 0.0

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = (new MLNParser).parseMLN(mlnString + "\nsmokes(Guy).")
      //println(mln.toStringExt)
    }

    it("MLN grounds") {
      val ground = mln.ground(mln.domainSizes)
      //println("Grounded theory"+ground.toMLNFileString)
    }

    it("Has correct wmc for weightedCNF") {
      // 1 f(1,1) ^ s(1) => s(1)
      // 1 f(1,2) ^ s(1) => s(2)
      // 1 f(2,1) ^ s(2) => s(1)
      // 1 f(2,2) ^ s(2) => s(2)

      // = 7.98

      val model = mln.toWeightedCNF()
      model.logSmoothWmc.logToDouble should be(7.98 +- 0.01)
    }

    it("Has correct wmc for weightedCNF with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      model.logSmoothWmc.logToDouble should be(7.98 +- 0.01)
    }

    it("Pr(p) with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.5 +- 0.01)
    }

    it("Pr(p) with implication") {
      val model = mln.toWeightedCNFWithoutSplitting(transformation = 1)
      val model2 = mln2.toWeightedCNFWithoutSplitting(transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.5 +- 0.01)
    }
  }

  describe("Friends (equality operator)") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima}
friends(person,person)
1 friends(x,y) ^ friends(x,z) => y=z
"""

    // 1 !f(x,y) v !f(x,z) v eq(y,z)

    // ignore these because they are always true, both in query wmc as in
    // partition function wmc.
    // 1 !f(1,1) v !f(1,1) v eq(1,1)=T # ignored
    // 1 !f(1,2) v !f(1,2) v eq(2,2)=T # ignored
    // 1 !f(2,1) v !f(2,1) v eq(1,1)=T # ignored
    // 1 !f(2,2) v !f(2,2) v eq(2,2)=T # ignored

    // 1 !f(1,1) v !f(1,2) v eq(1,2)=F
    // 1 !f(1,2) v !f(1,1) v eq(2,1)=F
    // 1 !f(2,1) v !f(2,2) v eq(1,2)=F
    // 1 !f(2,2) v !f(2,1) v eq(2,1)=F

    // Translation:
    // 1 !f(x,y) v !f(x,z), y!=z
    //
    // 1 !f(1,1) v !f(1,2)
    // 1 !f(1,2) v !f(1,1)
    // 1 !f(2,1) v !f(2,2)
    // 1 !f(2,2) v !f(2,1)

    var mln = MLN()
    var mln2 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = (new MLNParser).parseMLN(mlnString + "\nfriends(Guy,Nima).")
    }

    it("Has correct wmc") {
      // !f(1,1) : 2*e^(1+1)
      //  f(1,1) ^ !f(1,2) : 1*e^(1+1)
      //
      // !f(2,2) : 2*e^(1+1)
      //  f(2,2) ^ !f(2,1) : 1*e^(1+1)
      //
      // log(9*e**4 + 6*e**2 + 1*e**0) = 6.2854722
      // with eq=T clauses:
      // log(9*e**8 + 6*e**6 + 1*e**4) = 10.28547
      val model = mln.toWeightedCNF()
      //println(model)
      model.logSmoothWmc.logToDouble should be(10.28547 +- 0.01)
    }

    it("Has correct wmc and Pr (equivalence)") {
      val model = mln.toWeightedCNFWithoutSplitting()
      model.logSmoothWmc.logToDouble should be(10.28547 +- 0.01)

      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.362109 +- 0.01)
    }

    it("Has correct Pr (implication)") {
      val model = mln.toWeightedCNFWithoutSplitting(transformation = 1)
      val model2 = mln2.toWeightedCNFWithoutSplitting(transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.362109 +- 0.01)
    }
  }

  /** **/
  describe("Single atom") {

    val parser = new MLNParser
    val mlnString =
      """
p
1 p
"""
    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("Has correct wmc for weightedCNF") {
      // p=T: e**1
      // p=F: e**0
      // logZ = log(e**1 + e**0) = log(3.7182) =  1.31326
      val model = mln.toWeightedCNF()
      model.logSmoothWmc.logToDouble should be(1.31326 +- 0.01)
    }

    it("Has correct wmc for weightedCNF with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      model.logSmoothWmc.logToDouble should be(1.31326 +- 0.01)
    }
  }

  /** **/
  describe("Propositional") {

    val parser = new MLNParser
    val mlnString =
      """
p
q
r
1 p v q
2 p v r
"""
    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("Has correct wmc for weightedCNF") {
      // p=T: 4*e^(1+2)
      // p=F ^ r=T ^ q=F: 1*e^(0+2)
      // p=F ^ r=F ^ q=T: 1*e^(1+0)
      // p=F ^ r=T ^ q=T: 1*e^(1+2)
      // logZ = log(5*e^3+e^2+e^1) = log(110.5350225) = 4.7053324
      // lPr(p) = log(4*e**3) - 4.7053324 = -0.31903803
      // Pr(p) = 0.72684
      val model = mln.toWeightedCNF()
      model.logSmoothWmc.logToDouble should be(4.7053324 +- 0.01)
    }

    it("Has correct wmc for weightedCNF with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      model.logSmoothWmc.logToDouble should be(4.7053324 +- 0.01)
    }

    it("Pr(p) with implication") {
      val model = mln.toWeightedCNFWithoutSplitting(transformation = 1)
      val mln2 = (new MLNParser).parseMLN(mlnString + "\np.")
      val model2 = mln2.toWeightedCNFWithoutSplitting(transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.72684 +- 0.01)
    }
  }

  /** **/
  describe("Existential (simple)") {

    val parser = new MLNParser
    val parser2 = new MLNParser
    val mlnString =
      """
people = {Guy, Wannes, Jesse, Luc, Nima}
p(people)
q
1 p(x)
2 q <= (exist x p(x))
"""
    var mln = MLN()
    var mln2 = MLN()
    var pr = 0.0

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = parser2.parseMLN(mlnString + "\nq.")
    }

    it("Pr(p) with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.879854 +- 0.01)
    }

    it("Pr(p) with <=> and skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = true)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.879854 +- 0.01)
    }
  }

  /** **/
  describe("Existential (single formula)") {

    val parser = new MLNParser
    val parser2 = new MLNParser
    val mlnString =
      """
elmts= {E1,E2}
p(elmts)

1 EXIST x p(x)
"""

    var mln = MLN()
    var mln2 = MLN()
    var pr = 0.0

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = parser2.parseMLN(mlnString + "\np(E1).")
    }

    it("Pr(p) with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.59384 +- 0.01)
    }

    it("Pr(p) with <=> and skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = true)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.59384 +- 0.01)
    }

    it("Pr(p) with <=> and skolemize after PNF") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true, earlySk = false)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = true, earlySk = false)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.59384 +- 0.01)
    }
  }

  /** **/
  describe("Existential (CP-logic)") {

    val parser = new MLNParser
    val parser2 = new MLNParser
    val mlnString =
      """
// Original CP-theory
// p(X):0.3.
// q:0.2 <- p(X).


// Domains
elmts= {E1,E2,E3}

// Predicates
p(elmts)
ii_c1(elmts)
p_c1(elmts)
q

// Formulas
(p(x) ^ ii_c1(x)) <=> p_c1(x).
!p(x) => !ii_c1(x).
q <=> (EXIST x ii_c1(x)).
-1.2039728043259361 p(x)
-1.6094379124341003 p_c1(x)
"""
    var mln = MLN()
    var mln2 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      mln2 = parser2.parseMLN(mlnString + "\nq.")
    }

    it("Pr(p) with <=>") {
      val model = mln.toWeightedCNFWithoutSplitting()
      val model2 = mln2.toWeightedCNFWithoutSplitting()
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.126599 +- 0.01)
    }

    it("Pr(p) with <=> and skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = true)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.126599 +- 0.01)
    }

    it("Pr(p) with => and skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true, transformation = 1)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = true, transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.126599 +- 0.01)
    }

    it("Pr(p) with => and no skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = false, transformation = 1)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = false, transformation = 1)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.126599 +- 0.01)
    }

    it("Pr(p) with <= and no skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = false, transformation = 2)
      val model2 = mln2.toWeightedCNFWithoutSplitting(skolemize = false, transformation = 2)
      val pr = model2.logSmoothWmc / model.logSmoothWmc
      pr.toDouble should be(0.126599 +- 0.01)
    }
  }

  /** **/
  describe("Example MLN from Van den Broeck, Meert, Darwiche (2014)") {

    val parser = new MLNParser
    val parser2 = new MLNParser
    val mlnString =
      """
// Domains
people= {E1,E2,E3}

// Predicates
worksFor(people, people)
boss(people)

// Formulas
1.3 exist y worksFor(x,y) v boss(x)

"""
    var mln = MLN()
    var mln2 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
      //mln2 = parser2.parseMLN(mlnString+"\nq.")
    }

    it("Pr(p) with <=> and no Skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = false)
      model.logSmoothWmc.logToDouble should be(12.07 +- 0.01)
    }

    it("Pr(p) with <=> and Skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(12.07 +- 0.01)
    }

    it("Pr(p) with <=> and Skolemize after PNF") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true, earlySk = false)
      model.logSmoothWmc.logToDouble should be(12.07 +- 0.01)
    }
  }

  /** **/
  describe("Nested existential quantifiers") {

    val parser = new MLNParser
    val mlnString =
      """
// Domains
people= {E1,E2}

// Predicates
worksFor(people, people)
boss(people)

// Formulas
1.3 exist y exist x worksFor(x,y) v boss(x)

"""
    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("Pr(p) with <=> and no Skolemize") {
      // 2**6 worlds of which 1 is false => 
      // math.log(63*math.exp(1.3)+1) = 5.4474512
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = false)
      //println(model)
      //println(model.logSmoothWmc.logToDouble)
      model.logSmoothWmc.logToDouble should be(5.4474512 +- 0.01)
    }

    it("Pr(p) with <=> and Skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      //println(model)
      //println(model.logSmoothWmc.logToDouble)
      model.logSmoothWmc.logToDouble should be(5.4474512 +- 0.01)
    }
  }

  /** **/
  describe("Nested quantifiers (exist foral formula") {

    val parser = new MLNParser
    val mlnString =
      """
// Domains
people= {E1,E2}

// Predicates
worksFor(people, people)
boss(people)

// Formulas
1.3 exist y forall x worksFor(x,y) v boss(x)

"""
    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("Pr(p) with <=> and no Skolemize") {
      // 2**6 worlds of which 9+4+4 are false => 
      // math.log(47*math.exp(1.3)+17) = 5.2441617
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = false)
      //println(model)
      //println(model.logSmoothWmc.logToDouble)
      model.logSmoothWmc.logToDouble should be(5.2441617 +- 0.01)
    }

    it("Pr(p) with <=> and Skolemize") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(5.2441617 +- 0.01)
    }

    //it("Pr(p) with <=> and Skolemize after PNF") {
    //val model = mln.toWeightedCNFWithoutSplitting(skolemize=true, earlySk=false)
    //println(model)
    //model.logSmoothWmc.logToDouble should be (5.2441617 +- 0.01)
    //}
  }
  
  //--------------------------------------------------------------------------

  describe("Hierarchical domains (example Leon Bergen)") {

    val structureStr =
      """
person = {male, female}
male = 5 {}
female = 4 {}

friends(person,person)

friends(x,y), x in male, y in male.
friends(x,y), x in male, y in female.
"""
    // Only thing variable is f*m (=20) and f*f (=16)
    // 2**(20+16) models = 68719476736
    // log(68719476736) = 24.95329850015803 
      
    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }
    
    it("WMC") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(24.95329850015803 +- 0.01)
    }
  }
  
  describe("Hierarchical domains, extensive (example Leon Bergen)") {

    val structureStr =
      """
person = {male, female}
male = 5 {}
female = 4 {}

friends(person,person)
male(person)
female(person)
      
male(x), x in male.
!male(x), x in female.
female(x), x in female.
!female(x), x in male.

male(x) ^ male(y) => friends(x,y).
female(x) ^ female(y) => friends(x,y).
"""
    // Only thing variable is f*m (=20) and m*f (=20)
    // 2**(20+20) models = 1099511627776
    // log(1099511627776) = 27.725887222397812 
      
    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }
    
    it("WMC") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(27.725887222397812 +- 0.01)
    }
  }
  
  describe("Hierarchical domains (three-way split)") {

    val structureStr =
      """
person = {male, female, unknown}
male = 5 {}
female = 4 {}
unknown = 1 {}

friends(person,person)

friends(x,y), x in male, y in male.
friends(x,y), x in male, y in female.
"""
    // Only thing variable is u*m, m*u, u*f, f*u, u*u f*m (=20) and f*f (=16)
    // 2**(4+4+5+5+1+20+16) models = 36028797018963968
      
    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }
    
    it("WMC") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(38.123094930796995 +- 0.01)
    }
  }
  
  /* */
  describe("Hierarchical domains (on existential, simple)") {

    val structureStr =
      """
person = {male, female}
male = 5 {}
female = 5 {}

friends(person,person)

exist x friends(x,y), x in male.
"""
      
    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }
    
    it("WMC") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(68.9972 +- 0.01)
    }
  }
  
  /* */
  describe("Hierarchical domains (on existential)") {

    val structureStr =
      """
person = {male, female}
male = 5 {}
female = 5 {}

friends(person,person)

exist y friends(x,y), x in male.
"""
    // approx: 2**(10*10) - 5*2**(10*10-10) = log(69.3098)
      
    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }
    
    it("WMC") {
      val model = mln.toWeightedCNFWithoutSplitting(skolemize = true)
      model.logSmoothWmc.logToDouble should be(69.3098 +- 0.01)
    }
  }
  
  /* */
  describe("Learning parses disjuncts (CNF)") {

    val structureStr =
      """
//predicate declarations
workedUnder(person,person)
movie(film,person)
director(person)
genre(person,type)
actor(person)

0 workedUnder(person0,person1) v !movie(film0,person1) v director(person1)
0 workedUnder(person0,person1) v genre(person0,type0) v actor(person0)
"""      
    val parser = new MLNParser
    parser.setLearnModus(true)

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
    }
    
    it("MLN are disjuncts") {
      val str = mln.toStringLearnFormulas
    }
  }

}
