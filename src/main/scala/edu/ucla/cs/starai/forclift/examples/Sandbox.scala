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

package edu.ucla.cs.starai.forclift.examples

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models.MLNModel
import edu.ucla.cs.starai.forclift.languages._

object Sandbox extends FactorGraphParser {

  def main(args: Array[String]): Unit = {

    /*
		 {
				val theory = 
"""
predicate p 1 1
predicate q 1 1
predicate r 1 1

p v r
q v !r
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-propositional-dnnf.nnf")
			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-propositional-dnnf-smooth.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10
predicate p(D) 1 1
predicate q(D) 1 1
predicate r 1 1

p(X) v r
q(X) v !r
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-fo-dnnf-leaf.nnf")
			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-fo-dnnf-leaf.smooth.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10
predicate p(D) 1 1
predicate q(D) 1 1

p(X) v q(Y)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-fo-dnnf-ie.nnf")
		}
		
		{
				val theory = 
"""
domain D 10
predicate r(D,D) 1 1
predicate s(D) 1 1
predicate t(D) 1 1

r(X,Z) v t(Z)
r(X,Z) v !s(X)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-fo-dnnf-set.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10
predicate p(D) 1 1
predicate q(D) 1 1
predicate r(D) 1 1

p(X) v q(X)
!p(X) v r(X)
p(a)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-unitpropagation1.nnf")
		}
		{
				val theory = 
"""
domain D 10
predicate p(D,D) 1 1
predicate q(D,D) 1 1
predicate r(D,D) 1 1

p(X,Y) v q(X,Y)
!p(X,Y) v r(X,Y)
p(X,X)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-unitpropagation2.nnf")
		}
		{
				val theory = 
"""
domain D 10
predicate p(D,D) 1 1
predicate q(D) 1 1
predicate r(D) 1 1

p(X,Y) v q(X)
p(X,Y) v r(Y)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-unitpropagation3.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10 {a}
predicate p(D,D) 1 1
predicate q(D) 1 1
predicate r(D) 1 1

p(X,Y) v q(X), X!=a
p(a,Y) v !r(Y)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-independence.nnf")
		}
		*/
    /*
				{
				val theory = 
"""
domain D 10
predicate p(D) 1 1
predicate q(D) 1 1
predicate r(D,D) 1 1

p(X) v r(X,X) v q(Y) v r(Y,Z)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-ie.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10
predicate p(D) 1 1
predicate q(D) 1 1
predicate r 1 1

p(X) v r
q(X) v !r
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-shannon.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain People 10
predicate dislikes(People,People) 1 1
predicate fun(People) 1 1
predicate friends(People,People) 1 1

dislikes(X,Y) v friends(X,Y)
fun(X) v !friends(X,Y)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-ipg.nnf")
//			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact.nnf")
			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact-smoothed.nnf")
		}
		*/
    /*
		{
				val theory = 
"""
domain D 10
predicate fun(D) 1 1
predicate friends(D,D) 1 1

fun(X) v !friends(X,Y)
fun(X) v !friends(Y,X)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-atomcounting-big.nnf")
			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-atomcounting-big.nnf")
		}
		*/
    /*{
				val theory = 
"""
domain D 10
predicate p(D)
predicate q(D)

p(X) and q(Y) 1.2 1
!p(X) and q(Y) 1.2 1
p(X) and !q(Y) 1.2 1
!p(X) and !q(Y) 1.2 1
"""
					
			val theoryParser = new FactorGraphParser
			val theoryWmc = theoryParser.parseFactorGraph(theory).toWeightedCNF
			
			println(theoryWmc.toString)
			
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"example-ipg.nnf")
//			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact.nnf")
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"udi-compact")
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"udi-extended")
		}*/

    /* 
	     {  
	         
				def theory(n: Int) = { 
"domain D "+n+"\n"+
"domain E "+n+"\n"+
"domain F "+n+"\n"+ """

predicate p(D) 1 1
predicate q(E) 1 1
predicate r(F) 1 1

predicate f1a(D,E) 2 1
predicate f1b(D,E) 3 1
predicate f1c(D,E) 4 1
predicate f1d(D,E) 5 1

predicate f2a(E,F) 6 1
predicate f2b(E,F) 7 1
predicate f2c(E,F) 8 1
predicate f2d(E,F) 9 1

predicate f3a(D,F) 10 1
predicate f3b(D,F) 11 1
predicate f3c(D,F) 12 1
predicate f3d(D,F) 13 1

!p(X) v !q(Y) v f1a(X,Y)
p(X) v !f1a(X,Y)
q(Y) v !f1a(X,Y)

!p(X) v q(Y) v f1b(X,Y)
p(X) v !f1b(X,Y)
!q(Y) v !f1b(X,Y)

p(X) v !q(Y) v f1c(X,Y)
!p(X) v !f1c(X,Y)
q(Y) v !f1c(X,Y)

p(X) v q(Y) v f1d(X,Y)
!p(X) v !f1d(X,Y)
!q(Y) v !f1d(X,Y)


!q(X) v !r(Y) v f2a(X,Y)
q(X) v !f2a(X,Y)
r(Y) v !f2a(X,Y)

!q(X) v r(Y) v f2b(X,Y)
q(X) v !f2b(X,Y)
!r(Y) v !f2b(X,Y)

q(X) v !r(Y) v f2c(X,Y)
!q(X) v !f2c(X,Y)
r(Y) v !f2c(X,Y)

q(X) v r(Y) v f2d(X,Y)
!q(X) v !f2d(X,Y)
!r(Y) v !f2d(X,Y)


!p(X) v !r(Y) v f3a(X,Y)
p(X) v !f3a(X,Y)
r(Y) v !f3a(X,Y)

!p(X) v r(Y) v f3b(X,Y)
p(X) v !f3b(X,Y)
!r(Y) v !f3b(X,Y)

p(X) v !r(Y) v f3c(X,Y)
!p(X) v !f3c(X,Y)
r(Y) v !f3c(X,Y)

p(X) v r(Y) v f3d(X,Y)
!p(X) v !f3d(X,Y)
!r(Y) v !f3d(X,Y)
"""
				}
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory(1))
			
			println(theoryWmc.toString)
			
//			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"poole-compact.nnf")
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"poole-extended.nnf")
			
			for(n <- 0 to 7){
			    println("Domain size "+n)
			    val theoryParser = new CNFParser
			    val theoryWmc = theoryParser.parseWMC(theory(n))
			    println(" - propositional: exp("+theoryWmc.logPropWmc+")")
			    println(" - lifted: exp("+theoryWmc.logWmc+")")
			}
			
			for(n <- 50.to(1000,50)){
			    println("Domain size "+n)
			    val theoryParser = new CNFParser
			    val theoryWmc = theoryParser.parseWMC(theory(n))
//			    println(" - propositional: exp("+theoryWmc.logPropWmc+")")
			    println(" - lifted: exp("+theoryWmc.logWmc+")")
			}
		}
	*/
    /*
	     {
	        val theory = 
"""
domain D 5 

predicate p(D) 1 1
predicate t1(D,D) 1 1
predicate t2(D,D) 1 1
predicate t3(D,D) 1 1

p(X) v p(Y) v !t1(X,Y)
p(X) v !p(Y) v !t2(X,Y)
!p(X) v !p(Y) v !t3(X,Y)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(theory)
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"aaai12-compact.nnf")
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"aaai12-extended.nnf")
		}*/

//    {
//      val theory =
//        """
//domain D 5 
//
//predicate p(D) 1 1
//predicate c1(D,D,D) 1 1
//predicate c2(D,D,D) 1 1
//predicate c3(D,D,D) 1 1
//predicate c4(D,D,D) 1 1
//
//p(X) v p(Y) v p(Z) v !c1(X,Y,Z)
//p(X) v p(Y) v !p(Z) v !c2(X,Y,Z)
//p(X) v !p(Y) v !p(Z) v !c3(X,Y,Z)
//!p(X) v !p(Y) v !p(Z) v !c4(X,Y,Z)
//"""
//
//      val theoryParser = new WeightedCNFParser
//      val theoryWmc = theoryParser.parseWMC(io.Source.fromString(theory))
//
//      println(theoryWmc.toString)
//
//      theoryWmc.showNnfPdf(true, Integer.MAX_VALUE, "thesis-compact.nnf")
//      //			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"thesis-extended.nnf")
//    }
    /*{
	        val theory = 
"""
domain D 5 
domain T 5 

predicate p(D) 1 1
predicate t1(T) 1 1
predicate t2(T) 1 1
predicate t3(T) 1 1
predicate t4(T) 1 1
predicate m1(D,T) 1 1
predicate m2(D,T) 1 1
predicate m3(D,T) 1 1

p(X) v p(Y) v p(Z) v !t1(T) v !m1(X,T) v !m2(Y,T) v !m3(Z,T)
p(X) v p(Y) v !p(Z) v !t2(T) v !m1(X,T) v !m2(Y,T) v !m3(Z,T)
p(X) v !p(Y) v !p(Z) v !t3(T) v !m1(X,T) v !m2(Y,T) v !m3(Z,T)
!p(X) v !p(Y) v !p(Z) v !t4(T) v !m1(X,T) v !m2(Y,T) v !m3(Z,T)
"""
					
			val theoryParser = new CNFParser
			val theoryWmc = theoryParser.parseWMC(io.Source.fromString(theory))
			
			println(theoryWmc.toString)
			
			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"aaai12-2-compact.nnf")
			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"aaai12-2-extended.nnf")
		}
	}*/
    /*
        {
            val model = new MLNModel {

                def theoryString = (
                    """person = {P1,P2}
position = {P1,P2}
person = {P1,P2}
year = {P1,P2}
course = {P1,P2}
title = {P1,P2}
level = {P1,P2}
phase = {P1,P2}
num = {P1,P2}
Position(person,position)
Advisedby(person,person)
Taughtby(course,person,year)
Publication(title,person)
Professor(person)
Courselevel(course,level)
Phase(person,phase)
Ta(course,person,year)
Yearsinprogram(person,num)
Tempadvisedby(person,person)
Ta(a1,a2,a3).
-2.0175    Publication(a1,a2)
-3.12119   Taughtby(a1,a2,a3)
-0.0548202 Advisedby(a1,a1)
-1.47249   Position(a1,a2)
-0.0435963 Tempadvisedby(a1,a1)
-2.61089   Phase(a1,a2)
-1.5893    Courselevel(a1,a2)
0.0765773  Professor(a1)
-2.48722   Tempadvisedby(a1,a2)
-3.0837    Advisedby(a1,a2)
-2.57142   Ta(a1,a2,a3)
-0.583456  Yearsinprogram(a1,a2)
1.34552    !Tempadvisedby(a1,a2) v !Ta(a3,a1,a4)
1.41777    Courselevel(a1,a2) v Courselevel(a1,a3)
1.30419    Phase(a1,a2) v !Yearsinprogram(a1,a3)
1.69828    Professor(a1) v !Position(a1,a2)
1.82228    !Ta(a1,a2,a3) v !Publication(a4,a2)
""")

            }
            println("Order: "+model.theory.smoothNnf.evalOrder)
//            model.theory.showNnfPdf(true, 20, "bug.nnf")
            model.theory.logWmc
        }
//        {
//            val model = new MLNModel {
//
//                def theoryString = (
//                    """
//person = {P1,P2}
//Series
//Topic
//Attends(person)
//Attends(x) => Series.
//Topic => Attends(x).
//""")
//
//            }
//            model.theory.showNnfPdf(true, Integer.MAX_VALUE, "sml2.nnf")
//        }

    */
  }
}
