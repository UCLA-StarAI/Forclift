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

package edu.ucla.cs.starai.forclift.examples;
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

//package liftedinference.examples
//
//import edu.ucla.cs.starai.forclift._
//
//object DomainRecursion extends App{
//  /*
//    {
//				val theory = 
//"""
//domain D 5 {n}
//predicate p(D,D) 1 1
//predicate q(D,D) 1 1
//
//p(X,Y) v q(X,Y), X!=Y, X!=n, Y!=n
//!p(X,Y) v q(Y,X), X!=Y, X!=n, Y!=n
//
//p(n,Y) v q(n,Y), Y!=n
//!p(n,Y) v q(Y,n), Y!=n
//
//p(X,n) v q(X,n), X!=n
//!p(X,n) v q(n,X), X!=n
//"""
//					
//			val theoryParser = new CNFParser
//			val theoryWmc = theoryParser.parseWMC(theory)
//			
//			println(theoryWmc.toString)
//			
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"domainrecursion1.nnf")
////			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact.nnf")
////			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact-smoothed.nnf")
//    }
//    */
//    /*
//    
//    {
//				val theory = 
//
//				    """
//domain D 5 {n}
//predicate p(D,D) 1 1
//
//!p(X,Y) v !p(Y,Z) v p(X,Z), X!=Y, Y!=Z, X!=n, Y!=n, Z!=n
//
//!p(n,Y) v !p(Y,Z) v p(n,Z), Y!=Z, Y!=n, Z!=n
//!p(X,n) v !p(n,Z) v p(X,Z), X!=n, Z!=n
//!p(X,Y) v !p(Y,n) v p(X,n), X!=Y, X!=n, Y!=n
//
//!p(n,Y) v !p(Y,n) v p(n,n), Y!=n
//
//"""
//				/*val theory = 
//"""
//domain D 5
//predicate p(D,D) 1 1
//
//!p(X,Y) v !p(Y,Z) v p(X,Z), X!=Y, Y!=Z
//"""*/
//					
//			val theoryParser = new CNFParser
//			val theoryWmc = theoryParser.parseWMC(theory)
//			
//			println(theoryWmc.toString)
//			println("Shattered:")
//			println(theoryWmc.cnf.shatter.toString)
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"domainrecursion2.nnf")
//			
//    }
//    */
////    /*
//    
//    {
//        /*
//    }
//    // SIMILARITY CLAUSE
//				val theory = 
//"""
//domain D 5
//domain P 5 {n}
//predicate p(D,P) 1 1
//predicate s(D,D) 1 1
//
//!p(X,P) v !s(X,Y) v p(Y,P), P!=n
//
//!p(X,n) v !s(X,Y) v p(Y,n)
//"""
//	*/			    
//        /*
//				    val theory = 
//"""
//domain F 5
//domain G 5
//domain P 5 {n,m}
//predicate pf(F,P) 1 1
//predicate pg(G,P) 1 1
//predicate sgf(G,F) 1 1
//predicate sff(F,F) 1 1
//predicate sgg(G,G) 1 1
//
//pf(X,Y) v ¬pg(Z,Y) v ¬sgf(Z,X), Y≠n, Y!=m
//pf(X,m) v ¬pg(Z,m) v ¬sgf(Z,X)
//
//pf(X,Y) v ¬pf(Z,Y) v ¬sff(Z,X), Y≠n, Y!=m
//pf(X,m) v ¬pf(Z,m) v ¬sff(Z,X)
//
//pg(X,Y) v ¬pg(Z,Y) v ¬sgg(Z,X), Y≠n, Y!=m
//pg(X,m) v ¬pg(Z,m) v ¬sgg(Z,X)
//"""
//*/
//        val theory = 
//"""
//domain D 5 {n}
//domain P 5 
//predicate p(D,P) 1 1
//predicate q(D,P) 1 1
//predicate s(D,D) 1 1
//
//!p(X,P) v !s(X,Y) v p(Y,P), X!=n, Y!=n
//
//!p(n,P) v !s(n,Y) v p(Y,P), Y!=n
//!p(X,P) v !s(X,n) v p(n,P), X!=n
//!p(n,P) v !s(n,n) v p(n,P)
//"""
//					
//			val theoryParser = new CNFParser
//			val theoryWmc = theoryParser.parseWMC(theory)
//			
//			println(theoryWmc.toString)
//			println("Shattered:")
//			println(theoryWmc.cnf.shatter.toString)
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"domainrecursion2.nnf")
//			
//    }
////    */
//    /*
//    {
//				val theory = 
//"""
//domain D 5 {n}
//predicate f(D,D) 1 1
//
//!f(X,Y) v !f(Y,Z) v f(X,Z), X!=n, Y!=n, Z!=n, X!=Y, Y!=Z
//
//!f(n,Y) v !f(Y,Z) v f(n,Z), Y!=n, Z!=n, Y!=Z
//!f(X,n) v !f(n,Z) v f(X,Z), X!=n, Z!=n
//!f(X,Y) v !f(Y,n) v f(X,n), X!=n, Y!=n, X!=Y
//
//!f(n,Y) v !f(Y,n) v f(n,n), Y!=n
//"""
//					
//			val theoryParser = new CNFParser
//			val theoryWmc = theoryParser.parseWMC(theory)
//			
//			println(theoryWmc.toString)
//			
////			println(theoryWmc.nnf.toString)
//			
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"domainrecursion2.nnf")
////			theoryWmc.showNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact.nnf")
////			theoryWmc.showSmoothNnfPdf(true,Integer.MAX_VALUE,"example-ipg-compact-smoothed.nnf")
//    }
//    */
//    /*
//    {
//        /*
//        val theory = 
//"""
//domain F 5
//domain G 5
//domain H 5
//
//predicate fg(F,G) 1 1
//predicate gg(G,G) 1 1
//predicate gh(G,H) 1 1
//predicate hh(H,H) 1 1
//predicate ff(F,F) 1 1
//
//fg(X,Y) v !fg(X,Z) v !gg(Z,Y)
//gh(X,Y) v !gh(X,Z) v !hh(Z,Y)
//hh(X,Y) v !hh(X,Z) v !hh(Z,Y)
//gh(X,Y) v !gg(X,Z) v !gh(Z,Y)
//gg(X,Y) v !gg(X,Z) v !gg(Z,Y)
//fg(X,Y) v !ff(X,Z) v !fg(Z,Y)
//ff(X,Y) v !ff(X,Z) v !ff(Z,Y)
//"""*/
////        /*
//					 val theory = 
//"""
//domain F 5 {f}
//domain G 5 {g}
//domain H 5 {h}
//
//predicate fg(F,G) 1 1
//predicate gg(G,G) 1 1
//predicate gh(G,H) 1 1
//predicate hh(H,H) 1 1
//predicate ff(F,F) 1 1
//
//fg(X,Y) v !fg(X,Z) v !gg(Z,Y), X!=f, Y!=g, Z!=g
//fg(f,Y) v !fg(f,Z) v !gg(Z,Y), Y!=g, Z!=g
//fg(X,g) v !fg(X,Z) v !gg(Z,g), X!=f,Z!=g
//fg(X,Y) v !fg(X,g) v !gg(g,Y), X!=f, Y!=g
//
//gh(X,Y) v !gh(X,Z) v !hh(Z,Y), X!=g, Y!=h, Z!=h
//gh(g,Y) v !gh(g,Z) v !hh(Z,Y), Y!=h, Z!=h
//gh(X,h) v !gh(X,Z) v !hh(Z,h), X!=g, Z!=h
//gh(X,Y) v !gh(X,h) v !hh(h,Y), X!=g, Y!=h
//
//hh(X,Y) v !hh(X,Z) v !hh(Z,Y), X!=h, Y!=h, Z!=h
//hh(h,Y) v !hh(h,Z) v !hh(Z,Y), Y!=h, Z!=h
//hh(X,h) v !hh(X,Z) v !hh(Z,h), X!=h, Z!=h
//hh(X,Y) v !hh(X,h) v !hh(h,Y), X!=h, Y!=h
//
//gh(X,Y) v !gg(X,Z) v !gh(Z,Y), X!=g, Y!=h, Z!=g
//gh(g,Y) v !gg(g,Z) v !gh(Z,Y), Y!=h, Z!=g
//gh(X,h) v !gg(X,Z) v !gh(Z,h), X!=g, Z!=g
//gh(X,Y) v !gg(X,g) v !gh(g,Y), X!=g, Y!=h
//
//gg(X,Y) v !gg(X,Z) v !gg(Z,Y), X!=g, Y!=g, Z!=g
//gg(g,Y) v !gg(g,Z) v !gg(Z,Y), Y!=g, Z!=g
//gg(X,g) v !gg(X,Z) v !gg(Z,g), X!=g, Z!=g
//gg(X,Y) v !gg(X,g) v !gg(g,Y), X!=g, Y!=g
//
//fg(X,Y) v !ff(X,Z) v !fg(Z,Y), X!=f, Y!=g, Z!=f
//fg(f,Y) v !ff(f,Z) v !fg(Z,Y), Y!=g, Z!=f
//fg(X,g) v !ff(X,Z) v !fg(Z,g), X!=f, Z!=f
//fg(X,Y) v !ff(X,f) v !fg(f,Y), X!=f, Y!=g
//
//ff(X,Y) v !ff(X,Z) v !ff(Z,Y), X!=f, Y!=f, Z!=f
//ff(f,Y) v !ff(f,Z) v !ff(Z,Y), Y!=f, Z!=f
//ff(X,f) v !ff(X,Z) v !ff(Z,f), X!=f, Z!=f
//ff(X,Y) v !ff(X,f) v !ff(f,Y), X!=f, Y!=f
//"""
////*/
//			val theoryParser = new CNFParser
//			val theoryWmc = theoryParser.parseWMC(theory)
//			
//			println(theoryWmc.toString)
//			
////			println(theoryWmc.nnf.toString)
//			theoryWmc.showNnfPdf(false,Integer.MAX_VALUE,"domainrecursion4.nnf")
//    }
//*/
//}
