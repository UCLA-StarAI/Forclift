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

package edu.ucla.cs.starai.forclift.languages

import java.io.{FileNotFoundException, IOException}

import collection._
import scala.util.parsing.combinator._;
import scala.io._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.languages._
import edu.ucla.cs.starai.forclift.inference._

class FactorGraphParser extends JavaTokenParsers with ModelParser{

  def parseModel(theoryString: String) = parseFactorGraph(theoryString)
  
  /**
   * If you want the same namespace for this atom and a theory, then use the same Parser object.
   */
  def parseAtom(str: String): Atom = parseAll(atom, str).get

  def applyParsers[T](parsers: List[Parser[T]], line: String): T = parsers match {
    case parser :: rest => {
      val result = parseAll(parser, line)
      //        println(parser.toString +" on "+line +" gives ")
      //        println(result)
      if (result.successful) result.get
      else applyParsers(rest, line)
    }
    case Nil => throw new IllegalArgumentException("Failed to parse: " + line)
  }

  def domainSizes: DomainSizes = domainMap.values.map {
    case (k, v) =>
      (k.asInstanceOf[Domain], DomainSize(v, k.asInstanceOf[Domain]))
  }.toMap

  def predicateWeights: PredicateWeights = weightMap.toMap

  lazy val kwType: Parser[String] = "predicate".r
  lazy val kwDomain: Parser[String] = "domain".r

  val domainMap = new mutable.HashMap[String, (RootDomain, Int)]

  lazy val domainLine: Parser[Domain] = (
    kwDomain ~ """[A-Z][a-zA-Z0-9]*""".r ~ wholeNumber ~ opt("""\{""".r ~ repsep(constant, ",") ~ """\}""".r) ^^ {
      case _ ~ name ~ size ~ constantsOption => {
        val constants = if (constantsOption.isEmpty) List[Constant]() else {
          val Some(_ ~ c ~ _) = constantsOption
          c
        }
        domainMap.getOrElseUpdate(name, (new RootDomain(name, constants.toList), size.toInt))._1
      }
    }
    | failure("Illegal domain"))

  val predicateMap = new mutable.HashMap[(String, Int), Predicate]
  val weightMap = new mutable.HashMap[Predicate, Weights]

  lazy val typeLine: Parser[Predicate] = (
    kwType ~ predicate ~ opt("(" ~ rep1sep("""[A-Z][a-zA-Z0-9]*""".r, ",") ~ ")") ~ weights ^^ {
      case _ ~ predicateName ~ argList ~ weights =>
        val domainNames = argList match {
          case Some(_ ~ domainNames ~ _) => domainNames
          case None => List()
        }
        val domains = domainNames.map { domainName =>
          domainMap.getOrElse(domainName, throw new IllegalStateException("Unknown domain " + domainName))._1
        }
        val arity = domains.size
        assume(!predicateMap.contains((predicateName, arity)))
        val predicate = new Predicate(Symbol(predicateName), arity, domains)
        predicateMap((predicateName, arity)) = predicate
        weightMap(predicate) = weights
        predicate
    }
    | failure("Illegal type"))

  lazy val weights: Parser[Weights] = opt(floatingPointNumber ~ floatingPointNumber) ^^ {
    case None => Weights(1, 1)
    case Some(p ~ n) => Weights(p.toDouble, n.toDouble)
  }


  lazy val literal: Parser[(Boolean, Atom)] = (
    atom ^^ {
      (true, _)
    }
    | """[!¬]""".r ~ atom ^^ {
      case _ ~ a => (false, a)
    }
    | failure("Illegal literal"))

  lazy val atom: Parser[Atom] = (
    predicate ~ opt("(" ~> rep1sep(term, ",") <~ ")")
    ^^ {
      case name ~ None => {
        val predicate = predicateMap.getOrElseUpdate((name, 0), new Predicate(Symbol(name), 0))
        predicate()
      }
      case name ~ Some(arglist) => {
        val predicate = predicateMap.getOrElseUpdate((name, arglist.size), new Predicate(Symbol(name), arglist.size))
        assume((arglist zip predicate.domains).forall {
          case (a, d) =>
            !a.isInstanceOf[Constant] || d.contains(a.asInstanceOf[Constant])
        })
        predicate(arglist: _*)
      }
    }
    | failure("Illegal atom"))

  lazy val term: Parser[Term] = (
    constant | variable | failure("Illegal term"))

  lazy val constant: Parser[Constant] = """[a-z0-9]+""".r ^^ { Constant(_) } | failure("Illegal constant")

  val varMap = new mutable.HashMap[String, Var]

  lazy val variable: Parser[Var] = (
    """[A-Z][a-zA-Z0-9]*""".r ^^ { name =>
      varMap.getOrElseUpdate(name, new Var)
    }
    | failure("Illegal variable"))

  val predicate = """([a-z0-9_{}])+""".r
  
  def parseFactorGraph(str: String): FactorGraph = {
    val sourceFile = Source.fromString(str)
    try {
      parseFactorGraph(sourceFile)
    } catch {
      case e: IOException => throw new Exception(s"Problem with file $str: " + e.getMessage)
      case e: FileNotFoundException => throw new Exception(s"Can't find file $str")
    }
    finally {
      sourceFile.close()
    }
  }

  def parseFactorGraph(source: Source): FactorGraph = {
    val lineTypes = List(domainLine, typeLine, ifthenelse, ifthen, conjunctiveFactor, disjunctiveFactor, query)
    val lines = source.getLines.map { _.trim }.filter { _.nonEmpty }.map { line =>
      applyParsers(lineTypes, line)
    }.toList
    val parFactors = lines.collect { case bf: BrazFactor => bf }
    val factorGraph = new FactorGraph(parFactors.toList, domainSizes, predicateWeights)
    factorGraph
  }

  lazy val factorGraph: Parser[FactorGraph] = (
    rep(brazLine) ^^ { lines =>
      FactorGraph(lines.collect { case bf: BrazFactor => bf }, DomainSizes.empty, PredicateWeights.empty)
    }
    | failure("Illegal FactorGraph"))

  lazy val brazLine = domainLine | typeLine | parfactor | failure("Illegal Braz line")

  //order specific to general ?
  lazy val parfactor: Parser[BrazFactor] = (ifthenelse | ifthen | conjunctiveFactor | disjunctiveFactor | query | failure("Illegal BrazFactor"))

  lazy val query: Parser[Query] = (
    brazLiteral ^^ { Query(_) }
    | failure("Illegal Query"))

  lazy val conjunctiveFactor: Parser[ConjunctiveFactor] = (
    rep1sep(brazLiteral, """and""".r) ~ floatingPointNumber ~ floatingPointNumber ^^ {
      case literals ~ weight ~ negWeight => {
        ConjunctiveFactor(literals, weight.toDouble, negWeight.toDouble)
      }
    }
    | failure("Illegal conjunctive factor"))

  lazy val disjunctiveFactor: Parser[DisjunctiveFactor] = (
    rep1sep(brazLiteral, ("""or""".r | """v""".r)) ~ opt(floatingPointNumber ~ floatingPointNumber) ^^ {
      case literals ~ Some(weight ~ negWeight) => {
        DisjunctiveFactor(literals, weight.toDouble, negWeight.toDouble)
      }
      case literals ~ None => {
        DisjunctiveFactor(literals, 1, 0)
      }
    }
    | failure("Illegal disjunctive factor"))

  lazy val ifthen: Parser[IfThen] = (
    "if\\b".r ~ brazLiteral ~ "then\\b".r ~ brazLiteral ~ floatingPointNumber ^^ {
      case _ ~ il ~ _ ~ tl ~ w => IfThen(il, tl, w.toDouble)
    }
    | failure("Illegal IfThen"))

  lazy val ifthenelse: Parser[IfThenElse] = (
    "if\\b".r ~ brazLiteral ~ "then\\b".r ~ brazLiteral ~ floatingPointNumber ~ "else\\b".r ~ floatingPointNumber ^^ {
      case _ ~ il ~ _ ~ tl ~ p ~ _ ~ q => IfThenElse(il, tl, p.toDouble, q.toDouble)
    }
    | failure("Illegal IfThenElse"))

  lazy val brazLiteral: Parser[(Boolean, Atom)] = (
    not(reserved) ~> literal
    //		literal
    | failure("Illegal BrazLiteral"))

  lazy val reserved: Parser[String] = (kwIf | kwThen | kwElse | kwAnd | kwOr | kwType | kwDomain)

  val kwIf: Parser[String] = "if".r
  val kwThen: Parser[String] = "then".r
  val kwElse: Parser[String] = "else".r
  val kwAnd: Parser[String] = "and".r
  val kwOr: Parser[String] = "or".r

}
