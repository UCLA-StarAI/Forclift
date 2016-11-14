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

import collection._
import scala.util.parsing.combinator._;
import scala.io._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.languages._
import edu.ucla.cs.starai.forclift.inference._

@deprecated("Weighted CNF file format is being removed","3.0")
class WeightedCNFParser extends JavaTokenParsers {

  /**
   * If you want the same namespace for this atom and a theory, then use the same CNFParser object.
   */
  def parseAtom(str: String): Atom = parseAll(atom, str).get

  def parseClause(str: String): Clause = parseAll(clause, str).get

  def parseStringWMC(str: String, compilerBuilder: Compiler.Builder = Compiler.Builder.default): WeightedCNF = parseWeightedCNF(Source.fromString(str), compilerBuilder)

  @deprecated("Weighted CNF file format is being removed","3.0")
  def parseWeightedCNF(source: Source, compilerBuilder: Compiler.Builder = Compiler.Builder.default): WeightedCNF = {
    val cnf = parseCNF(source)
    WeightedCNF(cnf, domainSizes, predicateWeights, compilerBuilder = compilerBuilder)
  }

  def parseCNF(str: String): CNF = parseCNF(Source.fromString(str))

  def parseCNF(source: Source): CNF = {
    val lineTypes = List(domainLine, typeLine, clause)
    val lines = source.getLines.map { _.trim }.filter { _.nonEmpty }.map { line =>
      applyParsers(lineTypes, line)
    }
    val clauses = lines.collect { case clause: Clause => clause }
    new CNF(clauses.toList)
  }

  def applyParsers[T](parsers: List[Parser[T]], line: String): T = parsers match {
    case parser :: rest => {
      val result = parseAll(parser, line)
      //				println(parser.toString +" on "+line +" gives ")
      //				println(result)
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

  lazy val cnf: Parser[CNF] = (
    rep(cnfLine) ^^ { list => new CNF(list.collect { case clause: Clause => clause }) }
    | failure("Illegal CNF"))

  lazy val cnfLine = domainLine | typeLine | clause | failure("Illegal cnf line")

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

  lazy val clause: Parser[Clause] = (
    literals ~ opt(""",""".r ~ ineqConstr) ^^ {
      case (posLits, negLits) ~ None => Clause(posLits, negLits).standardizeApart
      case (posLits, negLits) ~ Some(_ ~ constrs) => Clause(posLits, negLits, Constraints(constrs)).standardizeApart
    }
    | failure("Illegal clause"))

  lazy val literals: Parser[(List[Atom], List[Atom])] = (
    rep1sep(literal, """[,v∨\|]""".r) ^^ {
      literals =>
        {
          val (posLiterals, negLiterals) = literals.partition { _._1 }
          (posLiterals.map { _._2 }, negLiterals.map { _._2 })
        }
    }
    | failure("Illegal set of literals"))

  lazy val ineqConstr: Parser[IneqConstr] = (
    rep1sep(variable ~ ("""!=""".r | """≠""".r) ~ term, ",") ^^ {
      matches =>
        IneqConstr(matches.map { case v ~ _ ~ a => (v, a) }: _*)
    }
    | failure("Illegal inequality constraint"))

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

}
