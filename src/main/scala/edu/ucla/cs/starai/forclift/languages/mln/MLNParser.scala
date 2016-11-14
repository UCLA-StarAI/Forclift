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

package edu.ucla.cs.starai.forclift.languages.mln

import java.io.{FileNotFoundException, IOException}

import collection._
import scala.util.parsing.combinator._
import scala.io._
import scala.math._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.languages.ModelParser
import edu.ucla.cs.starai.forclift.languages.StatRelModel

/** Syntax used to output MLNs */
final object FormulaSyntaxMLN extends FormulaSyntax {
  final override val kwNegation = "!"
  final override val kwConjunction = "^"
  final override val kwDisjunction = "v"
  final override val kwImplication = "=>"
  final override val kwImplicationRev = "<="
  final override val kwEquivalence = "<=>"
  final override val kwWildcardNegation = "*"
  final override val kwExistential = "EXIST"
  final override val kwUniversal = "FORALL"
}

/**
 * Parser for MLNs
 *
 * Operator precedence:
 *
 *  - Atomic operators: `!=`, `==`
 *  - negation: `!` or wildcard negation: `*`
 *  - conjunction: `^`
 *  - disjunction: `v`
 *  - implication: `=>`
 *  - equivalence: `<=>`
 *  - quantifiers: `FORALL`/`Forall`/`forall`, `EXISTS`/`Exists`/`exists`
 *
 * Structure of .mln file:
 * {{{
 * // Example MLN theory containing all of the syntax.
 * / * Comments can also be in C++-like comment blocks. * /
 *
 * // Include statements
 * #include filename.mln
 *
 * // Domain declarations
 * flip = {1,...,20}
 * throw = {1,...,20}
 * face = {1,...,6}
 * people = {Guy,Nima,Wannes,Jesse,Luc}
 *
 * // Predicate declarations
 * // Starts with [a-zA-Z]
 * Heads(flip)
 * outcome(throw, face)
 *
 * // Exact one instance true (done in predicate definition)
 * // (What is the diff with using a function here?)
 * // todo: Not yet supported
 * outcome(throw, face!)
 *
 * // Functions
 * // todo: Not yet supported
 * person = motherOf(person)
 *
 * // Clauses
 *
 * // Unit clause
 * 1 Heads(f)
 *
 * // Weighted clauses
 * 1 Smokes(x) => Cancer(x)
 * 1 Friends(x,y) => (Smokes(x) <=> Smokes(y))
 *
 * // Existential quantifier
 * 1 Exist f Outcome(t, f)
 *
 * // Hard constraints (note full stop at end)
 * Exist f Outcome(t, f).
 * Outcome(t, f) ^ f != f' => !Outcome(t, f').
 *
 * // To be learnt clause
 * // No weight or full stop at end
 * Outcome(t, f)
 *
 * // Magically ground clause
 * 1 Outcome(t, +f)
 *
 * // Equality
 * // Is replaced by an eq/2 predicate
 * pub(t,p) ^ publ(t,s) ^ prof(p) ^ stud(s) ^ !(s = p) => advBy(s,p)
 *
 * // Internal functions and predicates
 * // Alchemy has operators for arithmetic and string operations.
 * // todo: Not yet supported
 * }}}
 */
class MLNParser extends JavaTokenParsers with ModelParser {

  import FormulaSyntaxMLN._

  // TODO remove learning modus flag in parser and have a separate function for parsing model structures
  var isLearnModus = false
  def setLearnModus(set: Boolean): Unit = { isLearnModus = set }
  var isDatabase = false

  var debug = false
  def printd(str: String): Unit = if (debug) println(str)

  //--- VALS ---
  val domainMap = new mutable.HashMap[String, (RootDomain, mutable.HashSet[Constant], Int)]
  /** Map from string to (Subdomain, Parent, Size).
   *  Subdomains cannot have constants, only sizes.
   *  They can also not be used to define predicates. **/
  val subDomainMap = new mutable.HashMap[String, (Domain, String, Int)]
  /**
   * Keeps track of the extra constants found in the database that do not
   * appear in the theory.
   */
  val dbDomainMap = new mutable.HashMap[RootDomain, mutable.HashSet[Constant]]
  val predicateMap = new mutable.HashMap[(String, Int), Predicate]
  val weightMap = new mutable.HashMap[Predicate, Weights]
  val varMap = new mutable.HashMap[String, Var]

  def parseModel(theoryString: String) = parseMLN(theoryString)
  
  def increaseDomainSize(name:String, with_size:Int): Domain = {
    subDomainMap.get(name) match {
      case Some((domain,parent,current_size)) => {
        subDomainMap.update(name, (domain, parent, current_size+with_size))
        increaseDomainSize(parent, with_size)
      }
      case _ => {
        domainMap.get(name) match {
          case Some((domain,constants,current_size)) => {
            domainMap.update(name, (domain,constants,current_size+with_size))
            domain
          }
          case _ => {
            throw new IllegalStateException("Did not find domain: "+name)
          }
        }
      }
    }
  }
  
  def domainSizes: DomainSizes = { 
    val roots = domainMap.values.map {
      case (k, v, s) =>
        (k.asInstanceOf[Domain], DomainSize(s, k.asInstanceOf[Domain], v.toSet))
    }
    val subs = subDomainMap.values.map {
      case (d, p, s) =>
        if (s == 0) {
          println(s"WARNING: Subdomain $d has size zero (if constants are "+
                   "intended they should start with an uppercase letter)")
        }
        (d.asInstanceOf[Domain], DomainSize(s, d.asInstanceOf[Domain], Set()))
    }
//    println(subDomainMap)
    val all = (roots++subs).toMap
//    println(all)
    all
  }
  
  def hasSubdomains: Boolean = {
    subDomainMap.size > 0
  }
  
  def dbDomainSizes: DomainSizes = new DomainSizes(dbDomainMap.map {
    case (k, v) =>
      (k.asInstanceOf[Domain], DomainSize(v.size, k.asInstanceOf[Domain], v.toSet))
  }.toMap, true)
  def predicateWeights: PredicateWeights = weightMap.toMap
  def predicateSet: Set[Predicate] = predicateMap.values.toSet.filterNot(_ == Predicate.eq)

  // @todo Prime is missing in regex (fails)
  val domainRe = """[a-z][a-zA-Z0-9_-]*""".r
  val predRe = """[a-zA-Z_][a-zA-Z0-9_-]*""".r
  val constRe = """[A-Z0-9][a-zA-Z0-9_-]*""".r
  val varRe = """[a-z][a-zA-Z0-9_-]*""".r
  val infixOpRe = """(=|\+|-|<|<=|>|>=|/|%)""".r
  val commentRe = """//.*""".r

  val EOF = """\Z""".r
  val CR = """\n""".r
  val CR2 = """\r""".r
  val CRLF = """\n\r""".r
  //val CRLFEOF = """\r|\n|\Z|\r\n|\n\r""".r
  val CRLFEOF = """[\r\n]+|\Z""".r
  val SP = """[\t ]*""".r
  val SPNL = """[\t\n\r ]*""".r
  val SPCR = """[\t\r\n ]*""".r
  val reserved: Parser[String] =
    ("""([Ee]xist|EXIST)\b""".r | """([Ff]orall|FORALL)\b""".r)

  // EOL is necessary to use as an identifier as and ending of a statement
  override def skipWhitespace: Boolean = false

  //--- LAZY VALS ---

  /** */
  lazy val variable: Parser[Var] = (
    not(reserved) ~> varRe ^^ { name =>
      varMap.getOrElseUpdate(name, new Var)
    }
    | failure("Expected a variable."))

  /** */
  lazy val constant: Parser[Constant] = (
    not(reserved) ~> constRe ^^ { new Constant(_) }
    | """["].*["]""".r ^^ { Constant(_) }
    | """``.*''""".r ^^ { Constant(_) }
    | failure("Expected a constant."))

  /** */
  lazy val term: Parser[Term] = (
    constant
    | variable
    | failure("Expected a term."))

  /**
   * Parses an atom and returns a Boolean list of those arguments that were
   * prefixed by a plus.
   *
   * Atoms do not yet support functions as arguments.
   */
  lazy val atom: Parser[(Atom, List[Boolean])] = (
    not(reserved) ~> predRe ~ "(" ~ rep1sep(opt("+") ~ term, """\s*,\s*""".r) <~ ")" ^^ {
      case name ~ _ ~ plusarglist => {
        val arglist = plusarglist.map { case p ~ t => t }
        val plusses = plusarglist.map {
          case p ~ t =>
            p match {
              case None => false
              case _ => true
            }
        }

        printd("  Parsed atom: " + name + " with args: " + arglist)
        //val predicate = predicateMap.getOrElseUpdate((name,arglist.size), new Predicate(Symbol(name),arglist.size))
        val predicate = predicateMap.getOrElse((name, arglist.size), {
          // @todo Hack to reduce stacktrace, should be solved more nicely
          var exc = new IllegalArgumentException("Unknown predicate " + name + "/" + arglist.size)
          // WHY DO WE HAVE THIS??? - Guy
          //            exc.setStackTrace(Array(new StackTraceElement("MLNParser", "ParseAtom", "MLNParser", 0) ))
          throw exc
        })

        if (isLearnModus || isDatabase) {
          // If in learning modus, add new constants encountered
          // while parsing automatically to the domain's known
          // constants.
          (arglist zip predicate.domains).foreach {
            case (a, d) =>
              a match {
                case cons: Constant => {
                  if (!d.contains(a.asInstanceOf[Constant])) {
                    if (isDatabase) {
                      dbDomainMap.getOrElseUpdate(d, {
                        new mutable.HashSet[Constant]
                      }).add(cons)
                    } else {
                      //d.addConstant(cons)
                      domainMap.get(d.name) match {
                        case Some((_, c, s)) => {
                          printd("Adding new constant " + cons + " to domain " + d.name)
                          c.add(cons)
                        }
                        case None => { println("Warning: domain not found: %s" format d.name) }
                      }
                    }
                  }
                }
                case _ => {}
              }
          }
        } else {
          // If not in learning modus, reject constants that are
          // not yet defined.
          (arglist zip predicate.domains).foreach {
            case (a, d) =>
              if (a.isInstanceOf[Constant] && !d.contains(a.asInstanceOf[Constant])) {
                throw new IllegalArgumentException("Constant " + a + " is not a constant in domain " + d + ".")
              }
          }
        }

        (predicate(arglist: _*), plusses)
      }
    }
    | not(reserved) ~> predRe ^^ {
      case name => {
        printd("  Parsed atom: " + name)
        val predicate = predicateMap.getOrElseUpdate((name, 0), new Predicate(Symbol(name), 0))
        (predicate(), Nil)
      }
    }
    | failure("Expected an atom."))

  /** Parse an atom and a to-be-expanded atom (+ syntax in Alchemy) */
  lazy val expatomformula: Parser[Formula] = (
    atom ^^ {
      case a => {
        if (a._2.exists(_ == true)) {
          AtomicExpFormula(a._1, a._2)
        } else {
          LiteralFormula(a._1)
        }
      }
    })

  /** Detect whether it is an infix operator working on variable names. */
  lazy val infixoperator: Parser[Formula] = (
    SP ~ term ~ SP ~ infixOpRe ~ SP ~ term ~ SP ^^ {
      case _ ~ t1 ~ _ ~ op ~ _ ~ t2 ~ _ => {
        op match {
          case "=" => {
            val predicate = predicateMap.getOrElseUpdate(("eq", 2), Predicate.eq)
            printd("   Parsed = operator: " + t1 + " = " + t2)
            LiteralFormula(predicate(t1, t2))
          }
          case _ => {
            throw new IllegalStateException("Unrecognized operator: " + op)
          }
        }
      }
    }
    | failure("Expected an infix operator (like != or ==)."))

  /**
   * A formula is considered as an atomic part if it is:
   *
   *  - An atom
   *  - A parenthized part
   *  - A negation of an atom or of a parenthized part
   *  - A quantified formula
   */
  lazy val atomicformula: Parser[Formula] = (
    infixoperator
    | expatomformula
    | "(" ~ SP ~> formula <~ SP ~ ")"
    | "[" ~ SP ~> formula <~ SP ~ "]" ^^ { IndivisibleSubformula(_) }
    | FormulaSyntaxMLN.kwNegation ~ SP ~> expatomformula ^^ { a => NegFormula(a) }
    | FormulaSyntaxMLN.kwNegation ~ SP ~ "(" ~ SP ~> formula <~ SP ~ ")" ^^ { NegFormula(_) }
    | "*" ~ SP ~> expatomformula ^^ { a => WildNegFormula(a) }
    | "*" ~ SP ~ "(" ~ SP ~> formula <~ SP ~ ")" ^^ { WildNegFormula(_) }
    | existformula
    | univformula
    | failure("Expected an atom (or parenthesis or atomic subformula)."))

  /** A set of conjunctions is recognized as a list of atomic formulae. */
  lazy val conjformula: Parser[Formula] = (
    atomicformula ~ rep(SPNL ~ kwConjunction ~ SPNL ~> atomicformula) ^^ {
      case head ~ Nil => head
      case head ~ rest => {
        rest.foldLeft(head)((r, c) => new ConjFormula(r, c))
      }
    }
    | failure("Expected a conjunction, negation or atom."))

  /**
   * A set of disjunctions is recognized as a list of conjunctions.
   * Left-associative.
   */
  lazy val disjformula: Parser[Formula] = (
    conjformula ~ rep(SPNL ~ kwDisjunction ~ SPNL ~> conjformula) ^^ {
      case head ~ Nil => head
      case head ~ rest => {
        rest.foldLeft(head)((r, c) => new DisjFormula(r, c))
      }
    }
    | failure("Expected a disjunction, conjunction, negation or atom."))

  /**
   * Left-associative implication operator
   * {{{
   * a => b => c == (a => b) => c
   * }}}
   *
   * @todo Double check whether Alchemy is left-associative because logic is
   * right-associative according to Huth & Ryan 2007.
   *
   * @todo an exist in an implication without parenthesis is not parsed
   */
  lazy val implformula: Parser[Formula] = (
    disjformula ~ rep1(SPNL ~ kwImplicationRev ~ SPNL ~> disjformula) ^^ {
      case head ~ Nil => head
      case head ~ rest => {
        val l = (head :: rest).reverse
        l.tail.foldLeft(l.head)((r, c) => new ImplFormula(r, c))
      }
    }
    | disjformula ~ rep(SPNL ~ kwImplication ~ SPNL ~> disjformula) ^^ {
      case head ~ Nil => head
      case head ~ rest => {
        rest.foldLeft(head)((r, c) => new ImplFormula(r, c))
      }
    }
    | failure("Expected an implication, disjunction, conjunction, negation or atom."))

  /**
   * Left-associative equivalence operator
   * {{{
   * a <=> b <=> c == (a <=> b) <=> c
   * }}}
   *
   * @todo Double check whether Alchemy is left-associative because logic is
   * right-associative according to Huth & Ryan 2007.
   */
  lazy val eqformula: Parser[Formula] = (
    implformula ~ rep(SPNL ~ FormulaSyntaxMLN.kwEquivalence ~ SPNL ~> implformula) ^^ {
      case head ~ Nil => head
      case head ~ list => {
        list.foldLeft(head)((r, c) => new EqFormula(r, c))
      }
    }
    | failure("Expected an equivalence, implication, disjunction, conjunction, negation or atom."))

  /** Universal quantifier. */
  lazy val univformula: Parser[Formula] = (
    SP ~ """([Ff]orall|FORALL)""".r ~ SP ~> repsep(variable, SP ~ "," ~ SP) ~ SP ~ quantformula ^^ {
      case e ~ _ ~ f => ForallFormula(e, f)
    }
    | failure("Expected a universal quantifier."))

  /** Existential quantifier. */
  lazy val existformula: Parser[Formula] = (
    SP ~ """([Ee]xist|EXIST)""".r ~ SP ~> repsep(variable, SP ~ "," ~ SP) ~ SP ~ quantformula ^^ {
      case e ~ _ ~ f => ExistFormula(e, f)
    }
    | failure("Expected an existential quantifier."))

  lazy val quantformula: Parser[Formula] = (
    existformula
    | univformula
    | eqformula ^^ {
      case f => {
        printd("  Parsed formula without quantifier: " + f)
        f
      }
    }
    | failure("Expected an quantifier, implication, disjunction, conjunction, negation or atom."))

  /** A formula is an equivalence formula (if present). */
  lazy val formula: Parser[Formula] = (
    quantformula ^^ {
      case q => {
        printd(" Parsed formula: " + q)
        q
      }
    }
    | failure("Expected a formula."))

  /** A weighted formula is a double followed by a formula. */
  lazy val wformula: Parser[WeightedFormula] = (
    floatingPointNumber ~ """\s+""".r ~ formula ~ opt("""\s*,\s*""".r ~> constraints) <~ CRLFEOF ^^ {
      case prob ~ _ ~ form ~ con_o => {
        val con = con_o match {
          case None => Constraints.empty
          case Some(cons) => cons
        }
        printd(" Parsed weighted formula: " + prob + " " + form)
        WeightedFormula(form, prob.toDouble, false, con).standardizeApart
      }
    }
    | failure("Expected a weighted formula."))

  /**
   * Parse hard or to be learned formulas.
   *
   *  - A hard formula is a formula ending with a full stop.
   *  - A learned formula is a formua without full stop or weight.
   */
  lazy val hardlearnformula: Parser[Any] = (
    formula ~ SP ~ opt("""\s*,\s*""".r ~> constraints) ~ opt(".") <~ CRLFEOF ^^ {
      case f ~ _ ~ con_o ~ dot => {
        val con = con_o match {
          case None => Constraints.empty
          case Some(cons) => cons
        }
        dot match {
          case None => {
            printd("Parsed learn formula: " + f)
            if (isLearnModus) {
              WeightedFormula(f, 0, false, con).standardizeApart
            } else {
              throw new IllegalStateException(
                "Parsing theory failed for\n" +
                  f + "\n" +
                  "Formulas with to be learned weights are not allowed for performing inference.\n" +
                  "Did you mean to learn weights? Check whether you have used the --wl flag.\n\n")
              "# " + f.toString
            }
          }
          case _ => {
            printd("Parsed hard formula: " + f)
            WeightedFormula(f, 0, true, con).standardizeApart
          }
        }
      }
    }
    | failure("Expected a hard or to be learned formula."))

  /** */
  lazy val constraints: Parser[Constraints] = (
    rep1sep((ineqConstr | domConstr), """\s*,\s*""".r) ^^ {
      matches => {
        val vt = matches.collect{case (v:Var,t:Term) => (v,t)}
        val vd = matches.collect{case (v:Var,d:Domain) => (v,d)}
        Constraints(
          ineqConstrs = IneqConstr(vt: _*),
          elemConstrs = ElemConstr(vd: _*)
        )
      }
    }
    | failure("Illegal constraints")
  )
    
  /** */
  lazy val ineqConstr: Parser[(Var,Term)] = (
    variable ~ ("""\s*!=\s*""".r | """\s*≠\s*""".r) ~ term ^^ {
      case v ~ _ ~ a => {
        (v, a)
      }
    }
    | failure("Illegal inequality constraint")
  )
  
  /** */
  lazy val domConstr: Parser[(Var,Domain)] = (
    variable ~ """\s*in\s*""".r ~ domainRe ^^ {
      case v ~ _ ~ dom_name => {
        domainMap.get(dom_name) match {
          case Some((dom,_,_)) => (v, dom)
          case None => {
            subDomainMap.get(dom_name) match {
              case Some((dom,_,_)) => (v, dom)
              case None => throw new IllegalStateException("Domain not known: "+dom_name)
            }
          }
        }
      }
    }
    | failure("Illegal inequality constraint")
  )

  /**
   * A domain declaration starts with a domain name followed by an equal sign
   * and a list enclosed by curly braces. Integer domains can be specified
   * with a range (e.g., `3...8`).
   */
  lazy val domainDeclaration: Parser[Domain] = (
    domainRe ~ """\s*=\s*\{\s*""".r ~ repsep(constant, """\s*,\s*""".r) ~ """\s*\}""".r ^? {
      case name ~ _ ~ _ ~ _ if subDomainMap.contains(name) => {
        throw new IllegalStateException("Subdomain needs to be defined with size ar recursive subdomains: "+name)
      }
      case name ~ _ ~ constants ~ _ => {
        domainMap.getOrElseUpdate(name, (new RootDomain(name, constants.toList),
          mutable.HashSet(constants: _*), constants.length))._1
      }
    }
    | domainRe ~ """\s*=\s*""".r ~ wholeNumber ~ """\s*\{\s*\}""".r ^? {
      case name ~ _ ~ size ~ _ if subDomainMap.contains(name) => {
        increaseDomainSize(name, size.toInt)
      }
      case name ~ _ ~ size ~ _ => {
        domainMap.getOrElseUpdate(name, (new RootDomain(name, Nil), mutable.HashSet(), size.toInt))._1
      }
    }
    | domainRe ~ """\s*=\s*""".r ~ wholeNumber ~ """\s*\{\s*""".r ~ rep1sep(constant, """\s*,\s*""".r) <~ """\s*\}""".r ^^ {
      case name ~ _ ~ size ~ _ ~ constants => {
        domainMap.getOrElseUpdate(name, (new RootDomain(name, constants.toList), mutable.HashSet(constants: _*), size.toInt))._1
      }
    }
    | domainRe ~ """\s*=\s*\{\s*""".r ~ "[0-9]+".r ~ """\s*,\s*\.\.\.\s*,\s*""".r ~ "[0-9]+".r <~ """\s*\}""".r ^^ {
      case name ~ _ ~ start ~ _ ~ end => {
        val constants = (start.toInt to end.toInt).toList.map { i => Constant(i.toString) }
        domainMap.getOrElseUpdate(name, (new RootDomain(name, constants),
          mutable.HashSet(constants: _*), constants.length))._1
      }
    }
    | domainRe ~ """\s*=\s*\{\s*""".r ~ repsep(domainRe, """\s*,\s*""".r) ~ """\s*\}""".r ^^ {
      case name ~ _ ~ subdomains ~ _ => {
        val new_domain = new RootDomain(name, Nil)
        val (last_parent,last_name,cnt) = subdomains.toList
              .take(subdomains.length-2)
              .foldLeft[(Domain,String,Int)]((new_domain,name,1)){case ((cur_domain,cur_name,cnt),sd_name) =>
          // TODO: is ComplementDomain used correctly?
          
          val new_sd = new SubDomain("o_"+cnt, "o_"+sd_name, cur_domain, Set()) {
            override lazy val complement = new ComplementDomain(cnt.toString, sd_name, cur_domain, this, Set()) {
              override def toString() = sd_name
            }
            override def toString() = "o_"+sd_name
          }
          val new_sd_c = new_sd.complement
          subDomainMap.update(new_sd.toString, (new_sd, cur_name, 0))
          subDomainMap.update(sd_name, (new_sd_c, cur_name, 0))
          (new_sd, new_sd.toString, cnt+1)
        }

        val new_sd = new SubDomain((cnt+1).toString, "o_"+subdomains(subdomains.length-2), last_parent, Set()) {
          override lazy val complement = new ComplementDomain(cnt.toString, subdomains.last, last_parent, this, Set()) {
            override def toString() = subdomains(subdomains.length-1)
          }
          override def toString() = subdomains(subdomains.length-2)
        }
        subDomainMap.update(subdomains(subdomains.length-1), (new_sd.complement, last_name, 0))
        subDomainMap.update(subdomains(subdomains.length-2), (new_sd, last_name, 0))
        
        domainMap.getOrElseUpdate(name, (new_domain, mutable.HashSet(), 0))._1
      }
    }
    | failure("Illegal domain declaration"))

  /**
   * Predicate and function declarations
   *
   * @todo Function declarations
   */
  lazy val typeDeclaration: Parser[Predicate] = (
    predRe ~ opt("(" ~ SP ~> repsep(domainRe ~ opt("!"), """\s*,\s*""".r) <~ SP ~ ")") ~ opt("""\s+""".r ~> floatingPointNumber ~ """\s+""".r ~ floatingPointNumber) <~ CRLFEOF ^^ {
      case predicateName ~ arglist ~ predw =>
        printd("Parsed type declaration: " + predicateName + " with args: " + arglist)
        val domainNames = arglist match {
          case Some(domainNames) => domainNames.map {
            case name ~ None => name
            case name ~ exmark => {
              throw new IllegalStateException("Parsing theory failed. The ! operator in a predicate declaration is not yet supported.")
              name
            }
          }
          case None => List()
        }
        val domains = domainNames.map { domainName =>
          domainMap.getOrElseUpdate(domainName, {
            // Hack to reduce stacktrace, should be solved more nicely
            //var exc = new IllegalStateException("Unknown domain "+domainName)
            //exc.setStackTrace(Array(new StackTraceElement("MLNParser", "ParseType", "MLNParser", 0) ))
            //throw exc

            // Create new empty domain if not yet known
            (new RootDomain(domainName, Nil), mutable.HashSet(), 0)
          })._1
        }
        val arity = domains.size
        require(!predicateMap.contains((predicateName, arity)),
          "Predicate " + predicateName + "/" + arity + " is already known. This can occur when learning unit clause weights, whose initial weight is not given.")
        val predicate = new Predicate(Symbol(predicateName), arity, domains)
        predicateMap((predicateName, arity)) = predicate
        predw match {
          case None => {}
          case Some(wt ~ _ ~ wf) => {
            weightMap.put(predicate, Weights(wt.toDouble, wf.toDouble))
          }
        }
        predicate
    }
    | failure("Illegal type declaration"))

  /** Include statements */
  lazy val includeLine: Parser[List[Any]] = (
    "#include " ~ SP ~> """[\S]+""".r <~ CRLFEOF ^^ {
      case filename => {
        //println("Currently in directory: "+System.getProperty("user.dir"))
        //println("Trying to open file: "+filename)
        val inputfile = scala.io.Source.fromFile(filename)
        try {
          val filelines = inputfile.mkString
          parseAll(anyLines, rmCommentBlocks(filelines)).get
        } catch {
          case e: IOException => throw new Exception(s"Problem with file $filename: " + e.getMessage)
          case e: FileNotFoundException => throw new Exception(s"Can't find file $filename")
        }
        finally {
          inputfile.close()
        }
      }
    })

  /**
   * Parse a double forward slash comment line.
   *
   * Note that C++ like comment blocks are removed in a preprocessing phase
   * since they can appear everywhere and would be too much of an overhead
   * to actually parse.
   */
  lazy val commentLine: Parser[Any] = (
    commentRe <~ CRLFEOF ^^ {
      case c => {
        printd("Parsed comment line: " + c)
        c
      }
    })

  lazy val emptyLine: Parser[Any] = (
    """^([ \t]*[\n\r])+""".r ^^ {
      case e => {
        //printd("Parsed empty line")
        e
      }
    }
    | """^[ \t]+\Z""".r ^^ {
      case e => {
        //printd("Parsed empty last line")
        e
      }
    }
  )

  /**
   * Returns a line in the MLN.
   * Note that a line can return multiple lines (e.g., include).
   */
  lazy val anyLine: Parser[List[Any]] = (
    domainDeclaration ^^ { a => printd("-> Done parsing domain declaration: " + a); List(a) }
    | typeDeclaration ^^ { a => printd("-> Done parsing type declaration: " + a); List(a) }
    | wformula ^^ { a => printd("-> Done parsing wformula: " + a); List(a) }
    | commentLine ^^ { a => printd("-> Done parsing comment line: " + a); List(a) }
    | emptyLine ^^ { a => printd("-> Done parsing empty line"); List(a) }
    | hardlearnformula ^^ { a => printd("-> Done parsing heardlearnformula: " + a); List(a) } // hardFormula needs to be after typeDeclaration!
    | includeLine ^^ { a => printd("-> Done parsing include line: " + a); a }
    | failure("Line could not be parsed."))

  lazy val anyLines: Parser[List[Any]] = (
    rep(anyLine) ^^ { _.flatten })

  lazy val dbLine: Parser[Any] = (
    SPNL ~> atom <~ SPNL ^^ {
      _ match {
        case (a, p) => LiteralFormula(a)
      }
    }
    | SPNL ~> "!" ~> atom <~ SPNL ^^ {
      _ match {
        case (a, p) => LiteralFormula(a, false)
      }
    }
    | commentLine
    | emptyLine
    | failure("Illegal database line"))

  lazy val dbLines: Parser[List[Any]] = (
    rep(dbLine))

  //--- DEFS ---

  /**
   * Remove C++ like comment blocks before parse.
   *
   * C++ like comment blocks are removed in a preprocessing phase since they
   * can appear everywhere and it would be too much overhead to actually
   * parse.
   *
   * @param  input
   *         String containing C++ block comments
   * @return String with C++ block comments removed
   */
  def rmCommentBlocks(input: String): String = {
    if (input.length == 0)
      return input

    var commentlevel = 0
    var output = new mutable.StringBuilder

    var i = 0
    while (i < input.length - 1) {
      val ss = input.substring(i, i + 2)
      if (ss == "/*") {
        commentlevel += 1
        i += 1
      } else if (ss == "*/") {
        commentlevel -= 1
        i += 1
      } else if (commentlevel == 0) {
        output.append(input.charAt(i))
      }
      i += 1
    }
    if (commentlevel == 0) {
      output.append(input.charAt(i))
    }
    output.toString
  }

  /**
   * Add additional weighted formulas that are necessary for the parsed
   * theory.
   *
   * For example, when the =-infix operator is incountered, an `eq/2` predicate
   * needs to be defined.
   */
  def additionalWeightedFormulas(): List[WeightedFormula] = {
    //if (predicateMap.contains(("eq", 2))) {
    //val predicate = predicateMap.getOrElse(("eq",2), throw new IllegalStateException("Unknown predicate eq/2"))
    //val varX = new Var
    //val varY = new Var
    //WeightedFormula(LiteralFormula(predicate(varX, varX)), 0, true) ::
    //WeightedFormula(LitearlFormula(predicate(varX, varY), false), 0, true, IneqConstr((varX,varY))) ::
    //Nil
    //} else {
    //Nil
    //}
    Nil
  }

  /**
   * Parse an MLN
   *
   * @param   mlnStr
   *          String containing an MLN.
   */
  def parseMLN(mlnStr: String): MLN = {
    //println("Start MLN parsing")
    // The .mln file contains the MLN for inference
    //val mlnStrNoComments = """(?s).*?/\*.*?\*/.*""".r replaceAllIn ( mlnStr, "" )
    printd("\nStart parsing\n")
    val mlnlines = parseAll(anyLines, rmCommentBlocks(mlnStr))
    mlnlines match {
      case Failure(msg, next) => {
        System.err.println("Error while parsing MLN (line " + next.pos.line + ", column " + next.pos.column + "):\n" + msg + "\n" + next.pos.longString)
        //throw new IllegalStateException("Parsing theory failed.")
        //println(lines1)
        MLN() // Return empty MLN
      }
      case _ => {
        val wformulas = mlnlines.get.collect { case line: WeightedFormula => line }
        val wformulas2 = wformulas.filterNot { _.isEmpty } ::: additionalWeightedFormulas()
        MLN(wformulas2, predicateSet, domainSizes, Nil, predicateWeights)
      }
    }
  }

  @deprecated("no longer in use?","3.0")
  def parseMLNWithDB(mlnStr: String, dbStr: String, useExplicitConstants: Boolean = false): MLN = {
    //println("Start MLN parsing")
    // The .mln file contains the MLN for inference
    //val mlnStrNoComments = """(?s).*?/\*.*?\*/.*""".r replaceAllIn ( mlnStr, "" )
    printd("\nStart parsing\n")
    val mlnlines = parseAll(anyLines, rmCommentBlocks(mlnStr))

    mlnlines match {
      case Failure(msg, next) => {
        System.err.println("Error while parsing MLN (line " + next.pos.line + ", column " + next.pos.column + "):\n" + msg + "\n" + next.pos.longString)
        throw new IllegalStateException("Parsing theory failed.")
        //println(lines1)
        //MLN() // Return empty MLN
      }
      case _ => {
        printd("\nStart parsing db\n")
        val dblines = parseAll(dbLines, dbStr)

        dblines match {
          case Failure(msg, next) => {
            System.err.println("Error while parsing database (line " + next.pos.line + ", column " + next.pos.column + "):\n" + msg + "\n" + next.pos.longString)
            throw new IllegalStateException("Parsing database failed.")
            new MLN // Return empty MLN
          }
          case _ => {
            val wformulas = mlnlines.get.collect { case line: WeightedFormula => line }
            val wformulas2 = wformulas.filterNot { _.isEmpty } ::: additionalWeightedFormulas()
            val ev = dblines.get.collect { case line: Formula => line }
            //val ds = dbDomainSizes.makeConsistentWith(domainSizes)
            //MLN(Nil, predicateSet, ds, ev)

            val curDomainSizes = if (useExplicitConstants) {
              domainSizes.asExplicitConstants
            } else { 
              domainSizes.asStaticConstants
            }

            MLN(wformulas2, predicateSet, curDomainSizes, ev, predicateWeights)
          }
        }
      }
    }
  }

  /**
   * Parse an Alchemy database based on the previously parsed theory. The
   * results of parsing the database are not persistent and are stored as
   * evidence in an MLN instance. This MLN should be seen as complementary
   * to the orginal MLN obtained with parseMLN.
   *
   * @param   dbStr
   *          The contents of a .db file containing the evidence
   */
  def parseDB(dbStr: String): MLN = {
    dbDomainMap.clear()
    isDatabase = true
    val lines = parseAll(dbLines, rmCommentBlocks(dbStr))
    isDatabase = false

    lines match {
      case Failure(msg, next) => {
        System.err.println("Error while parsing database (line " + next.pos.line + ", column " + next.pos.column + "):\n" + msg + "\n" + next.pos.longString)
        //throw new IllegalStateException("Parsing database failed.")
        new MLN // Return empty MLN
      }
      case _ => {
        val ev = lines.get.collect { case line: Formula => line }
        val ds = dbDomainSizes.makeConsistentWith(domainSizes)
        MLN(Nil, predicateSet, ds, ev)
      }
    }
  }

  /** Parse a string containing a single atom. */
  def parseAtom(atomStr: String): Atom = {
    parseAll(atom, atomStr) match {
      case Success(result, _) => result._1
      case Failure(msg, next) => {
        System.err.println("Error: Parsing atom failed:\n" + msg + "\n" + next.pos.longString)
        throw new IllegalStateException("Couldn't parse atom " + atomStr + ".")
      }
      case Error(msg, next) => {
        System.err.println("Error: Parsing atom failed:\n" + msg + "\n" + next.pos.longString)
        throw new IllegalStateException("Couldn't parse atom " + atomStr + ".")
      }
    }
  }
}
