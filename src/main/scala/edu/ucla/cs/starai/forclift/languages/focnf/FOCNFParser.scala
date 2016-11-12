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

package edu.ucla.cs.starai.forclift.languages.focnf

import scala.collection._
import scala.io._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.languages.ModelParser


class FOCNFParser extends ModelParser {
    
  val domainMap = new mutable.HashMap[String, (RootDomain, mutable.HashSet[Constant], Int)]
  // One entry per predicate name because in FOCNF we do not allow predicate overloading
  val predicateMap = new mutable.HashMap[String, Predicate]
  val weightMap = new mutable.HashMap[Predicate, Weights]
  val varMap = new mutable.HashMap[String, Var]
  val atomWeights = new mutable.ListBuffer[(Atom,Weights)]
  
  def domainSizes: DomainSizes = { 
    val roots = domainMap.values.map {
      case (k, v, s) =>
        (k.asInstanceOf[Domain], DomainSize(s, k.asInstanceOf[Domain], v.toSet))
    }
    roots.toMap
  }
  
  def parseModel(theoryString: String): FOCNF = parse(theoryString)
  
  def predicateWeights: PredicateWeights = weightMap.toMap
  def predicateSet: Set[Predicate] = predicateMap.values.toSet.filterNot(_ == Predicate.eq)
  
  def parseDomain(tokens:List[String], line_nbr:Int): List[Formula] = {
    tokens match {
      case "d" :: r_domain(name) :: r_int(size_str) :: constants_str => {
        val size = size_str.toInt
        val constants = constants_str.map{constant_str =>
          if (!r_cons.pattern.matcher(constant_str).matches) {
            throw new IllegalStateException(s"ERROR: Expected constant instead of $constant_str (line $line_nbr)")
          }
          Constant(constant_str)
        }
        val domain = new RootDomain(name, constants.toList)
        domainMap.getOrElseUpdate(name, (domain, mutable.HashSet(constants: _*), size))
      }
      case d :: tail if d != "d" => {
        throw new IllegalStateException(s"ERROR: Line should start with 'd ' (line $line_nbr)")
      }
      case "d" :: name :: size :: tail  => {
        val error_name = (if (r_domain.pattern.matcher(name).matches) "" else "Expected domain name on position 2, got $name. ")
        val error_size = (if (r_int.pattern.matcher(size).matches) "" else "Expected domain size on position 3, got $size. ")
        throw new IllegalStateException(s"ERROR: $error_name$error_size(line $line_nbr)")
      }
      case _ => {
        throw new IllegalStateException(s"ERROR: Line does not match domain declaration (line $line_nbr)")
      }
    }
    Nil
  } 
  
  def parseRelation(tokens:List[String], line_nbr:Int): List[Formula] = {
    tokens match {
      case "r" :: entry :: Nil => {
        val tokens_entry = tokenizeEntry(entry, line_nbr, 0)
        tokens_entry match {
          case r_pred(predicate_name) :: "(" :: domains_str => {
            if (domains_str.last != ")") {
              throw new IllegalStateException(s"ERROR: Domain declaration should end with a parenthesis (line $line_nbr)")
            }
            val domains = domains_str.slice(0,domains_str.length-1).map{ domain_name =>
              domainMap.getOrElse(domain_name,
                                  throw new IllegalStateException(s"Unknown domain $domain_name (line $line_nbr)"))._1
            }
            val arity = domains.size
            if (predicateMap.contains(predicate_name)) {
              throw new IllegalStateException(s"Predicate names cannot be overloaded: $predicate_name (line $line_nbr)")
            }
            val predicate = new Predicate(Symbol(predicate_name), arity, domains)
            predicateMap(predicate_name) = predicate
          }
          case name :: tail => {
            val error_name = (if (r_pred.pattern.matcher(name).matches) "" else "Expected predicate name on position 2, got $name. ")
            throw new IllegalStateException(s"ERROR: $error_name(line $line_nbr)")
          }
          case _ => {
            throw new IllegalStateException(s"ERROR: Entry does not match relation declaration ($line_nbr)")
          }
        }
      }
      case _ => {
        throw new IllegalStateException(s"ERROR: Line does not match relation declaration ($line_nbr)")
      }
    }
    Nil
  }
  
  def parseWeight(tokens:List[String], line_nbr: Int): List[Formula] = {
    tokens match {
      case "w" :: r_pred(pred_name) :: r_fp(wt) :: r_fp(wf) :: Nil => {
        val predicate = predicateMap.getOrElse(pred_name, 
         throw new IllegalStateException(s"ERROR: Unknown predicate: $pred_name (line $line_nbr)")   
        )
        val weights = Weights(wt.toDouble, wf.toDouble)
        weightMap.put(predicate, weights)
        Nil
      }
      case "w" :: atom :: r_fp(wt) :: r_fp(wf) :: Nil => {
        val tokens_atom = tokenizeEntry(atom, line_nbr, 2)
        tokens_atom match {
          case r_pred(pred_name) :: "(" :: terms_str => {
            if (terms_str.last != ")") {
              throw new IllegalStateException(s"ERROR: Atom should end with parenthesis (line $line_nbr)")
            }
            val terms = terms_str.slice(0,terms_str.length-1).map{term_str =>
              if (!r_cons.pattern.matcher(term_str).matches) {
                throw new IllegalStateException(s"ERROR: Expected constant, found $term_str (line $line_nbr)")
              }
              Constant(term_str)
            }
            val predicate = predicateMap.getOrElse(pred_name, 
             throw new IllegalStateException(s"ERROR: Unknown predicate: $pred_name (line $line_nbr)")   
            )
            val ws = Weights(wt.toDouble,wf.toDouble)
            val atom = predicate(terms:_*)
            atomWeights.append((atom, ws))
          }
          case _ => {
            throw new IllegalStateException(s"ERROR: Incorrect atom declaration $atom (line $line_nbr)")
          }
        }
      }
      case _ => {
        throw new IllegalStateException(s"ERROR: Line does not match weight declaration ($line_nbr)")
      }
    }
    Nil
  }
  
  def parseFormula(tokens:List[String], line_nbr:Int): List[Formula] = {
    //println("parseFormula: "+tokens.mkString(","))
    val (quants_str, lits_str) = tokens.partition{entity => entity(0) == '?' || entity(0) == '!'}
    val lits_formula:List[Formula] = lits_str.map{lit_str =>
      if (lit_str(0) == '-') {
        NegFormula(LiteralFormula(parseAtom(lit_str.tail, line_nbr)))
      } else {
        LiteralFormula(parseAtom(lit_str, line_nbr))
      }
    }
    val rlits_formula = lits_formula.reverse
    val lit_formula = rlits_formula.tail.foldLeft(rlits_formula.head){(r,c) => new DisjFormula(c,r)}
    
    val quant_formula = quants_str.foldRight(lit_formula){(c,r) =>
      val quant = c.head
      val var_str = c.tail
      val variable = varMap.getOrElseUpdate(var_str, new Var)
      quant match {
        case '?' => {
          new ExistFormula(variable :: Nil, r)
        }
        case '!' => {
          new ForallFormula(variable :: Nil, r)
        }
      }  
    }
    quant_formula :: Nil
  }
  
  def parseAtom(atomString:String): Atom = parseAtom(atomString,0)
  
  def parseAtom(atom_str:String, line_nbr:Int): Atom = {
    //println(s"Parsing atom: $atom_str")
    val tokens = tokenizeEntry(atom_str, line_nbr, 0)
    val pred = predicateMap.getOrElse(tokens.head,
      throw new IllegalStateException(s"ERROR: Unknown predicate: ${tokens.head} (line $line_nbr)")
    )
    tokens.tail match {
      case Nil => pred()
      case "(" :: tail => {
        if (tail.last != ")") {
          throw new IllegalStateException(s"ERROR: Expected closing parenthesis ($line_nbr)")
        }
        val terms = tail.slice(0,tail.length-1).zip(pred.domains).map{case (term_str,d) =>
          term_str match {
            case r_cons(c_str) => {
              val c = Constant(c_str)
              if (!d.contains(c)) {
                throw new IllegalStateException(s"Constant $c_str is not a constant in domain $d.")
              }
              c
            }
            case r_var(v) => varMap.getOrElseUpdate(v, new Var)
            case _ => {
              throw new IllegalStateException(s"ERROR: Expected constant or variable, found $term_str ($line_nbr)")
            }
          }
        }
        pred(terms:_*)
      }
      case s :: tail => {
        throw new IllegalStateException(s"ERROR: Unexpected symbol '$s', expected opening parenthesis ($line_nbr)")
      }
    }
  }
  
  val r_whitespace = """[ \t]""".r
  val r_quote = """["']""".r
  val r_symbol = """([-?!])""".r
  val r_domain = """([a-z][a-zA-Z0-9_-]*)""".r
  val r_pred = """([a-zA-Z_][a-zA-Z0-9_-{}]*)""".r
  val r_cons = """([A-Z0-9][a-zA-Z0-9_-]*)""".r
  val r_var = """([a-z][a-zA-Z0-9_-]*)""".r
  val r_int = """([0-9][0-9]*)""".r
  val r_fp = """([-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)""".r
  
  def tokenizeLine(line:String, line_nbr:Int): List[String] = {
    val (tokens,token,quote,paren) = line.zipWithIndex.foldLeft((List[String](),"",0,0)){(r,c) =>
      (r,c) match {
        case ((l,"",0,p),(r_quote(),_))      => (l   ,"\""  ,1,p)
        case ((l,t ,1,p),(r_quote(),_))      => (l   ,t+"\"",0,p)
        case ((l,t ,0,p),('(',_))            => (l   ,t+"(" ,0,p+1)
        case ((l,t ,0,p),(')',_))            => (l   ,t+")" ,0,p-1)
        case ((l,"",0,0),(r_whitespace(),_)) => (l   ,""    ,0,0)
        case ((l,t ,0,0),(r_whitespace(),_)) => (t::l,""    ,0,0)
        case ((l,t ,q,p),(c  ,_))            => (l   ,t+c   ,q,p)
        case (_,(_,n)) => {
          throw new IllegalStateException(s"ERROR: Illegal symbol: $c ($line_nbr:$n)") 
        }
      }
    }
    if (quote == 1) {
      throw new IllegalStateException(s"ERROR: No matching quotes ($line_nbr:${line.length})")
    }
    val alltokens = token::tokens
    alltokens.reverse
  }
  
  def tokenizeEntry(line:String, line_nbr:Int, line_offset:Int): List[String] = {
    val (tokens,token,quote,paren) = line.zipWithIndex.foldLeft((List[String](),"",0,0)){(r,c) =>
      (r,c) match {
        case ((l,"",0,p),(r_quote(),_))      => (l          ,"\""  ,1,p)
        case ((l,t ,1,p),(r_quote(),_))      => (l          ,t+"\"",0,p)
        case ((l,t ,0,p),('(',_))            => ("("::t::l  ,""    ,0,p+1)
        case ((l,t ,0,p),(')',_))            => (")"::t::l  ,""    ,0,p-1)
        case ((l,"",0,p),(r_symbol(s),_))    => (s"$s"::l   ,""    ,0,p)
        case ((l,t ,0,p),(r_symbol(s),_))    => (s"$s"::t::l,""    ,0,p)
        case ((l,t ,0,p),(',',_))            => (t::l       ,""    ,0,p)
        case ((l,"",0,p),(r_whitespace(),_)) => (l          ,""    ,0,p)
        case ((l,t ,0,p),(r_whitespace(),_)) => (t::l       ,""    ,0,p)
        case ((l,t ,q,p),(c  ,_))            => (l          ,t+c   ,q,p)
        case (_,(_,n)) => {
          throw new IllegalStateException(s"ERROR: Illegal symbol: $c ($line_nbr:${line_offset+n}") 
        }
      }
    }
    if (quote == 1) {
      throw new IllegalStateException(s"ERROR: No matching quotes ($line_nbr:${line_offset+line.length})")
    }
    val alltokens = token match {
      case "" => tokens
      case _ => token::tokens
    }
    alltokens.reverse
  }
  
  def parseLine(line: String, line_nbr: Int): List[Formula] = {
    val tokens = tokenizeLine(line, line_nbr)
    tokens match {
      case "p" :: _ => {Nil} // CNF type
      case "c" :: _ => {Nil}  // Comment
      case "w" :: _ => {parseWeight(tokens, line_nbr)}          // Weight
      case "r" :: _ => {parseRelation(tokens, line_nbr)}        // Relation
      case "d" :: _ => {parseDomain(tokens, line_nbr)}          // Domain
      case _   => {parseFormula(tokens, line_nbr)}          // Formula
    }
  }
  

  def parse(theoryStr: String): FOCNF = {
    val lines = theoryStr.lines.map{_.trim}.zipWithIndex.filter{case (line,nbr) => line.nonEmpty}
    val formulas = lines.flatMap{case (line,nbr) => parseLine(line, nbr+1)}.toList
    val formulas2 = formulas.map(_.standardizeApart)
    FOCNF(formulas2,
          predicateSet.toSet,
          domainSizes,
          predicateWeights,
          atomWeights.toList)
  }

}
