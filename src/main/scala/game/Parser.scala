package game

import cats.implicits._
import game.words._
import cats.Monoid
import monocle.macros._
import util.Utils._

object Parser {

  final case class ParsedCommand(
    cmd:       Command,
    verb:      Option[String],
    adjective: Option[String])

  val cmdLens = GenLens[ParsedCommand](_.cmd)
  val actionLens = cmdLens composeLens GenLens[Command](_.action)
  val subjectInfoLens = cmdLens composeLens GenLens[Command](_.subjectInfos)
  val verbLens = GenLens[ParsedCommand](_.verb)
  val adjectiveLens = GenLens[ParsedCommand](_.adjective)
  
  implicit val monoid: Monoid[ParsedCommand] = new Monoid[ParsedCommand] {
    def combine(a: ParsedCommand, b: ParsedCommand) = ???
    def empty = ParsedCommand(Command(None, Set(), Set()), None, None)
  }

  def parse(input: String) =
    input.split(" ").foldLeft(Monoid[ParsedCommand].empty.pure[WithError]) 
      { (acc, item) => acc.flatMap { parseWord(item) } } |> 
      { _.flatMap(x => Right(parseEnd(x))) } map (_.cmd)
 
  val articles = Set("a", "az")
 
  def parseWord(word: String)(parsed: ParsedCommand): Either[Error, ParsedCommand] =    
    if (articles.contains(word)) Right(parsed.copy())   
    else if (Actions.verbs.contains(word)) Right(parseVerb(word, parsed))
    else if (Actions.conjunctives.contains(word)) Right(parseConjunctive(word, parsed))     
    else Nouns.nouns.get(word).flatMap(x => Some(Right(parseNoun(x, parsed))))
      .orElse(Adjectives.adjectives.get(word).flatMap(x => Some(Right(parseAdjective(x, parsed)))))
      .getOrElse(Left(ParseError(word)))
      
  def parseVerb(word: String, parsed: ParsedCommand) = verbLens.set(Some(word))(parsed)

  def parseAdjective(word: String, parsed: ParsedCommand) =
    adjectiveLens.set(Some(word))(parsed)

  def parseNoun(word: String, parsed: ParsedCommand) =    
      (subjectInfoLens.modify(_ + SubjectInfo(parsed.adjective, word)) andThen
      adjectiveLens.set(None))(parsed)

  def parseConjunctive(word: String, parsed: ParsedCommand) =
    (actionLens.set(Actions.actions.get(ActionKey(parsed.verb, Some(word)))) andThen
      verbLens.set(None))(parsed)

  def parseEnd(parsed: ParsedCommand) =
    actionLens.set(
        if (parsed.verb.isDefined) Actions.actions.get(ActionKey(parsed.verb, None))
        else parsed.cmd.action)(parsed)

}