package game.words

case class ActionKey(
  verb: Option[String],
  conjunctive: Option[String]
  )
  
object Actions {
  
  val list = List(
      Alias("é", "észak", "északra"),
      Alias("k", "kelet", "keletre"),      
      Alias("le", "menj le", "mássz le"),
      Alias("mássz"),
      Alias("nézd","n","nézd meg", "vizsgáld", "vizsgáld meg"),
      Alias("vedd","vedd fel"),
      Alias("tedd","tedd le","dobd el","dobd le"),
      Alias("told","told el", "mozgasd"),
      Alias("nyisd","nyisd ki", "tárd", "tárd ki"),
      Alias("leltár","l"),
      )
        
  def unhandledActionMessage(action: String) =     
    action match {
    case "é" | "k" | "d" | "ny" => "Nem tudsz abba az irányba menni."
    case _ => "Nem tudom hogyan kellene csinálni."
  }
     
  def getKey = (x: String) => 
    x.split(" ") match {
      case Array(a, b) => ActionKey(Some(a),Some(b))
      case Array(a)    => 
        if (conjunctives.contains(a)) ActionKey(None, Some(a))
        else ActionKey(Some(a), None) 
    }
       
  val conjunctives = list.foldLeft(List.empty[Option[String]])(
      (acc, item) => (item.aliases.toList :+ item.name)
        .map(_.split(" ").lift(1)) ::: acc)
        .filter(_.isDefined).map(_.get).toSet
     
  val actions =  list.foldLeft(Map.empty[ActionKey, String])((acc, item) => acc ++ 
      ((item.name :: item.aliases.toList) map (t => getKey(t) -> item.name)).toMap) 
      
  val verbs = actions.keys.map(_.verb).filter(_.isDefined).map(_.get).toSet   
}