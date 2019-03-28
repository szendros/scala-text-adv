package game.words

object Nouns {
  
 val list = List(
      Alias("szekrény","szekrényt"),
      Alias("ágy","ágyat", "ágyra"),
      Alias("ajtó","ajtót"),
      Alias("alma","almát"),
      Alias("ablak","ablakot"),
      Alias("asztal","asztalt","asztalra"),
      Alias("kulcs","kulcsot","kulccsal"),
      Alias("doboz","dobozt"),
      )      
  
  def getMap(x:Alias) : Map[String, String] =
    ((x.name :: x.aliases.toList) map 
      (t => t -> x.name)).toMap 
            
  val nouns =  list.foldLeft(Map.empty[String, String])(_ ++ getMap(_))  
}