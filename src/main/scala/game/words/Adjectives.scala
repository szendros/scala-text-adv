package game.words

object Adjectives {
  
   val list = List(
      Alias("kis","kicsi","apró"),
      Alias("nagy","óriás", "hatalmas"),    
      )     
      
  def getMap(x:Alias) : Map[String, String] =
    ((x.name :: x.aliases.toList) map 
      (t => t -> x.name)).toMap 
            
  val adjectives =  list.foldLeft(Map.empty[String, String])(_ ++ getMap(_)) 
}