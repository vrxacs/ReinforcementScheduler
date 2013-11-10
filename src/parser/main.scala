package parser

import scala.xml._
import scala.collection.mutable.HashMap
import scala.collection.immutable._
import reinforcement._

object main {

  def main(args: Array[String]): Unit = {
    
    val filename = "data.xml"
    
    val xml = XML.loadFile(filename)
    
    assert(xml.isInstanceOf[scala.xml.Elem])
    
    val slots = getSlots((xml \ "slots")(0))
    val profs = getProfs((xml \ "prof")(0))
    val classes = getClasses((xml \ "class")(0))
    val rooms = getRooms((xml \ "rooms")(0))
    
    var s = new Schedule(slots, rooms, profs, classes)

    s.printRoomSlots

    println("========= Starting State")
    
    s.roomSlots = s.getStartingState
    s.printRoomSlots
    
    println("========= Reinforcement Learning")
    
    var rl = new TDRL(s)
    rl.reinforcementLearning
  }
  
  def getRooms(xml: scala.xml.NodeSeq) : Array[Room] = {
    var ls = List[Room]()
    
    for(l <- (xml \\ "room"))
    	ls = new Room((l \ "@id").text.toInt, (l \ "@name").text) :: ls
    
    ls.reverse.toArray
  }
  
  def getClasses(xml: scala.xml.NodeSeq) : Map[String, Class] = {
    var ls = List[Class]();
    
    for(l <- (xml \\ "lecture")){
      var prefs = List[Int]()
      for(pr <- (l \\ "pref"))
    	  prefs = pr.text.toInt :: prefs
      ls = new Class((l \ "@id").text, (l \ "@prof").text, prefs) :: ls
    }
      
    ls.reverse.map(p => (p.id -> p)).toMap
  }
  
  def getProfs(xml: scala.xml.NodeSeq) : HashMap[String, List[Int]] ={
    val map = new HashMap[String, List[Int]]
    
    for(l <- (xml \\ "pr")){
    	var ls = List[Int]()
    
    	for(k <- (l \\ "slot"))
    	  ls = k.text.toInt :: ls 
    	  
    	map.put((l \ "@name").text, ls)
    }
    
    map
  }
  
  def getSlots(xml: scala.xml.NodeSeq) : Array[Slot] = {
    var ls = List[Slot]()

	for(l <- (xml \ "time"))
	    ls = new Slot((l \ "@id").text.toInt, l.text, "") :: ls

    ls.reverse.toArray
  }

}