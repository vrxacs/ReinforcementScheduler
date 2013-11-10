package parser

import scala.collection.mutable.HashMap
import scala.collection.immutable._
import scala.collection.JavaConverters._

class Room(var id: Int, var name: String) {
  override def toString() : String = {
    id + " (" + name + ")"
  }
}

class Class(var id: String, var prof: String, var prefs : List[Int]) {
  override def toString() : String = {
    id + " (" + prof + ")"
  }
}

case class Slot(val id: Int, val time: String, var classID: String) {
  override def toString() : String = {
    id + " (" + time + ") -> " + classID
  }
  def copy : Slot = {
    new Slot(id, time, classID)
  }
}

class Schedule(val slots: Array[Slot], val rooms : Array[Room], 
				val profs : HashMap[String, List[Int]], val classes : Map[String, Class] ) {
	var roomSlots = getRoomSlots
	
	// TODO: handle error if there are more classes than slots
	// TODO: handle error if a prof has more classes than slots he's willing to teach during
	def getStartingState = {
	  var roomSlots = getRoomSlots
	  var allSlots = for((roomID, sl) <- roomSlots; slot <- sl) yield slot
	  (allSlots, classes.keys).zipped.map((s : Slot, id : String) => s.classID = id)
	  roomSlots
	}
	
	// FUTURE: if the branching factor is too high, use stochastic sampling
	def getPossibleNextStates = {
	  var ls = List[Schedule]()
	  var sl = for((r, a) <- roomSlots; s <- a) yield (r, s)
	
	  // TODO: remove multiple instances
	  for((cRoom, cSlot) <- sl; (fRoom, fSlot) <- sl if cRoom != fRoom || cSlot.id != fSlot.id) {
	    val s = new Schedule(slots, rooms, profs, classes)
	    s.roomSlots = copyRoomSlots
	    s.Switch(cRoom, cSlot, fRoom, fSlot)
	    ls = s :: ls
	  }
	  
	  ls
	}
	
	def roomSlotsEquals(other : Schedule) : Boolean = {
	  var temp = true
	  if(roomSlots.length != other.roomSlots.length) temp = false
	  for(i <- 0 until roomSlots.length){
	    if(roomSlots(i)._2.length != other.roomSlots(i)._2.length) temp = false
	    for(j <- 0 until roomSlots(i)._2.length)
	      if(roomSlots(i)._2(j).classID != other.roomSlots(i)._2(j).classID) 
	        temp = false
	  }
	  temp 
	}

	def changeSlot(r: Int, s: Int, cl : String) = {
	  for((roomID, sl) <- roomSlots if roomID == r; slot <- sl if slot.id == s ) slot.classID = cl
	}
	
	def Switch(r1: Int, s1 : Slot, r2 : Int, s2 : Slot) = {
		var temp = s2.classID
		changeSlot(r2, s2.id, s1.classID)
		changeSlot(r1, s1.id, temp)	
	}

	// TODO: lots of overlap between getConflicts and findProfConflicts
	// one method that gets passed in a function that either sums up the number of conflicts 
	// or returns their instances?
	def getConflicts : List[(Int, Slot)] = {
	  
	  def findProfConflictHelper(pID: String) : List[(Int, Slot)] = {
	    var t = List[(Int, Slot)]()
	   
	    val temp = for((r, a) <- roomSlots; 
	    			s <- a if (s.classID.size > 0 && // if ID is not null
	    					classes(s.classID).prof == pID)) // if it's the same prof
	    			  yield (r, s)
	    
	    // check whether a professor has multiple classes at the same time
	    // TODO: figure out a more elegant solution, 
	    // DP seems one possible approach though it might be an overkill
	    for( (r, Slot(i, time, classID)) <- temp)
	      temp.foreach(x => if(x._2.id == i && x._1 != r) t = x :: t)
	    
	    // check whether a professor has a class scheduled during a bad time
	    for((r, s) <- temp if(profs(pID).contains(s.id))) 
	      t = (r, s) :: t
	      
	    t 
	  }
	  
	  var i = List[(Int, Slot)]()
	  
	  profs.foreach(p => i = findProfConflictHelper(p._1) ++ i)
	  
	  i.distinct
	  
	}
	
	def getFreeSlots = for((r, a) <- roomSlots; s <- a if(s.classID.size == 0)) yield (r, s)
	
	// TODO: think of different metrics
	// 1. number of conflicts (1 input) - check
	// 2. average room preference (S) (1 input) - check
	// 3. room density (rooms.size inputs) - check
	// 4. number of prof collisions per slot (slots.size inputs)
	// 5. number of preference collisions per room (rooms.size inputs)
	def roomDensity : Array[Double] = {
	  var temp = new Array[Double](rooms.size)
	  var count = 0
	  for((roomId, sl) <- roomSlots){
	    var t = 0.0
	    sl.foreach(s => if(!s.classID.isEmpty()) t = t+1)
	    temp(count) = t/sl.size
	    count = count + 1
	  }
	  return temp
	}
	
	def averageRoomPreference : Double = {
	  var r = 0.0
	  
	  for(room <- rooms){
	    classes.foreach(cl => 
	      if(cl._2.prefs.contains(room.id))
	        r = r + 1)
	  }
	  r = r/rooms.size
	  r
	}
	
	def findConflics : Int = {
	  var c = 0
	  c = c + findProfConflicts
	  c = c + findRoomPrefConflicts
	  c
	}
	
	def findRoomPrefConflicts : Int = {
	  var c = 0
	  
	  for((r, a) <- roomSlots;
	      cl <- a if cl.classID != "";
	      pref <- classes(cl.classID).prefs if pref != r)
	  {
	    c = c + 1
	  }
	    
	  
	  c
	}
	
	def findProfConflicts : Int = {
	  def findProfConflictHelper(pID: String) : Int = {
	    var t = 0
	   
	    val temp = for((r, a) <- roomSlots; 
	    			s <- a if (s.classID.size > 0 && // if ID is not null
	    					classes(s.classID).prof == pID)) // if it's the same prof
	    			  yield (r, s)
	    
	    // check whether a professor has multiple classes at the same time
	    // TODO: figure out a more elegant solution, 
	    // DP seems one possible approach though it might be an overkill
	    for( (r, Slot(i, time, classID)) <- temp)
	      temp.foreach(s => if(s._2.id == i && s._1 != r) t += 1)
	    
	    // check whether a professor has a class scheduled during a bad time
	    for((r,s) <- temp if(profs(pID).contains(s.id))) 
	      t += 1
	      
	    t 
	  }
	  
	  var i = 0
	  
	  profs.foreach(p => i += findProfConflictHelper(p._1))
	  
	  i
	}
	
	def getRoomSlots = for(r <- rooms) yield (r.id, copySlots)
	
	def copyRoomSlots = {
	  var copyRoomSlots = new Array[(Int, Array[Slot])](roomSlots.length)
	  for(i <- 0 until roomSlots.length){
	    var arr = new Array[Slot](roomSlots(i)._2.length)
	    for(j <- 0 until roomSlots(i)._2.length)
	      arr(j) = roomSlots(i)._2(j).copy
	    copyRoomSlots(i) = (roomSlots(i)._1, arr)
	  }
	  copyRoomSlots
	}
	
	def copySlots = {
	 var copy = new Array[Slot](slots.size)
	 for(x <- 0 until slots.size)
	   copy(x) = slots(x).copy
	 copy
	}
	
	def putClass(cl: String, room: Int, slot: Int) = {
	  for(r <- roomSlots) r match {
	    case (id, sl) if id == room =>
	        sl.foreach(s => if(s.id == slot) {
	          s.classID = cl
	          println("Changing stuff")
	        })
	    case _ =>
	  }
	}
	
	def printRoomSlots = {
	  roomSlots.foreach(p => {
	    println(p._1) 
	    p._2.foreach(x => println(x))
	  })
	}
	
}