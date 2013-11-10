package reinforcement

import parser._
import java.util.Random

trait predictionFunction {
  def predict(x : Array[Double]) : Double
  def update(x : Array[Double], newX : Array[Double], newY : Double)
}

class SimpleP(n : Int) extends predictionFunction {
  var alpha = 0.1
  var gamma = 0.1 
  var v = Array.fill(n)(1.0)
  
  def predict(x : Array[Double]) : Double = {
    var i = 0.0
    v.zip(x).foreach(t => i = i + t._1*t._2)
    i
  }
  
  def update(x : Array[Double], newX : Array[Double], newY : Double) = {
    var newV = new Array[Double](n)
    var sum = v.sum
    for(i <- 0 until n){
      newV(i) = v(i) + alpha*(newY + gamma*predict(newX) + predict(x))*x(i)
      newV(i) = newV(i)/sum
      println("Old v = " + v(i) + "; New v = " + newV(i) + "; NewY = " + newY + "; predict(x) = " + predict(x))
    }
     // TODO: try the q-learning eq from p. 36 of Zhang's paper
    
    v = newV
  }
}

class TDRL(var schedule : Schedule) {
    var history = Vector[Schedule]()
    val slots = schedule.slots
    val rooms = schedule.rooms
	val profs = schedule.profs
	val classes = schedule.classes
    
    def updateHistory(s : Schedule) {
      var temp = new Schedule(slots, rooms, profs, classes)
      temp.roomSlots = s.copyRoomSlots
      history = history :+ temp
    }
    
    def checkHistory(s : Schedule, old : Schedule) : Boolean = {
      //var size = 2
      var ret = true;
      for(i <- 0 until history.size){
      
    	 if(s.roomSlotsEquals(history(i)) &&
    	      (i > 0 && old.roomSlotsEquals(history(i-1))))
        //if(s.roomSlotsEquals(history(i)))
    		  ret = false
      }  
      ret
    }
    
	def reinforcementLearning = {
    
	    // Simple reinforcement
	    // TODO: Think what happens when there are unsolvable conflicts
	    // ex. a prof has as many classes as there are slots 
	    // and wants to not work during a slot
    	var epsilon = 0.2
		val rand = new Random(System.currentTimeMillis());
	    
    	var n = 2 + schedule.rooms.size
    	var p = new SimpleP(n)
	    
    	var x = 1.0
	    var y = 0.0
	    var epoch = 0
	    var arr = new Array[Double](n)
	    var oldArr = new Array[Double](n)
	    
	    while(x > 0 && epoch < 100){
	      // choose action ( max as evaluated by prediction function? ) ( try stochastic )
	      // and get next state
	      var best = new Schedule(slots, rooms, profs, classes)
	      var min = 0.0 // find a better starting value
	      var begin = true
	      // TODO: use a fold?
	      oldArr = arr
	      var nextStates = schedule.getPossibleNextStates
	      println(nextStates.size)
	      
	      var r = rand.nextDouble()
	      
	      if(r <= epsilon){
	        var i = rand.nextInt(nextStates.size)
	        arr(0) = nextStates(i).findConflics
	        arr(1) = nextStates(i).averageRoomPreference
	        var rDensity = nextStates(i).roomDensity
	        for(t <- 2 until best.rooms.size+2)
	          arr(t) = rDensity(t-2)
	          
	        best.roomSlots = nextStates(i).copyRoomSlots
	      }
	      else {
			      nextStates.foreach(sch => {
			    	  	arr(0) = sch.findConflics
			    	  	arr(1) = sch.averageRoomPreference
			    	  	var rDensity = sch.roomDensity
				        for(t <- 2 until best.rooms.size+2)
				          arr(t) = rDensity(t-2)
		    	  		if(begin){
		    	  		  min = p.predict(arr)
		        		  best.roomSlots = sch.copyRoomSlots
		        		  begin = false
		    	  		}
		    	  		else if(min > p.predict(arr) && checkHistory(sch, best)) {
		        		  min = p.predict(arr)
		        		  best.roomSlots = sch.copyRoomSlots
		        		}})
		  }
	      println("===========")
	      println("Epoch " + epoch)
	      schedule.roomSlots = best.copyRoomSlots
	      updateHistory(schedule)
	      // update prediction function
	      x = schedule.findConflics
	      println("Conflicts " + x)
	      y = -x
	        
	      p.update(oldArr, arr, y)
	      // if it reaches a state of no conflicts, stop
	      for(i <- p.v)
	    	  println(i)
	    	  
	      epoch = epoch + 1
	    }
	    
	    println("final schedule")
	    schedule.printRoomSlots
	    
	    for(i <- p.v)
	      println(i)
	    
	}
	
	
  
}