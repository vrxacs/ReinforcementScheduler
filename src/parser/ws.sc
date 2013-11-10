package parser

import scala.xml._
import reinforcement._
import java.util.Random
    
object ws {

		var p = new SimpleP(1)            //> p  : reinforcement.SimpleP = reinforcement.SimpleP@2dec8909
		
    var arr = Array[Double](1)                    //> arr  : Array[Double] = Array(1.0)
    var newArr = Array[Double](1)                 //> newArr  : Array[Double] = Array(1.0)
    arr(0) = 1
    p.predict(arr)                                //> res0: Double = 1.0
    
    var arr2 = new Array[Double](2)               //> arr2  : Array[Double] = Array(0.0, 0.0)
    
		val rand = new Random(System.currentTimeMillis())
                                                  //> rand  : java.util.Random = java.util.Random@5d5bdc50
		rand.nextDouble()                 //> res1: Double = 0.5453219748945015
		
    
}