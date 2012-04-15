import scala.util.Random
import scala.runtime.RichChar 
class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  var n = 0
  val rnd = new Random()
  var direction = 0
  def respond(input: String) = {
    val (opcode, params) = CommandParser.apply(input)
    n += 1
    if( opcode == "React" ) {
            val view = params("view")
            val entity = params("entity")
            val viewObj = View(view)
            var dir = viewObj.safeDirections()
            // we assemble a response command and yield it back to the server
            if (dir.x == 0 && dir.y == 0) {
               if (direction == 0) {
                 dir = XY(0, 1)
               } else if (direction == 1) {
                dir = XY(1, 1) 
               } else if (direction == 2) {
                dir = XY(1, 0) 
               } else if (direction == 3) {
                 dir = XY(1, -1)
               } else if (direction == 4) {
                 dir = XY(0, -1)
               } else if (direction == 5) {
                 dir = XY(-1, -1)
               } else if (direction == 6) {
                dir = XY(-1, 0)
               } else if (direction == 7) {
                dir = XY(-1, 1)
               }
               //dir = safeMove(dir, viewObj)
            }
            dir = safeMove(XY(dir.x, dir.y), viewObj)
            "Move(dx="+dir.x+",dy="+dir.y+")"
    } else if ( opcode == "Goodbye") {
        println(params("energy"))
        ""
    }
  }
  def safeMove(d: XY, view: View) : XY = {
    var myXY = d.fix()
    val cell = view.cellAtRelPos(List(myXY.x, myXY.y))
    if (cell == "W" || cell == "p" || cell == "s" || cell == "m" || cell == "b") {
        direction = (direction + 1) % 8
        myXY = XY(0,0)
    }
    myXY
  }
}
case class View(cells: String) {
    val rnd = new Random()
    val size = math.sqrt(cells.length).toInt
    val center = List(size/2, size/2)
    def indexFromAbsPos(absPos: List[Int]) = absPos(0) + absPos(1) * size
    def absPosFromRelPos(relPos: List[Int]) = List(relPos(0) + center(0), relPos(1) + center(1))
    def indexFromRelPos(relPos: List[Int]) = indexFromAbsPos(absPosFromRelPos(relPos))
    def cellAtRelPos(relPos: List[Int]) : (String) = cells.charAt(indexFromRelPos(relPos)).toString()
    def safeDirections() : XY = {
        var foods  = List[Int]()
        var seen = false
        var closest = 1000
        var output = XY(0,0)
        for (i <- -15 until 15) {
            for (j <- -15 until 15) {
                val cell = cellAtRelPos(List(i, j))
                if (cell == "P" || cell == "B") {
                    val dist = Math.abs(i) + Math.abs(j)
                    val dirXY = XY(i,j).fix()
                    val cell = cellAtRelPos(List(dirXY.x, dirXY.y))
                    if (dist < closest && safePath(XY(i, j))) {
                        closest = dist
                        seen = true
                        output = XY(i, j)
                    }    
                }
            }
        }
        output
    }
    def safePath(xy: XY) : Boolean = {
        var safe = true
        val path = pathTo(xy)
        for (i <- 0 until path.length) {
            val cell = cellAtRelPos(List(path(i).x, path(i).y))
            if (cell == "W" || cell == "p" || cell == "s" || cell == "m" || cell == "b") {
                safe = false
            }
        }
        safe
    }
    def pathTo(xy: XY) : List[XY] = {
        val fixedXY = xy.fix()
        var output = List[XY]()
        for (i <- 1 until math.abs(math.max(math.abs(xy.x), math.abs(xy.y)))) {
            var x = 0
            var y = 0
            if (math.abs(xy.x) >= i) {
                x = i * fixedXY.x
            } else {
                x = xy.x
            }    
            if (math.abs(xy.y) >= i) {
                y = i * fixedXY.y
            } else {
                y = xy.y
            }
            output ::= XY(x, y)
        }
        output
    }
    def printView() = {
        println(cellAtRelPos(List(-1,-1))+cellAtRelPos(List(0,-1))+cellAtRelPos(List(1,-1)))
        println(cellAtRelPos(List(-1,0))+cellAtRelPos(List(0,0))+cellAtRelPos(List(1,0)))
        println(cellAtRelPos(List(-1,1))+cellAtRelPos(List(0,1))+cellAtRelPos(List(1,1)))
    }
}

/** Utility methods for parsing strings containing a single command of the format
 *  "Command(key=value,key=value,...)"
 */
object CommandParser {
    /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
    def apply(command: String) : (String,Map[String,String]) = {
        /** "key=value" => ("key","value") */
        def splitParameterIntoKeyValue(param: String) : (String,String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("parameter not a valid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)
        val opcode = segments(0)
        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParameterIntoKeyValue ).toMap
        (opcode,keyValuePairs)
    }
}

case class XY(x: Int, y: Int) {
    def fix () = {
        var newX = x
        var newY = y
        if (x > 1) {
            newX = 1
        }    
        if (x < -1) {
            newX = -1
        }    
        if (y < -1) {
            newY = -1
        }    
        if (y > 1) {
            newY = 1
        }    
        XY(newX, newY)
    }
    def reverse () = {
        XY(-x, -y)
    }
}    
object XY {
    def randomUnit(rnd: Random) = XY(rnd.nextInt(3) - 1,rnd.nextInt(3) - 1) 
}
