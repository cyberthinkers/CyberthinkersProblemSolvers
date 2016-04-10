package temporarytests
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

import org.cyberthinkers.problemsolvers.cassowary._

//case class Point(x: Int, y: Int){

//  def +(p: Point) = Point(x + p.x, y + p.y)
//  def /(d: Int) = Point(x / d, y / d)
//}
// this configuration is only for initial develop of the Cassowary Solver in JavaScript

@JSExport
object ScalaJSExample {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    //    val ctx = canvas.getContext("2d")
    //                    .asInstanceOf[dom.CanvasRenderingContext2D]
    //
    //    var count = 0
    //    var p = Point(0, 0)
    //    val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))
    //
    //    def clear() = {
    //      ctx.fillStyle = "black"
    //      ctx.fillRect(0, 0, 255, 255)
    //    }
    //
    //    def run = for (i <- 0 until 10){
    //      if (count % 3000 == 0) clear()
    //      count += 1
    //      p = (p + corners(Random.nextInt(3))) / 2
    //
    //      val height = 512.0 / (255 + p.y)
    //      val r = (p.x * height).toInt
    //      val g = ((255-p.x) * height).toInt
    //      val b = p.y
    //      ctx.fillStyle = s"rgb($g, $r, $b)"
    //
    //      ctx.fillRect(p.x, p.y, 1, 1)
    //    }
    //
    //    dom.setInterval(() => run, 50)

    //    val v1 = Variable(123)
    //    val v2 = Variable(456)
    //    val m = Map(v1 -> 1, v2 -> 2)
    //    val k = m.keySet
    //    val v = m(v1)
    //    println("m = " + m);
    //    println("k = " + k);
    //    println("v = " + v);
    val m: Map[AbstractVariable, Set[AbstractVariable]] =
      Map(Variable(1, 1) -> Set(Variable(2, 3)),
          Variable(2, 3) -> Set(Variable(4, 5)))
    println("m = " + m)
    val v = Variable(2, 3)
    val s = Set(v)
//    val t = new Tableau(m)
//    val varset = t.columns.get(Variable(1, 1)).get
//    println("varset = " + varset)
//    val c = varset collect t.columns
//    println("c = " + c)
  }
}