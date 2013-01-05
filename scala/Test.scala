package test

import LJava.{Constraint, MathFormulas, LJ, VariableMap, Variable, QueryParameter}
import collection.JavaConversions._

object Test {
  def mkConstraint(vs:Array[Variable], a:Int, b:Int):Constraint = {
    val l:List[java.lang.Object] = List(1.asInstanceOf[java.lang.Integer], vs(a) ,vs(b))
    new Constraint(MathFormulas.abs, l:_*)
  }

  def mkComp(c:Constraint, vs:Array[Variable], a:Int, b:Int):Constraint = new Constraint(c, LJ.OR, mkConstraint(vs,a,b))

  var p: Constraint = null
  var v: Array[LJava.Variable]= null
  var lz:LJava.Lazy =null

  def main (args: Array[String]) {
    val a:Array[java.lang.Integer] = Array(1,2,3,4,5,6,7,8)
    LJ.group(a:_*)
    val vss = LJ.varArray(8)
    val c:Constraint  = List((1,2),(1,4),(2,3),(2,5),(3,6),(4,5),(5,6),(5,7))
                         .foldLeft(mkConstraint(vss, 0 ,2)) {case (c, (a,b))=>mkComp(c, vss, a, b)}
    p = c
    v = vss
    val lz1 = LJ.lazy1(LJ.relation(v), LJ.DIFFER, p)
val yy =   lz1.next
    print (yy)
    lz = lz1
    println(lz.hasNext)
    while (lz.hasNext) {
      val l = lz.next
      val q = l.toArray(vss:_*).foreach((x)=> {
        val c = x.get(0).asInstanceOf[LJava.Association]
          println ("sss => " + c.ASSS)
        })

    }
    println("Hello from Scala!")
  }
}
