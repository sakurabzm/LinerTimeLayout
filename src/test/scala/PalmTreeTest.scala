import scala.collection.mutable.{ListBuffer, Stack}
import scala.math._

object PalmTreeTest extends App{

  var i = 0
  sealed trait TreeType
  case object Arc extends TreeType
  case object Frond extends TreeType

  sealed  trait CompType
  case object Bond extends CompType
  case object Polygon extends CompType
  case object Triconnected extends CompType

//  case class Edge(v: Vertex, w: Vertex, tt: TreeType){
//    var parent: Vertex = v
//    var treeArc: Vertex = w
//    var treeType : TreeType = tt
//
//
//  }

  //init
  type Edge = (Vertex, Vertex)
  type Component = ListBuffer[Edge]
  type VirtualEdge = (Vertex, Vertex, Component, CompType)
  val graphc = new Graph
  var length = 13
  lazy val root: Vertex = verticeslist.head
  var verticeslist = new ListBuffer[Vertex]()
  var bucket = new Array[ListBuffer[Edge]](3*length + 2)
  var palmtree = new ListBuffer[Edge]()
  var startPath = new ListBuffer[(Int, Int)]()
  var tStack = Stack[(Int, Vertex, Vertex)]()
  var eStack = Stack[Edge]()
  val eos = (0, new Vertex(""), new Vertex(""))
  var virtualEdgeList = new ListBuffer[VirtualEdge]
  var newTreeEdgeList = new ListBuffer[Edge]
  var triComponents = new ListBuffer[Component]


  for(i <- bucket.indices){
    bucket(i) = new ListBuffer[(Vertex, Vertex)]()
  }

  def createGraph(): Unit ={
    var v1 = new Vertex("v1")
    var v2 = new Vertex("v2")
    var v3 = new Vertex("v3")
    var v4 = new Vertex("v4")
    var v5 = new Vertex("v5")
    var v6 = new Vertex("v6")
    var v7 = new Vertex("v7")
    var v8 = new Vertex("v8")
    var v9 = new Vertex("v9")
    var v10 = new Vertex("v10")
    var v11 = new Vertex("v11")
    var v12 = new Vertex("v12")
    var v13 = new Vertex("v13")

    verticeslist += v1
    verticeslist += v2
    verticeslist += v3
    verticeslist += v4
    verticeslist += v5
    verticeslist += v6
    verticeslist += v7
    verticeslist += v8
    verticeslist += v9
    verticeslist += v10
    verticeslist += v11
    verticeslist += v12
    verticeslist += v13

    v1.transition += v2
    v1.transition += v4
    v1.transition += v8
    v1.transition += v12
    v1.transition += v13

    v2.transition += v1
    v2.transition += v3
    v2.transition += v13

    v3.transition += v2
    v3.transition += v4
    v3.transition += v13

    v4.transition += v1
    v4.transition += v3
    v4.transition += v5
    v4.transition += v6
    v4.transition += v7

    v5.transition += v4
    v5.transition += v6
    v5.transition += v7
    v5.transition += v8

    v6.transition += v4
    v6.transition += v5
    v6.transition += v7

    v7.transition += v4
    v7.transition += v5
    v7.transition += v6

    v8.transition += v1
    v8.transition += v5
    v8.transition += v9
    v8.transition += v11
    v8.transition += v12

    v9.transition += v8
    v9.transition += v10
    v9.transition += v11
    v9.transition += v12

    v10.transition += v9
    v10.transition += v11
    v10.transition += v12

    v11.transition += v8
    v11.transition += v9
    v11.transition += v10

    v12.transition += v1
    v12.transition += v8
    v12.transition += v9
    v12.transition += v10

    v13.transition += v1
    v13.transition += v2
    v13.transition += v3
  }

  def createGraph2(): Unit ={

    var s = new Vertex("s")
    var v1 = new Vertex("v1")
    var v2 = new Vertex("v2")
    var v3 = new Vertex("v3")
    var v4 = new Vertex("v4")
    var v5 = new Vertex("v5")
    var v6 = new Vertex("v6")
    var v7 = new Vertex("v7")
    var t = new Vertex("t")

    verticeslist += s
    verticeslist += v1
    verticeslist += v2
    verticeslist += v3
    verticeslist += v4
    verticeslist += v5
    verticeslist += v6
    verticeslist += v7
    verticeslist += t


    s.transition += v1
    s.transition += v2
    s.transition += t


    v1.transition += s
    v1.transition += v3
    v1.transition += v4
    v1.transition += v5

    v2.transition += s
    v2.transition += v3
    v2.transition += v4
    v2.transition += v5

    v3.transition += v1
    v3.transition += v2
    v3.transition += v4

    v4.transition += v1
    v4.transition += v2
    v4.transition += v3

    v5.transition += v1
    v5.transition += v2
    v5.transition += v6
    v5.transition += v7

    v6.transition += v5
    v6.transition += v7

    v7.transition += v5
    v7.transition += v6
    v7.transition += t

    t.transition += v7
    t.transition += s

//    eStack.push((s, t))

  }

  def createGraph3(): Unit ={
    var s = new Vertex("s")
    var u = new Vertex("u")
    var v = new Vertex("v")
    var w = new Vertex("w")
    var x = new Vertex("x")
    var y = new Vertex("y")
    var z = new Vertex("z")
    var t = new Vertex("t")

    verticeslist += s
    verticeslist += u
    verticeslist += v
    verticeslist += w
    verticeslist += x
    verticeslist += y
    verticeslist += z
    verticeslist += t

    s.transition += u
    s.transition += t
    u.transition += v
    u.transition += w
    v.transition += u
    v.transition += w
    v.transition += x
    w.transition += u
    w.transition += v
    w.transition += x
    x.transition += v
    x.transition += w
    x.transition += y
    y.transition += x
    y.transition += z
    z.transition += y
    z.transition += t
    t.transition += z
    t.transition += s

  }


  case class Graph(){
    var vertices = new ListBuffer[Vertex]()
    var edges = new ListBuffer[Edge]()

    def initGraph(v: ListBuffer[Vertex], e: ListBuffer[Edge]): Unit ={
      vertices = v.clone()
      edges = e.clone()
    }

    def removeEdge(e: Edge): Unit ={
      edges -= e
    }
  }


  case class Vertex(n: String){
    def uuid = java.util.UUID.randomUUID().hashCode()
    val name = n
    var num, lowpt1, lowpt2, nd, father = 0
    var flag: Boolean = true
    var visited: Boolean = false
    var transition = new ListBuffer[Vertex]()
    var adjList = new ListBuffer[Vertex]()
//    var d = new ListBuffer[Vertex]()
    var high = new ListBuffer[Int]()
    var deg = 0

    def copy(v: Vertex): Unit ={
      num = v.num
      lowpt1 = v.lowpt1
      lowpt2 = v.lowpt2
      high = v.high
      nd = v.nd
      father = v.father
      flag = v.flag
      visited = v.visited
      transition = v.transition
      adjList = v.adjList
//      d = v.d
      deg = v.deg
    }
    def degree(): Int ={
      return max(0,transition.length + deg)
    }

    def highv(): Int ={
      if(high.nonEmpty)
        return high.head
      else{
        return 0
      }
    }

    def firstChild(): Int ={
      if(adjList.nonEmpty)
        return adjList.head.num
      else{
        return 0
      }
    }

    def addTransition(v: Vertex): Unit ={
      if(!transition.contains(v))
        transition += v
    }
    def removeTransition(v: Vertex): Unit ={
      transition -= v
    }

    def removeAdjacent(v: Vertex): Unit ={
      adjList -= v
    }

    def removeHigh(vn: Int): Unit ={
      high -= vn
    }

    override def equals(o: Any) = o match {
      case that: Vertex => that.hashCode == this.hashCode
      case _ => false
    }
    override def hashCode = uuid
    override def toString: String = {
      "num = " + num + " father = " + father + " nd = " + nd + " lowpt1 = " + lowpt1 + " lowpt2 = " + lowpt2
    }
  }

  def search1(v: Vertex, u: Vertex): Unit ={
    i += 1
    v.nd = 0
    v.num = i
    v.lowpt1 = i
    v.lowpt2 = i
    for (w <- v.transition){
      if(w.num == 0){
        palmtree += ((v, w))
        v.nd += 1
        w.father = v.num
//        v.d += w
        search1(w, v)
        v.nd += w.nd
//        v.d ++= w.d

        if(v.lowpt1 > w.lowpt1){
          v.lowpt2 = min(v.lowpt1, w.lowpt2)
          v.lowpt1 = w.lowpt1
        }else if(v.lowpt1 == w.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.lowpt2)
        }else{
          v.lowpt2 = min(v.lowpt2, w.lowpt1)
        }

      }else if((v.num > w.num) && ((w.num != u.num) || (v.flag == false))){

        palmtree += ((v, w))
//        println("check: father: " + v.num + " son: " + w.num)
        if(w.num < v.lowpt1){
          v.lowpt2 = v.lowpt1
          v.lowpt1 = w.num
        }else if(w.num > v.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.num)
        }

      }else if(w.num == u.num && v.flag == true){
//        println("check: father v: " + v.num + " son w: " + w.num)
        v.flag = false
      }
    }
  }

  def sort(): Unit = {
    for ((v,w) <- palmtree) {
      if (v.num > w.num) {
        bucket((3 * w.num) + 1) += ((v, w))
        w.high += v.num
      }else if(v.num > w.lowpt2){
        bucket(3 * w.lowpt1) += ((v, w))
      }else{
        bucket(3 * w.lowpt1 + 2) += ((v, w))
      }
    }
    for(i <- bucket.indices){
      if(bucket(i).length > 0){
        for((v, w) <- bucket(i)){
          v.adjList += w
        }
      }
    }
  }

  var start = true
  def startPath(v: Vertex): Unit ={

    for(w <- v.adjList){
      if(v.num < w.num){
        if(start){
          startPath += ((v.num, w.num))
          start = false
        }
        startPath(w)
      }else{
        if(start){
          startPath += ((v.num, w.num))
        }else{
          start = true
        }
      }
    }
  }

  def pathSearch(v: Vertex): Unit ={
    var h = 0
    var a = new Vertex("a")
    var b = new Vertex("b")
    v.visited = true
    for (w <- v.adjList){
      h = tStack.top._1
      a = tStack.top._2
      b = tStack.top._3

      if(v.num < w.num) {
        if (startPath.contains((v.num, w.num))) {
          var triDel = false
          var deltri = Stack[(Int, Vertex, Vertex)]()
          while (a.num > w.lowpt1) {
            deltri.push(tStack.pop())
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3
            triDel = true
          }
          if (!triDel) {
            tStack.push((w.num + w.nd - 1, verticeslist.find(p => p.num == w.lowpt1).get, v))
          }else{
            val bb = deltri.top._3
            val y = deltri.sortBy(_._1).last._1
//            deltri.foreach(f => if (y < f._1) y = f._1)
            tStack.push((max(y, w.num + w.nd - 1), verticeslist.find(p => p.num == w.lowpt1).get, bb))
          }
          tStack.push(eos)
        }

        pathSearch(w)
        eStack.push((v, w))
        /*
        * check type2 pairs
        */
        h = tStack.top._1
        a = tStack.top._2
        b = tStack.top._3
        var wcopy = w
        while(v.num != 1 && ((a.num == v.num) || (wcopy.degree() == 2 && wcopy.firstChild() > wcopy.num))){

          if(a.num == v.num && b.father == a.num){
            tStack.pop()
          }else{
            var eab: Edge = null
            var ee: Edge = null
            if(wcopy.degree() == 2 && wcopy.firstChild() > wcopy.num){
              println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
              var c = new Component
              c += newComponent(eStack.pop())
              val x = eStack.top._2
              println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
              c += newComponent(eStack.pop())
              ee = ((v, x))
              newVirtualEdge(v, x, c)
              if(getEstackx() == v.num && getEstacky() == b.num){
                eab = eStack.pop()
              }
            }else{
              tStack.pop()
              var c = new Component
              var x = getEstackx()
              var y = getEstacky()
              while((x <= h && x >= a.num) && (y <= h && y >= a.num)){
                if(x == a.num && y == b.num){
                  println("debug type2...2... " + getEstackxn() + " -> " + getEstackyn())
                  eab = eStack.pop()
                  x = getEstackx()
                  y = getEstacky()
                }else{
                  println("debug type2...3... " + getEstackxn() + " -> " + getEstackyn())
                  c += newComponent(eStack.pop())
                  x = getEstackx()
                  y = getEstacky()
                }
              }
              ee = (a, b)
              newVirtualEdge(a, b, c)
            }
            if(eab != null){
              var c = new Component
              c += newComponent(eab)
              println("debug type2...4... " + eab._1.name + " -> " + eab._2.name)
              c += newComponent(((ee._1, ee._2)))
              println("debug type2...4... " + ee._1.name + " -> " + ee._2.name)
              ee = (v, b)
              newVirtualEdge(v, b, c)
            }
            eStack.push((ee._1, ee._2))
            makeTreeEdge((v, b))
            println("type2 " + v.name + " -> " + b.name)
            wcopy = b
          }
          h = tStack.top._1
          a = tStack.top._2
          b = tStack.top._3
        }
        /*
        * type2 finished
        */

        /*
        * check for a type-1 pair
        */
        var notvisited = false
        v.adjList.foreach(f => if(!f.visited) notvisited = true)
        if(w.lowpt2 >= v.num && w.lowpt1 < v.num && (v.father != 1 || notvisited)){
          var c = new Component
          var x = getEstackx()
          var y = getEstacky()
          while ((x >= w.num && x <= w.num + w.nd) || (y >= w.num && y <= w.num + w.nd)){
            println("debug type1...1... " + getEstackxn() + " -> " + getEstackyn())
            c += newComponent(eStack.pop())
            x = getEstackx()
            y = getEstacky()
          }

          var lowpt1w = verticeslist.find(p => p.num == w.lowpt1).get
          var ee: Edge = (v, lowpt1w)
          newVirtualEdge(v, lowpt1w, c)

          if(x == v.num && y == w.lowpt1){
            var c = new Component
            println("debug type1...2... " + getEstackxn() + " -> " + getEstackyn())
            c += newComponent(eStack.pop())
            println("debug type1...3... " + ee._1.name + " -> " + ee._2.name)
            c += newComponent((ee._1, ee._2))
            ee = (v, lowpt1w)
            newVirtualEdge(v, lowpt1w, c)
          }

          if(w.lowpt1 != v.father){
            eStack.push((ee._1, ee._2))
            makeTreeEdge((lowpt1w, v))
            println("type1.1 " + lowpt1w.name + " -> " + v.name)
          }else{
            var c = new Component
            println("debug type1...4... " + lowpt1w.name + " -> " + v.name)
            c += newComponent((lowpt1w, v))
            ee = (lowpt1w, v)
            newVirtualEdge(lowpt1w, v, c)
            makeTreeEdge((lowpt1w, v))
            println("type1.2 " + lowpt1w.name + " -> " + v.name)
          }
        }

        /*
        * type1 finished
        */

        if (startPath.contains((v.num, w.num))) {
          while (!tStack.top.equals(eos)) {
            tStack.pop()
          }
          tStack.pop()
        }
        h = tStack.top._1
        a = tStack.top._2
        b = tStack.top._3
        while (!tStack.top.equals(eos) && ((a.num != v.num) && (b.num != v.num) && (v.highv() > h))) {
          tStack.pop()
          h = tStack.top._1
          a = tStack.top._2
          b = tStack.top._3
        }
      }else{
        val e = (w, v)
        if(startPath.contains((v.num, w.num))){
          var triDel = false
          var deltri = Stack[(Int, Vertex, Vertex)]()
          while(a.num > w.num){
            deltri.push(tStack.pop())
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3
            triDel = true
          }
          if(triDel == false){
            tStack.push((v.num, w, v))
          }else{
            val bb = deltri.top._3
            val y = deltri.sortBy(_._1).last._1
            tStack.push((y, w, bb))
          }
        }
        if(w.num == v.father){
          var c = new Component
          println("debug frond..." + e._1.name + " -> " + e._2.name)
          c += newComponent(e)
          println("debug frond..." + w.name + " -> " + v.name)
          c += newComponent((w, v))
          val ee: Edge = (w, v)
          newVirtualEdge(w, v, c)
          makeTreeEdge(ee)
          println("frond " + w.name + " -> " + v.name)
        }else{
          eStack.push(e)
        }
      }
    }
  }

  def getEstackx(): Int ={
    if(eStack.isEmpty){
      return 0
    }else{
      return eStack.top._1.num
    }
  }

  def getEstackxn(): String ={
    if(eStack.isEmpty){
      return "empty"
    }else{
      return eStack.top._1.name
    }
  }

  def getEstacky(): Int ={
    if(eStack.isEmpty){
      return 0
    }else{
      return eStack.top._2.num
    }
  }

  def getEstackyn(): String ={
    if(eStack.isEmpty){
      return "empty"
    }else{
      return eStack.top._2.name
    }
  }

  def newComponent(e: Edge): Edge ={

    graphc.removeEdge(e)
    e._1.removeAdjacent(e._2)
    e._1.removeTransition(e._2)
    e._1.removeHigh(e._2.num)
    e._2.removeTransition(e._1)

    return e
  }


  def newVirtualEdge(v: Vertex, w: Vertex, c: Component): Unit ={

    w.father = v.num
    c += ((v,w))
    var e: VirtualEdge = null
    if(c.length > 3){
      e = (v, w, c, Triconnected)
    }else{
      var vn = 0
      var v1 = c.head._1.num
      for(i <- c){
        if(i._1.num == v1 || i._2.num == v1){
          vn += 1
        }
      }
      if(vn < 3){
        e = (v, w, c, Polygon)
      }else{
        e = (v, w, c, Bond)
      }
    }

    virtualEdgeList += e

//    v.deg += 1
//    w.deg += 1
    if(!v.transition.contains(w)){
      v.addTransition(w)
    }
    if(!w.transition.contains(v)){
      w.addTransition(v)
    }
    if(!v.adjList.contains(w)){
      var bn = 0
      if(w.num < v.num){
        bn = 3 * w.num + 1
        v.high += w.num
      }else if(w.lowpt2 < v.num){
        bn = 3 * w.lowpt1
      }else{
        bn = 3 * w.lowpt1 + 2
      }

      for(i <- v.adjList.indices){
        val wo = v.adjList(i)
        if(wo.num < v.num){
          if(bn < (3 * wo.num + 1)){
            v.adjList.insert(i, w)
            return
          }else if(wo.lowpt2 < v.num){
            if(bn < (3 * wo.lowpt1)){
              v.adjList.insert(i, w)
              return
            }
          }else{
            if(bn < (3 * wo.lowpt1 + 2)){
              v.adjList.insert(i, w)
              return
            }
          }
        }
      }
      v.adjList += w
    }
  }

  def makeTreeEdge(e : Edge): Unit ={
    palmtree += e
    newTreeEdgeList += e
  }

  /*
  * TODO. need to change to linear time
  */
  def buildTriComp(): Unit ={
    var lastComponent = new Component
    while(eStack.nonEmpty){
      lastComponent += eStack.pop()
    }


    for(i <- virtualEdgeList.indices){
      val vi = virtualEdgeList(i)
      if(vi._3.nonEmpty){
        if((vi._4.equals(Bond) || vi._4.equals(Polygon))){
          for(e <- vi._3){
            for(j <- i+1 to virtualEdgeList.length-1){
              val vj = virtualEdgeList(j)
              if(vj._3.contains(e) && vi._4.equals(vj._4)){
                vi._3 ++= vj._3
                vi._3 -= e
                vi._3 -= e
                vj._3.clear()
              }
            }
          }
          triComponents += vi._3
        }else{
          triComponents += vi._3
        }
      }
    }
    triComponents += lastComponent
  }

  var superroot = new Vertex("superroot")
  superroot.num = -1

  createGraph2
  search1(verticeslist.head, superroot)
  sort
  startPath(verticeslist.head)
  graphc.initGraph(verticeslist, palmtree)
  tStack.push(eos)
  pathSearch(verticeslist.head)
  buildTriComp()


//  println("--------transions----------")
//  verticeslist.foreach(f => {
//    println("v: " + f.num)
//    println(f.transition.foreach(g => {
//      print(g.num + "   ")
//    }))
//    println
//  })
//  println("--------------------------------------")


//  println("--------components----------")
//  virtualEdgeList.foreach(f => {
//    println(s"v: ${f._1.name} -> w: ${f._2.name}")
//    println("componets:")
//    f._3.foreach(g => {
//      println(s"v: ${g._1.name} -> w: ${g._2.name}")
//    })
//    println("*************************")
//  })

  println("--------Tri Components----------")
  triComponents.foreach(f => {
    println("componets:")
    f.foreach(g => {
      println(s"v: ${g._1.name} -> w: ${g._2.name}")
    })
    println("------------------------------------")
  })
  println("**********************************************")


//  println(palmtree.foreach(f => println(f._1.num + "  " +f._2.num)))
//  for(v <- vertexlist){
//    println(v)
//    print("adj: " )
//    v.adjList.foreach(f => print("  " + f.num))
//    println()
//    println("-------------------------")
//  }


//  for(edge <- palmtree.sortBy(_._1.num).reverse){
//    println(edge._1.num + " -> " + edge._2.num)
//  }
//
//    val c = new Component()
//    while (!eStack.isEmpty){
//      c += eStack.pop()
//    }
//    c.foreach(f => {
//      print(f._1.num + " --> " + f._2.num)
//      println
//    })
//
//  println("--------------------------------------")
//
//  virtualEdgeList.foreach(f => {
//    print(f._1.num + " -> " +   f._2.num)
//    println()
//  })

//  println("--------------------------------------")
//
//
//  verticeslist.foreach(f => {
//    println("num: " + f.num + " transition: " + f.transition.length)
//    f.transition.foreach(g => {
//      println("tran: " + g.num)
//    })
//    println("*************")
//  })
//
//  newTreeEdgeList.foreach(f => {
//    print(f._1.num + " -> " +   f._2.num)
//    println()
//  })



}
