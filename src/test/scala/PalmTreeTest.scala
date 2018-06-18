import scala.collection.mutable.{ListBuffer, Stack}
import scala.math._

object PalmTreeTest extends App{

  var i = 0
  sealed trait TreeType
  case object Arc extends TreeType
  case object Frond extends TreeType
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
  type VirtualEdge = (Vertex, Vertex, Component)
  var length = 13
  lazy val root: Vertex = verticeslist.head
  var verticeslist = new ListBuffer[Vertex]()
  var bucket = new Array[ListBuffer[Edge]](3*length + 2)
  var palmtree = new ListBuffer[Edge]()
  var startPath = new ListBuffer[(Int, Int)]()
  var tStack = Stack[(Int, Vertex, Vertex)]()
  var eStack = Stack[Edge]()
  val eos = (0, new Vertex, new Vertex)
  val graphc = new Graph
  var virtualEdgeList = new ListBuffer[VirtualEdge]
  var newTreeEdgeList = new ListBuffer[Edge]

  for(i <- bucket.indices){
    bucket(i) = new ListBuffer[(Vertex, Vertex)]()
  }

  var v1 = new Vertex()
  var v2 = new Vertex()
  var v3 = new Vertex()
  var v4 = new Vertex()
  var v5 = new Vertex()
  var v6 = new Vertex()
  var v7 = new Vertex()
  var v8 = new Vertex()
  var v9 = new Vertex()
  var v10 = new Vertex()
  var v11 = new Vertex()
  var v12 = new Vertex()
  var v13 = new Vertex()

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


  case class Vertex(){
    var num, lowpt1, lowpt2, nd, father = 0
    var flag: Boolean = true
    var visited: Boolean = false
    var transition = new ListBuffer[Vertex]()
    var adjList = new ListBuffer[Vertex]()
    var d = new ListBuffer[Vertex]()
    var high = new ListBuffer[Int]()

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
      d = v.d
    }
    def degree(): Int ={
      return transition.length
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
        v.d += w
        search1(w, v)
        v.nd += w.nd
        v.d ++= w.d

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
    var a = new Vertex()
    var b = new Vertex()
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
              println("debug type2...1... " + eStack.top._1.num + " -> " + eStack.top._2.num)
              var c = new Component
              c += newComponent(eStack.pop())
              val x = eStack.top._2
              println("debug type2...1... " + eStack.top._1.num + " -> " + eStack.top._2.num)
              c += newComponent(eStack.pop())
              ee = ((v, x))
              newVirtualEdge(v, x, c)
              if(eStack.top._1.num == v.num && eStack.top._2.num == b.num){
                eab = eStack.pop()
              }
            }else{
              tStack.pop()
              var c = new Component
              var x = eStack.top._1.num
              var y = eStack.top._2.num
              while((x <= h && x >= a.num) && (y <= h && y >= a.num)){
                if(x == a.num && y == b.num){
                  println("debug type2...2... " + eStack.top._1.num + " -> " + eStack.top._2.num)
                  eab = eStack.pop()
                  x = eStack.top._1.num
                  y = eStack.top._2.num
                }else{
                  println("debug type2...3... " + eStack.top._1.num + " -> " + eStack.top._2.num)
                  c += newComponent(eStack.pop())
                  x = eStack.top._1.num
                  y = eStack.top._2.num
                }
              }
              ee = (a, b)
              newVirtualEdge(a, b, c)
            }
            if(eab != null){
              var c = new Component
              c += newComponent(eab)
              println("debug type2...4... " + eab._1.num + " -> " + eab._2.num)
              c += newComponent(((ee._1, ee._2)))
              println("debug type2...4... " + ee._1.num + " -> " + ee._2.num)
              ee = (v, b)
              newVirtualEdge(v, b, c)
            }
            eStack.push((ee._1, ee._2))
            makeTreeEdge((v, b))
            println("type2 " + v.num + " -> " + b.num)
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
        if(wcopy.lowpt2 >= v.num && wcopy.lowpt1 < v.num && (v.father != 1 || notvisited)){
          var c = new Component
          var x = eStack.top._1.num
          var y = eStack.top._2.num
          while ((x >= wcopy.num && x < wcopy.num + wcopy.nd) || (y >= wcopy.num && y < wcopy.num + wcopy.nd)){
            println("debug type1...1... " + eStack.top._1.num + " -> " + eStack.top._2.num)
            c += newComponent(eStack.pop())
            x = eStack.top._1.num
            y = eStack.top._2.num
          }

          var lowpt1w = verticeslist.find(p => p.num == wcopy.lowpt1).get
          var ee: Edge = (v, lowpt1w)
          newVirtualEdge(v, lowpt1w, c)

          if(x == v.num && y == wcopy.lowpt1){
            var c = new Component
            println("debug type1...2... " + eStack.top._1.num + " -> " + eStack.top._2.num)
            c += newComponent(eStack.pop())
            println("debug type1...3... " + ee._1.num + " -> " + ee._2.num)
            c += newComponent((ee._1, ee._2))
            ee = (v, lowpt1w)
            newVirtualEdge(v, lowpt1w, c)
          }

          if(wcopy.lowpt1 != v.father){
            eStack.push((ee._1, ee._2))
            makeTreeEdge((lowpt1w, v))
            println("type1.1 " + lowpt1w.num + " -> " + v.num)
          }else{
            var c = new Component
            println("debug type1...4... " + lowpt1w.num + " -> " + v.num)
            c += newComponent((lowpt1w, v))
            ee = (lowpt1w, v)
            newVirtualEdge(lowpt1w, v, c)
            makeTreeEdge((lowpt1w, v))
            println("type1.2 " + lowpt1w.num + " -> " + v.num)
          }
        }

        /*
        * type1 finished
        */

        if (startPath.contains((v.num, wcopy.num))) {
          while (!tStack.top.equals(eos)) {
            tStack.pop()
          }
          tStack.pop()
        }
        h = tStack.top._1
        a = tStack.top._2
        b = tStack.top._3
        while ((a.num != v.num) && (b.num != v.num) && (v.highv() > h)) {
          tStack.pop()
          h = tStack.top._1
          a = tStack.top._2
          b = tStack.top._3
        }
      }else{
        val e = (v, w)
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
          println("debug frond..." + e._1.num + " -> " + e._2.num)
          c += newComponent(e)
          println("debug frond..." + w.num + " -> " + v.num)
          c += newComponent((w, v))
          val ee: Edge = (w, v)
          newVirtualEdge(w, v, c)
          makeTreeEdge(ee)
          println("frond " + w.num + " -> " + v.num)
        }else{
          eStack.push(e)
        }
      }
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
    c += ((v,w))
    val e: VirtualEdge = (v, w, c)
    virtualEdgeList += e
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

  var superroot = new Vertex
  superroot.num = -1





  search1(verticeslist.head, superroot)
  sort

  println("--------adjlist----------")

  verticeslist.foreach(f => {
    println("v: " + f.num)
    println(f.adjList.foreach(g => {
      print(g.num + "   ")
    }))
    println
  })
  startPath(verticeslist.head)


    println("--------------------------------------")

//    startPath.foreach(f => {
//      print(f._1 + " -> " +   f._2)
//      println()
//    })
//
//    println("--------------------------------------")


  graphc.initGraph(verticeslist, palmtree)
  tStack.push(eos)
  pathSearch(verticeslist.head)




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
