import math.min

def distanceMatrix(s1:String, s2:String)= {
  def minimum(i1: Int, i2: Int, i3: Int)=min(min(i1, i2), i3)
  val dist=Array.tabulate(s2.length+1, s1.length+1){(j,i)=>if(j==0) i else if (i==0) j else 0}
  for(j<-1 to s2.length; i<-1 to s1.length)
     dist(j)(i)=if(s2(j-1)==s1(i-1)) dist(j-1)(i-1)
          else minimum(dist(j-1)(i)+1, dist(j)(i-1)+1, dist(j-1)(i-1)+1)
   dist
}
 
def stringMatrix(m: Array[Array[Int]]) =
  m.map { row => row.mkString("  ") } .mkString("\n")

def distanceMatrixS(s1:String, s2:String)= {
  val m = distanceMatrix(s1, s2)
  stringMatrix(m)
}
  
val m1 = distanceMatrix("kitten", "sitting")
println(stringMatrix(m1))
m1("sitting".length)("kitten".length)
