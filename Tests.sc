// worksheet não funciona pra stream !!!!!
lazy val as = Seq({
  println(s"a1")
  true
}, {
  println(s"a2")
  false
}, {
  println(s"a3")
  false
}).Stream

val bs = Stream({
  println(s"b1")
  true
}, {
  println(s"b2")
  true
}, {
  println(s"b3")
  true
}).toStream

val f = for {
  a <- as
  b <- bs
} yield a && b

as.forall(_ == true)
//bs.forall(_ == true)
//f.forall(_ == true)