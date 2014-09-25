// worksheet não funciona pra stream !!!!!
val as = Seq({
  println(s"a1")
  true
}, {
  println(s"a2")
  false
}, {
  println(s"a3")
  true
})

val bs = Seq({
  println(s"b1")
  true
}, {
  println(s"b2")
  true
}, {
  println(s"b3")
  false
})

val f = (for {
  a <- as.par.toStream
  b <- bs.par.toStream
} yield {
  lazy val res = {
    println(s"a&&b")
    a && b
  }
  res
})
f.forall(_ == true)
