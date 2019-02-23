full_join_n <- function(a, b, c, d, e, f, g, h, i, j) {
  merge1 <- full_join(a, b)
  merge2 <- full_join(c, d)
  merge3 <- full_join(e, f)
  merge4 <- full_join(g, h)
  merge5 <- full_join(i, j)
  merge6 <- full_join(merge1, merge2)
  merge7 <- full_join(merge3, merge4)
  merge8 <- full_join(merge6, merge7)
  merge9 <- full_join(merge8, merge5)
  return(merge9)
}

full_merge <- full_join_n(data91, data93, data96, data99, data02, data05, data08, data11, data14, data17)
