pairs1 <-
  function (x) {
    as.vector(t(cbind(head(x, -1), s1 = tail(x, -1))))
  }
