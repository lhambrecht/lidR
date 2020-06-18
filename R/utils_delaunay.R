tDelaunay = function(P, trim = 0, ...)
{
  if (inherits(P, "LAS"))
    P <- coordinates3D(P)

  if (!is.data.frame(P) && !is.matrix(P))
    stop("Internal error. No method to triangulate this input", call. = FALSE)

  if (is.matrix(P))
    Q <- P[,1:2]
  else
    Q <- as.matrix(P[,1:2])

  if (nrow(Q) < 3)
    stop("Internal error in tDelaunay: cannot triangulate less than 3 points.", call. = FALSE)

  Q <- RTriangle::pslg(Q)
  D <- RTriangle::triangulate(Q)[["T"]]

  if (trim != 0)
  {
    N <- tInfo(D, as.matrix(P))
    K <- N[,7] < abs(trim)
    if (trim < 0) K <- !K
    D <- D[K, , drop = FALSE]
  }

  return(D)
}

# @rdname tDelaunay
tInterpolate = function(D, P, X, threads = 1L)
{
  stopifnot(is.matrix(D), is.matrix(P), is.matrix(X))

  I <- tSearch(D, P, X, threads)
  N <- tInfo(D, P)
  N <- N[I,1:4]
  Z <- -(X[,1] * N[,1] + X[,2] * N[,2] + N[,4]) / N[,3]
  return(Z)
}

# @rdname tDelaunay
tSearch = C_tsearch

# @rdname tDelaunay
tInfo =  C_tinfo
