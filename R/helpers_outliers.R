############################
# Hilfsfunktionen – Outlier
############################

# Lauf eines Outlier-Tests auf einem Vektor von Residuen
run_outlier_test <- function(x, method) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    return(rep(FALSE, length(x)))
  }

  if (method == "Dixon") {
    res <- tryCatch(
      DixonTest(x),
      error = function(e) NULL
    )
    if (is.null(res)) return(rep(FALSE, length(x)))
    idx <- as.numeric(res$statistic["index"])
    flags <- rep(FALSE, length(x))
    if (!is.na(idx) && idx >= 1 && idx <= length(x)) {
      flags[idx] <- TRUE
    }
    return(flags)
  }

  if (method == "Grubbs") {
    res <- tryCatch(
      grubbs.test(x),
      error = function(e) NULL
    )
    if (is.null(res)) return(rep(FALSE, length(x)))
    idx <- which.max(abs(x - mean(x, na.rm = TRUE)))
    flags <- rep(FALSE, length(x))
    if (!is.na(idx)) {
      flags[idx] <- TRUE
    }
    return(flags)
  }

  if (method == "Rosner") {
    k <- min(3, n - 2)
    res <- tryCatch(
      rosnerTest(x, k = k),
      error = function(e) NULL
    )
    if (is.null(res)) return(rep(FALSE, length(x)))
    out <- res$all.stats
    flags <- rep(FALSE, length(x))
    if (!is.null(out$Outlier)) {
      flags[out$Obs.Num[out$Outlier]] <- TRUE
    }
    return(flags)
  }

  rep(FALSE, length(x))
}
