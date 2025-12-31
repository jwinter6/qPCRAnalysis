############################
# Hilfsfunktionen – Standardkurven
############################

# Hilfsfunktion: linearen Dynamikbereich (LDR) finden
find_linear_range <- function(df_group) {
  # df_group: Daten einer Kombination aus Target_ID/Sample über verschiedene Quantities
  df_group <- df_group %>%
    arrange(logQ)

  n <- nrow(df_group)
  if (n < 3) {
    return(NULL)
  }

  best_r2 <- -Inf
  best_fit <- NULL
  best_idx <- c(1, n)

  # Alle möglichen Fenster der Länge >= 3 durchprobieren
  for (start_idx in 1:(n - 2)) {
    for (end_idx in (start_idx + 2):n) {
      sub <- df_group[start_idx:end_idx, ]
      fit <- lm(Ct_mean ~ logQ, data = sub)
      r2  <- summary(fit)$r.squared

      if (!is.na(r2) && r2 > best_r2) {
        best_r2  <- r2
        best_fit <- fit
        best_idx <- c(start_idx, end_idx)
      }
    }
  }

  if (is.null(best_fit) || best_r2 < 0.98) {
    return(NULL)
  }

  sub <- df_group[best_idx[1]:best_idx[2], ]
  slope     <- coef(best_fit)[["logQ"]]
  intercept <- coef(best_fit)[["(Intercept)"]]

  efficiency <- (10^(-1 / slope) - 1) * 100

  tibble(
    logQ_min  = min(sub$logQ),
    logQ_max  = max(sub$logQ),
    Q_min     = min(sub$Quantity),
    Q_max     = max(sub$Quantity),
    n_points  = nrow(sub),
    slope     = slope,
    intercept = intercept,
    r2        = best_r2,
    efficiency = efficiency
  )
}
