# =============================================================================
# fitness_filter.R — the online fitness filter
# -----------------------------------------------------------------------------
# Carries the fitness model's latent state forward one run at a time. The
# model's static parameters are learned offline by MCMC in the running-analysis
# repo and arrive here in a model bundle (model/fitness_model.rds); everything
# after that happens in this app.
#
#   static parameters  S posterior draws, held fixed. A run arriving after the
#                      fit reweights them by how well each predicted it; when
#                      the effective sample size falls below half, it is time
#                      to refit offline and install a new bundle.
#   dynamic state      fitness phi(t) = phi0 + f(t) (+ training-load terms for
#                      v11), f a Matern-5/2 GP written as the 3-state linear
#                      SDE it exactly is. Per draw, a Gaussian over the state,
#                      propagated exactly between runs and updated at each run
#                      by assumed-density filtering (Laplace-centred
#                      Gauss-Hermite quadrature under the model's likelihood).
#
# This is a copy of running-analysis/R/40_online_filter.R without its batch-only
# parts (history replay, smoother). The two must implement the same model, so
# a change to the likelihood there needs copying here.
# =============================================================================

# ── 3x3 algebra, vectorised over draws ───────────────────────────────────────

mm3 <- function(A, B) {
  C <- array(0, dim(A))
  for (i in 1:3) for (j in 1:3)
    C[, i, j] <- A[, i, 1] * B[, 1, j] + A[, i, 2] * B[, 2, j] + A[, i, 3] * B[, 3, j]
  C
}
t3  <- function(A) aperm(A, c(1, 3, 2))
mv3 <- function(A, v) sapply(1:3, function(i) A[, i, 1] * v[, 1] + A[, i, 2] * v[, 2] + A[, i, 3] * v[, 3])
sym3 <- function(A) (A + t3(A)) / 2

# ── Matern-5/2 as a linear SDE ───────────────────────────────────────────────
#   dx = F x dt + L dW,  F = companion matrix of (s + k)^3,  k = sqrt(5) / ell
# Stationary covariance and the exact transition over dt days.  F + kI is
# nilpotent, so expm(F dt) = exp(-k dt) (I + N dt + N^2 dt^2 / 2) exactly.

matern52_pinf <- function(alpha, ell) {
  k <- sqrt(5) / ell; a2 <- alpha^2; S <- length(k)
  P <- array(0, c(S, 3, 3))
  P[, 1, 1] <- a2
  P[, 2, 2] <- a2 * k^2 / 3
  P[, 3, 3] <- a2 * k^4
  P[, 1, 3] <- P[, 3, 1] <- -a2 * k^2 / 3
  P
}

matern52_transition <- function(ell, dt) {
  k <- sqrt(5) / ell; S <- length(k)
  N <- array(0, c(S, 3, 3))
  N[, 1, 1] <- k;  N[, 1, 2] <- 1
  N[, 2, 2] <- k;  N[, 2, 3] <- 1
  N[, 3, 1] <- -k^3; N[, 3, 2] <- -3 * k^2; N[, 3, 3] <- -2 * k
  N2 <- mm3(N, N)
  I  <- array(0, c(S, 3, 3)); for (i in 1:3) I[, i, i] <- 1
  (I + N * dt + N2 * dt^2 / 2) * exp(-k * dt)
}

# ── Gauss-Hermite nodes (Golub-Welsch), for integrals against N(0, 1) ────────
gh_nodes <- function(n) {
  b <- sqrt(seq_len(n - 1) / 2)
  J <- matrix(0, n, n); J[cbind(1:(n - 1), 2:n)] <- b; J[cbind(2:n, 1:(n - 1))] <- b
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values * sqrt(2), w = e$vectors[1, ]^2)   # sum(w) = 1
}
GH <- gh_nodes(9)

# ── The model's parameters, as the filter needs them ─────────────────────────

online_params <- function(online) {
  d <- online$draws
  list(S = nrow(d), d = d, delta = online$delta, const = online$const,
       banister = "tau_fitness" %in% names(d))
}

# A static parameter, or `default` for a model that does not have it
# (b_fresh exists in v8-v10 only)
par_or <- function(d, name, default = 0) if (name %in% names(d)) d[[name]] else default

# ── Initial state: the GP's stationary prior at the first day ────────────────

online_init <- function(par) {
  S <- par$S
  list(t     = 0,                               # days since const$date_min
       m     = matrix(0, S, 3),
       P     = matern52_pinf(par$d$alpha_gp, par$d$ell_gp),
       lw    = rep(0, S),                       # log weights of theta draws
       ew7   = 0, ew42 = 0, load_day = 0,       # freshness EWMAs, see below
       Fv    = rep(1, S), Gv = rep(1, S),       # v11 fitness / fatigue, per draw
       recent = numeric(0),                     # last 28 days' load, for plans
       seen  = character(0))
}

online_predict <- function(st, par, t_new) {
  dt <- t_new - st$t
  stopifnot(dt >= -1e-9)
  if (dt > 1e-9) {
    A    <- matern52_transition(par$d$ell_gp, dt)
    Pinf <- matern52_pinf(par$d$alpha_gp, par$d$ell_gp)
    st$m <- mv3(A, st$m)
    # A P A' + Q  with  Q = Pinf - A Pinf A'
    st$P <- sym3(mm3(mm3(A, st$P - Pinf), t3(A)) + Pinf)
  }
  st$t <- t_new
  st
}

# ── Training load: freshness (v8-v10) and fitness / fatigue (v11) ───────────
# ewma_load() in 20_new_models.R and banister_day() in the v11 Stan file: the
# value on day d uses load up to d - 1.  The state keeps the averages valid on
# day floor(st$t) and the load logged so far that day; moving to a later day
# folds that load in, then decays through the days with nothing logged.
online_advance_load <- function(st, par, day_new) {
  day_old <- floor(st$t)
  n <- day_new - day_old
  if (n <= 0) return(st)
  a7 <- exp(-1 / 7); a42 <- exp(-1 / 42)
  st$ew7  <- (a7  * st$ew7  + (1 - a7)  * st$load_day) * a7^(n - 1)
  st$ew42 <- (a42 * st$ew42 + (1 - a42) * st$load_day) * a42^(n - 1)
  if (isTRUE(par$banister)) {
    aF <- exp(-1 / par$d$tau_fitness); aG <- exp(-1 / par$d$tau_fatigue)
    st$Fv <- (aF * st$Fv + (1 - aF) * st$load_day) * aF^(n - 1)
    st$Gv <- (aG * st$Gv + (1 - aG) * st$load_day) * aG^(n - 1)
  }
  st$recent <- tail(c(st$recent, st$load_day, rep(0, n - 1)), 28)
  st$load_day <- 0
  st
}
online_freshness <- function(st, par) (st$ew7 - st$ew42) / par$const$fresh_sd

# b_F F - b_G G for each draw at the state's day; 0 for models without it
banister_now <- function(st, par)
  if (isTRUE(par$banister)) par$d$b_fitness * st$Fv - par$d$b_fatigue * st$Gv else 0

# Move the state to time t for *prediction*.  Days up to `as_of` (default
# today) that have no runs logged really had zero load.  Days after it follow
# `plan`: a vector of daily standardised loads (HRSS / const$load_mean), one
# per day from as_of onwards, recycled if short.  The default plan carries on
# at the average of the last 28 days.
online_project <- function(st, par, t, plan = NULL,
                           as_of = as.numeric(Sys.Date() - par$const$date_min)) {
  day_old <- floor(st$t); day_new <- floor(t)
  if (day_new > day_old) {
    if (is.null(plan)) plan <- if (length(st$recent)) mean(st$recent) else 1
    for (dd in day_old:(day_new - 1)) {
      # fold the load of day dd, then step to day dd + 1
      L <- if (dd == day_old) st$load_day
           else if (dd < as_of) 0
           else plan[(dd - max(as_of, day_old + 1)) %% length(plan) + 1]
      st$load_day <- L
      # online_advance_load() measures from floor(st$t), so the Matern state
      # has to move with it one day at a time; otherwise each step re-decays
      # from day_old and the averages collapse whatever the plan
      st <- online_predict(online_advance_load(st, par, dd + 1), par, max(dd + 1, st$t))
    }
  }
  online_predict(st, par, t)
}

# ── Composed-error log density and its derivatives ───────────────────────────
# Exactly the Stan target:  e = y - frontier,  shortfall u ~ N+(mu, su).
# Returned per element; derivatives are with respect to e and mu.

log_mills <- function(z) dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE)

composed_ll <- function(e, mu, sigma, su, deriv = FALSE) {
  sc <- sqrt(sigma^2 + su^2); lam <- su / sigma; c2 <- sigma / (sc * su)
  z1 <- (e + mu) / sc
  z2 <- mu * c2 - e * lam / sc
  z3 <- mu / su
  ll <- -log(sc) + dnorm(z1, log = TRUE) + pnorm(z2, log.p = TRUE) - pnorm(z3, log.p = TRUE)
  if (!deriv) return(ll)
  r2 <- exp(log_mills(z2)); r3 <- exp(log_mills(z3))
  h2 <- -r2 * (z2 + r2);    h3 <- -r3 * (z3 + r3)
  b  <- lam / sc
  list(ll = ll,
       de   = -z1 / sc - b * r2,
       dmu  = -z1 / sc + c2 * r2 - r3 / su,
       dee  = -1 / sc^2 + b^2 * h2,
       demu = -1 / sc^2 - b * c2 * h2,
       dmumu = -1 / sc^2 + c2^2 * h2 - h3 / su^2)
}

# ── Observation matrices for one run, S x n ──────────────────────────────────
# `ro` is the run's best-effort rows, with the same columns the batch model
# sees (log_speed, log_dist_c, log_frac, effort, has_hr, encumbered, is_race,
# temp_ctr, elev_ctr, dist_idx).  Everything in the frontier except phi, and
# everything in the shortfall mean except eta, goes into C and M0.
run_matrices <- function(ro, par, fresh, ban = 0) {
  d <- par$d; S <- par$S; n <- nrow(ro)
  rep_c <- function(v) matrix(v, S, n, byrow = TRUE)
  rep_r <- function(v) matrix(v, S, n)
  C <- rep_r(d$phi0 + ban) + rep_r(par_or(d, "b_fresh")) * fresh -
       rep_r(d$beta1) * rep_c(ro$log_dist_c) -
       rep_r(d$beta2) * rep_c(ro$log_dist_c^2) -
       rep_r(d$b_temp) * rep_c(ro$temp_ctr) -
       rep_r(d$b_elev) * rep_c(ro$elev_ctr)
  R  <- rep_c(as.numeric(ro$is_race))
  M0 <- (rep_r(d$m0) -
         rep_r(d$m_effort) * rep_c((ro$effort - par$const$effort_ref) * ro$has_hr) -
         rep_r(d$m_frac) * rep_c(ro$log_frac) +
         rep_r(d$m_enc) * rep_c(as.numeric(ro$encumbered)) +
         par$delta[, ro$dist_idx, drop = FALSE]) * (1 - R)
  list(Y = rep_c(ro$log_speed), C = C, M0 = M0, R = R, n = n)
}

# The log posterior is not jointly concave in (f, eta) everywhere: the
# -log Phi(mu / su) normaliser is convex in mu, and su is small, so where a hard
# training effort puts mu near zero the Hessian can lose definiteness.  Shift
# both eigenvalues down just enough (Levenberg) that Newton always ascends.  At
# the mode this only affects the quadrature *proposal*, not the integrand, so
# the moments stay exact.
nd_hessian <- function(h11, h12, h22, vf, ve) {
  lmax <- (h11 + h22) / 2 + sqrt(((h11 - h22) / 2)^2 + h12^2)
  tau  <- pmax(0, lmax + 0.25 * pmin(1 / vf, 1 / ve))
  tau[!is.finite(tau)] <- 0
  list(h11 = h11 - tau, h12 = h12, h22 = h22 - tau)
}

# ── Measurement update for one run ───────────────────────────────────────────
# For each draw: joint posterior over (f, eta) under the Gaussian prior
# N(m_f, P_ff) x N(0, sigma_eta^2).  Newton to the mode, Gauss-Hermite on the
# Laplace-rotated grid for the exact moments and normaliser.  Returns the
# updated state and the per-draw log predictive density of the run.

online_update <- function(st, par, ro, fresh, iters = 8) {
  d <- par$d; S <- par$S
  X <- run_matrices(ro, par, fresh, banister_now(st, par))
  # noise scale per observation; v10 gives races their own
  sr <- race_sigma(d)
  X$sigma <- matrix(d$sigma, S, X$n) * (1 - X$R) + matrix(sr, S, X$n) * X$R
  X$su <- matrix(d$su, S, X$n)
  has_train <- any(ro$is_race == 0)

  mf <- st$m[, 1]; vf <- st$P[, 1, 1]
  ve <- d$sigma_eta^2

  logpost <- function(f, eta) {
    e <- X$Y - X$C - f; mu <- X$M0 + eta * (1 - X$R)
    rowSums(composed_ll(e, mu, X$sigma, X$su)) -
      0.5 * (f - mf)^2 / vf - 0.5 * eta^2 / ve
  }

  # Newton on (f, eta), vectorised over draws, with step halving
  f <- mf; eta <- rep(0, S)
  for (it in seq_len(iters)) {
    e <- X$Y - X$C - f; mu <- X$M0 + eta * (1 - X$R)
    D <- composed_ll(e, mu, X$sigma, X$su, deriv = TRUE)
    Tm <- 1 - X$R
    g1 <- -rowSums(D$de) - (f - mf) / vf                  # de/df = -1
    g2 <-  rowSums(D$dmu * Tm) - eta / ve
    H <- nd_hessian(rowSums(D$dee) - 1 / vf, -rowSums(D$demu * Tm),
                    rowSums(D$dmumu * Tm^2) - 1 / ve, vf, ve)
    H11 <- H$h11; H12 <- H$h12; H22 <- H$h22
    det <- H11 * H22 - H12^2
    s1 <- -( H22 * g1 - H12 * g2) / det
    s2 <- -(-H12 * g1 + H11 * g2) / det
    lp0 <- logpost(f, eta); step <- rep(1, S)
    for (h in 1:6) {
      bad <- !(logpost(f + step * s1, eta + step * s2) >= lp0 - 1e-10)
      bad[is.na(bad)] <- TRUE
      if (!any(bad)) break
      step[bad] <- step[bad] / 2
    }
    f <- f + step * s1; eta <- eta + step * s2
    if (isTRUE(max(abs(step * s1)) < 1e-9 && max(abs(step * s2)) < 1e-9)) break
  }

  # Laplace covariance at the mode; its Cholesky rotates the GH grid
  e <- X$Y - X$C - f; mu <- X$M0 + eta * (1 - X$R)
  D <- composed_ll(e, mu, X$sigma, X$su, deriv = TRUE); Tm <- 1 - X$R
  H <- nd_hessian(rowSums(D$dee) - 1 / vf, -rowSums(D$demu * Tm),
                  rowSums(D$dmumu * Tm^2) - 1 / ve, vf, ve)
  H11 <- H$h11; H12 <- H$h12; H22 <- H$h22
  det <- H11 * H22 - H12^2
  S11 <- -H22 / det; S12 <- H12 / det; S22 <- -H11 / det       # -H^{-1}
  l11 <- sqrt(S11); l21 <- S12 / l11; l22 <- sqrt(pmax(S22 - l21^2, 1e-300))
  if (!has_train) { l21 <- 0; l22 <- sqrt(ve) }  # eta decoupled: prior only

  K <- length(GH$x)
  zz <- expand.grid(a = seq_len(K), b = seq_len(K))
  za <- GH$x[zz$a]; zb <- GH$x[zz$b]; wq <- GH$w[zz$a] * GH$w[zz$b]
  lw_nodes <- matrix(0, S, nrow(zz)); fn <- lw_nodes
  for (q in seq_len(nrow(zz))) {
    fq <- f   + l11 * za[q]
    eq <- eta + l21 * za[q] + l22 * zb[q]
    fn[, q] <- fq
    # integrand / Gaussian proposal density, times the GH weight
    lw_nodes[, q] <- logpost(fq, eq) + 0.5 * (za[q]^2 + zb[q]^2) + log(wq[q])
  }
  mx <- apply(lw_nodes, 1, max)
  w  <- exp(lw_nodes - mx); sw <- rowSums(w)
  # log p(run | past, theta):  integral of lik x prior, prior normalised
  loglik <- mx + log(sw) + log(2 * pi) + log(l11 * l22) -
            0.5 * log(2 * pi * vf) - 0.5 * log(2 * pi * ve)
  w  <- w / sw
  pm <- rowSums(w * fn)
  pv <- pmax(rowSums(w * fn^2) - pm^2, 1e-12)

  # Project onto x: the conditional of (f', f'') given f is linear-Gaussian,
  # so matching the moments of f fixes the whole Gaussian.
  Kg <- st$P[, , 1] / vf                                # S x 3, gain on f
  st$m <- st$m + Kg * (pm - mf)
  for (i in 1:3) for (j in 1:3)
    st$P[, i, j] <- st$P[, i, j] - Kg[, i] * Kg[, j] * (vf - pv)
  st$P <- sym3(st$P)

  list(state = st, loglik = loglik, f_mean = pm, f_var = pv)
}

# ── Process one activity: freshness, predict, update, load, reweighting ──────
# `run` is a one-row list with day (days since date_min, numeric, may include a
# time-of-day fraction), load_std (the run's HRSS / const$load_mean, 0 if NA)
# and activity_id.  `ro` is its best-effort rows (possibly none).  `reweight`
# says whether the run is new information for theta, i.e. was not in the fit.

online_step <- function(st, par, run, ro, reweight) {
  st <- online_advance_load(st, par, floor(run$day))
  st <- online_predict(st, par, run$day)
  ll <- rep(0, par$S)
  if (!is.null(ro) && nrow(ro) > 0) {
    up <- online_update(st, par, ro, fresh = online_freshness(st, par))
    st <- up$state; ll <- up$loglik
    if (reweight) st$lw <- st$lw + ll
  }
  st$load_day <- st$load_day + run$load_std
  st$seen <- c(st$seen, as.character(run$activity_id))
  attr(st, "last_loglik") <- ll
  st
}

# Noise scale for race observations: v10's sigma_race, else the shared sigma
race_sigma <- function(d) if ("sigma_race" %in% names(d)) d$sigma_race else d$sigma

draw_weights <- function(st) { w <- exp(st$lw - max(st$lw)); w / sum(w) }
weights_ess  <- function(st) { w <- draw_weights(st); 1 / sum(w^2) }

# ── Race-time prediction from the current state ──────────────────────────────
# Predictive, as in fit_v8(): frontier - half-normal race shortfall + noise.
# v8-v10's freshness defaults to 0, matching the batch models' "phi at
# reference freshness"; pass use_freshness = TRUE to include it.  v11's
# fitness / fatigue always count, projected to t under `plan` (see
# online_project): a race prediction for a future date is a prediction for a
# particular training plan.

online_predict_race <- function(st, par, dist_km, t = st$t, n_per = 25L,
                                use_freshness = FALSE, plan = NULL,
                                probs = c(0.1, 0.5, 0.9)) {
  s2 <- online_project(st, par, t, plan)
  d  <- par$d; S <- par$S
  fr <- if (use_freshness) online_freshness(s2, par) else 0
  ban <- banister_now(s2, par)
  map_dfr(dist_km, function(dk) {
    ld  <- log(dk / 5)
    f   <- rnorm(S * n_per, rep(s2$m[, 1], n_per), rep(sqrt(s2$P[, 1, 1]), n_per))
    lv  <- rep(d$phi0 + ban + par_or(d, "b_fresh") * fr - d$beta1 * ld - d$beta2 * ld^2, n_per) + f -
           abs(rnorm(S * n_per, 0, rep(d$su, n_per))) +
           rnorm(S * n_per, 0, rep(race_sigma(d), n_per))
    tm  <- dk / exp(lv) * 60
    w   <- rep(draw_weights(st), n_per) / n_per
    o   <- order(tm); cw <- cumsum(w[o])
    q   <- sapply(probs, function(p) tm[o][which(cw >= p)[1]])
    tibble(distance_km = dk, pred_mean = sum(w * tm),
           !!!setNames(as.list(q), paste0("pred_q", round(100 * probs))))
  })
}

# Fitness now: the filtered phi(t) and the implied maximal 5 km, mixed over draws
online_fitness <- function(st, par, t = st$t, plan = NULL) {
  s2 <- online_project(st, par, t, plan); w <- draw_weights(st)
  phi_m <- par$d$phi0 + banister_now(s2, par) + s2$m[, 1]
  mean_phi <- sum(w * phi_m)
  var_phi  <- sum(w * (s2$P[, 1, 1] + (phi_m - mean_phi)^2))
  list(phi = mean_phi, phi_sd = sqrt(var_phi), t5_min = 5 / exp(mean_phi) * 60)
}
