##############################################################################################
# [REVISION 2026-11] Benchmark tests for the revision (Public Choice version).
# Builds a group-round panel straight from the raw oTree export (independent of the cleaning in
# Vote_Selling.R) and tests the solved benchmark game:
#   * a vote is worth E = 2400 to either party iff the voter is pivotal, 0 otherwise;
#   * budgets M (900-1800) < E, so willingness to pay = M when pivotal (loss aversion has no bite);
#   * price-setting parties (vote buying): asymmetric Bertrand -> voter gets u* + max(0, M - Delta);
#   * price-setting voter (vote selling): voter asks M from both, both accept -> voter gets u* + M;
#   * predicted proposer gap for the voter (pivotal only) = min(Delta, M); 0 when non-pivotal.
# All results are stored in the list `rt` (used by Vote_Selling_Bahamonde_Canales_Paper_3.rnw).
# Standalone use (prints a report):  Rscript Vote_Selling_revision_tests.R
##############################################################################################

## ---- revision.tests ----
rt <- local({
  suppressMessages({
    if (!require("pacman")) install.packages("pacman"); library(pacman)
    p_load(rio, dplyr, tidyr, sandwich, lmtest, ggplot2)
  })
  options(dplyr.summarise.inform = FALSE)

  local.data <- "/Users/hectorbahamonde/research/Exp_Vote_Selling/data/data.xlsx"
  raw <- if (file.exists(local.data)) rio::import(local.data) else
    rio::import("https://github.com/hbahamonde/Economic_Experiment_Vote_Selling/raw/master/data/data.xlsx")
  n.raw <- nrow(raw)
  raw <- subset(raw, participant.payoff > 0)          # same exclusion as Vote_Selling.R
  E <- 2400

  # ---------------------------------------------------------------- panel
  grab <- function(g, k) {
    pre <- paste0("vote_", g, ".", k, ".")
    col <- function(x) { nm <- paste0(pre, x); if (nm %in% names(raw)) raw[[nm]] else NA }
    data.frame(
      game = ifelse(g == "b", "Vote Buying", "Vote Selling"),
      session = raw$session.code, pid = raw$participant.code, round = k,
      group = col("group.id_in_subsession"),
      role = col("player.votanteOpartido"), label = col("player.tipoAoB"),
      M = col("group.presupuesto"), nA = col("group.n_votantes_A"), nB = col("group.n_votantes_B"),
      uA = col("group.pje_win_cA"), uB = col("group.pje_win_cB"),
      puntos = col("player.puntos"),
      offer = col("player.p_oferta_amount"), vb_choice = col("player.p_oferta_acepta"),
      req_A = col("player.p_oferta_amount_A"), req_B = col("player.p_oferta_amount_B"),
      accepts = col("player.partido_envia_puntos"), vs_choice = col("player.votante_acepta_oferta"),
      stringsAsFactors = FALSE)
  }
  long <- bind_rows(lapply(c("b", "s"), function(g) bind_rows(lapply(1:3, function(k) grab(g, k))))) %>%
    filter(!is.na(role), !is.na(M), M > 0)

  gv <- long %>% filter(role == "votantes") %>%
    transmute(game, session, round, group, voter = pid, label, M, nA, nB, uA, uB,
              v_puntos = as.numeric(puntos), vb_choice, vs_choice,
              req_A = as.numeric(req_A), req_B = as.numeric(req_B)) %>%
    mutate(
      n_label = ifelse(label == "A", nA, nB), n_other = ifelse(label == "A", nB, nA),
      pivotal = as.integer(n_label - n_other == 1),
      Delta   = abs(uA - uB), u_star = pmax(uA, uB),
      pref    = ifelse(uA >= uB, "A", "B"),
      label_consistent = as.integer(label == pref),
      leader  = ifelse(nA > nB, "A", "B"),
      u_default = ifelse(leader == "A", uA, uB),
      pred_vb = ifelse(pivotal == 1, u_star + pmax(0, M - Delta), u_default),
      # Price-setting voter: the threat to sell to the other party is credible unless the label
      # party is the preferred party and Delta >= M (then no transfer in equilibrium).
      credible = as.integer(Delta < M | label != pref),
      pred_vs = ifelse(pivotal == 1, u_star + M * credible, u_default),
      pred_gap = pred_vs - pred_vb)

  pp <- long %>% filter(role != "votantes") %>%
    mutate(party = ifelse(role == "Partido A", "A", "B")) %>%
    select(game, session, round, group, pid, party, offer, accepts) %>%
    mutate(offer = as.numeric(offer), accepts = as.numeric(accepts)) %>%
    left_join(gv, by = c("game", "session", "round", "group")) %>%
    mutate(lead_party = as.integer(party == leader), pref_party = as.integer(party == pref),
           request = ifelse(party == "A", req_A, req_B),
           offer_p = 100 * offer / M, request_p = 100 * request / M)

  # ---------------------------------------------------------------- payoff-code check
  offers_w <- pp %>% select(game, session, round, group, party, offer, accepts) %>%
    pivot_wider(names_from = party, values_from = c(offer, accepts))
  gv <- gv %>%
    mutate(sold_to = case_when(
      game == "Vote Buying"  & vb_choice %in% c(1, 2) ~ c("A", "B")[pmax(1, vb_choice)],
      game == "Vote Selling" & vs_choice %in% c(1, 2) ~ c("A", "B")[pmax(1, vs_choice)],
      TRUE ~ NA_character_)) %>%
    left_join(offers_w, by = c("game", "session", "round", "group")) %>%
    mutate(
      transfer = case_when(
        is.na(sold_to) ~ 0,
        game == "Vote Buying"  & sold_to == "A" ~ offer_A,
        game == "Vote Buying"  & sold_to == "B" ~ offer_B,
        game == "Vote Selling" & sold_to == "A" ~ ifelse(accepts_A %in% 1, req_A, 0),
        game == "Vote Selling" & sold_to == "B" ~ ifelse(accepts_B %in% 1, req_B, 0)),
      transfer = ifelse(is.na(transfer), 0, transfer),
      moves = !is.na(sold_to) & sold_to != label &
        ((game == "Vote Buying"  & ifelse(sold_to == "A", offer_A, offer_B) > 0) |
         (game == "Vote Selling" & ifelse(sold_to == "A", req_A, req_B) > 0)),
      fA = nA + ifelse(moves & sold_to == "A", 1, ifelse(moves & sold_to == "B", -1, 0)),
      fB = nB + ifelse(moves & sold_to == "B", 1, ifelse(moves & sold_to == "A", -1, 0)),
      win_rule = ifelse(fA > fB, "A", "B"),
      rule_puntos = ifelse(win_rule == "A", uA, uB) + transfer,
      mismatch = v_puntos != rule_puntos,
      sold_pref = as.integer(!is.na(sold_to) & sold_to == pref & transfer > 0))

  # ---------------------------------------------------------------- helpers
  mean_cl <- function(y, cluster) {
    m <- lm(y ~ 1); s <- sqrt(vcovCL(m, cluster = cluster, type = "HC1")[1, 1]); b <- unname(coef(m)[1])
    list(est = b, se = s, lo = b - 1.96 * s, hi = b + 1.96 * s, n = length(y), G = length(unique(cluster)))
  }
  coef_cl <- function(m, cluster, term) {
    ct <- coeftest(m, vcov = vcovCL(m, cluster = cluster, type = "HC1"))
    list(est = ct[term, 1], se = ct[term, 2], p = ct[term, 4])
  }
  gap_test <- function(d, y = "v_puntos") {
    d$y <- d[[y]]
    m <- lm(y ~ I(game == "Vote Selling"), data = d)
    s <- sqrt(vcovCL(m, cluster = d$session, type = "HC1")[2, 2]); b <- unname(coef(m)[2])
    G <- length(unique(d$session)); dfree <- G - 2; tc <- qt(0.975, dfree)
    bench <- mean(d$pred_vs[d$game == "Vote Selling"]) - mean(d$pred_vb[d$game == "Vote Buying"])
    list(est = b, se = s, lo = b - tc * s, hi = b + tc * s, p = 2 * pt(-abs(b / s), dfree),
         mde = (tc + qt(0.8, dfree)) * s, bench = bench, p.bench = 2 * pt(-abs((b - bench) / s), dfree),
         tost = abs(b) + qt(0.95, dfree) * s, n = nrow(d), G = G)
  }

  # ---------------------------------------------------------------- tests
  out <- list()
  out$n.raw <- n.raw; out$n.dropped <- n.raw - nrow(raw)
  out$sample <- gv %>% group_by(game) %>%
    summarise(participants = n_distinct(c(voter, pp$pid[pp$game == first(game)])),
              sessions = n_distinct(session), voter.rounds = n(), pivotal = sum(pivotal),
              label.consistent = 100 * mean(label_consistent))
  out$participants <- tapply(long$pid, long$game, function(x) length(unique(x)))
  out$sessions <- tapply(long$session, long$game, function(x) length(unique(x)))
  out$mismatch <- gv %>% group_by(game) %>% summarise(n = n(), mismatches = sum(mismatch, na.rm = TRUE))
  out$bug.vs <- gv %>% filter(game == "Vote Selling", label == "A", sold_to == "A", transfer > 0) %>%
    summarise(n = n(), unpaid = sum(mismatch), lost = sum(rule_puntos - v_puntos))

  vb  <- pp %>% filter(game == "Vote Buying", !is.na(offer))
  vbn <- vb %>% filter(pivotal == 0); vbp <- vb %>% filter(pivotal == 1)
  out$vb.np.any   <- mean_cl(100 * (vbn$offer > 0), vbn$pid)
  out$vb.np.pct   <- mean_cl(vbn$offer_p, vbn$pid)
  out$vb.np.pos   <- mean_cl(vbn$offer_p[vbn$offer > 0], vbn$pid[vbn$offer > 0])
  out$vb.p.any    <- 100 * mean(vbp$offer > 0)
  out$vb.np.n <- nrow(vbn); out$vb.p.n <- nrow(vbp)
  out$vb.piv      <- coef_cl(lm(offer_p ~ pivotal, data = vb), vb$pid, "pivotal")
  out$vb.piv.any  <- coef_cl(lm(I(100 * (offer > 0)) ~ pivotal, data = vb), vb$pid, "pivotal")
  out$vb.pref <- vbp %>% mutate(bench = ifelse(pref_party == 1, 100 * pmax(0, M - Delta) / M, 100)) %>%
    group_by(pref_party) %>% summarise(n = n(), any = 100 * mean(offer > 0), pct = mean(offer_p), bench = mean(bench))
  out$vb.pref.test <- coef_cl(lm(offer_p ~ pref_party, data = vbp), vbp$pid, "pref_party")

  vs  <- pp %>% filter(game == "Vote Selling")
  vsa <- vs %>% filter(request > 0, !is.na(accepts))
  out$vs.acc <- vsa %>% group_by(pivotal) %>% summarise(n = n(), accept = 100 * mean(accepts), req = mean(request_p))
  out$vs.acc.test <- coef_cl(lm(I(100 * accepts) ~ pivotal, data = vsa), vsa$pid, "pivotal")
  out$vs.acc.price.np <- coef_cl(lm(I(100 * accepts) ~ request_p, data = subset(vsa, pivotal == 0)),
                                 subset(vsa, pivotal == 0)$pid, "request_p")
  out$vs.acc.price.p  <- coef_cl(lm(I(100 * accepts) ~ request_p, data = subset(vsa, pivotal == 1)),
                                 subset(vsa, pivotal == 1)$pid, "request_p")
  vsp <- vs %>% filter(pivotal == 1, !is.na(request))
  out$vs.p.req  <- mean_cl(vsp$request_p, vsp$voter)
  out$vs.p.full <- mean_cl(100 * (vsp$request >= vsp$M), vsp$voter)
  out$vs.p.zero <- 100 * mean(vsp$request == 0)
  out$vs.pref <- vsp %>% group_by(pref_party) %>% summarise(n = n(), pct = mean(request_p), zero = 100 * mean(request == 0))
  vsn <- vs %>% filter(pivotal == 0, !is.na(request))
  out$vs.np.req <- mean_cl(vsn$request_p, vsn$voter)

  out$lead.vb <- coef_cl(lm(offer_p ~ lead_party, data = vbn), vbn$pid, "lead_party")
  out$lead.vb.p <- coef_cl(lm(offer_p ~ lead_party, data = vbp), vbp$pid, "lead_party")
  out$lead.vs <- coef_cl(lm(I(100 * accepts) ~ lead_party, data = subset(vsa, pivotal == 0)),
                         subset(vsa, pivotal == 0)$pid, "lead_party")
  out$lead.tab <- bind_rows(
    vb %>% group_by(pivotal, lead_party) %>% summarise(n = n(), value = mean(offer_p)) %>% mutate(stat = "Vote buying: offer (% of budget)"),
    vsa %>% group_by(pivotal, lead_party) %>% summarise(n = n(), value = 100 * mean(accepts)) %>% mutate(stat = "Vote selling: requests accepted (%)"))

  out$payoffs <- gv %>% group_by(game, pivotal) %>%
    summarise(n = n(), recorded = mean(v_puntos), rule = mean(rule_puntos),
              benchmark = mean(ifelse(game == "Vote Buying", pred_vb, pred_vs)),
              transfer = mean(transfer), bench.transfer = mean(ifelse(pivotal == 1,
                ifelse(game == "Vote Buying", pmax(0, M - Delta), M * credible), 0)),
              win.pref = 100 * mean(win_rule == pref), credible = 100 * mean(credible),
              sold.pref = 100 * mean(sold_pref), M = mean(M), Delta = mean(Delta))
  out$pred.gap.piv <- mean(gv$pred_gap[gv$pivotal == 1])
  out$pred.gap.pct <- 100 * mean(gv$pred_gap[gv$pivotal == 1] / gv$M[gv$pivotal == 1])
  out$gap <- list(
    all.rec = gap_test(gv), piv.rec = gap_test(subset(gv, pivotal == 1)), np.rec = gap_test(subset(gv, pivotal == 0)),
    all.rule = gap_test(gv, "rule_puntos"), piv.rule = gap_test(subset(gv, pivotal == 1), "rule_puntos"),
    np.rule = gap_test(subset(gv, pivotal == 0), "rule_puntos"))

  # ---------------------------------------------------------------- extra tests of the proposer advantage
  gvp <- subset(gv, pivotal == 1)
  # (a) Does the realized advantage rise with Delta? Benchmark slope = 1 (for Delta < M).
  m_int <- lm(rule_puntos ~ I(game == "Vote Selling") * Delta, data = gvp)
  V_int <- vcovCL(m_int, cluster = gvp$session, type = "HC1")
  tn <- 'I(game == "Vote Selling")TRUE:Delta'
  b_int <- unname(coef(m_int)[tn]); s_int <- sqrt(V_int[tn, tn]); df_int <- length(unique(gvp$session)) - 2
  out$int <- list(est = b_int, se = s_int, p = 2 * pt(-abs(b_int / s_int), df_int),
                  p.one = 2 * pt(-abs((b_int - 1) / s_int), df_int))
  # (b) Randomization inference: permute institution labels across sessions
  set.seed(20261108)
  sess <- unique(gv[, c("session", "game")])
  ri_stat <- function(d, lab) {
    g <- lab[match(d$session, sess$session)]
    mean(d$rule_puntos[g == "Vote Selling"]) - mean(d$rule_puntos[g == "Vote Buying"])
  }
  obs_ri <- ri_stat(gvp, sess$game)
  sims <- replicate(5000, ri_stat(gvp, sample(sess$game)))
  out$ri <- list(est = obs_ri, p = mean(abs(sims) >= abs(obs_ri)),
                 p.bench = mean(abs(sims - mean(sims)) >= abs(obs_ri - out$gap$piv.rule$bench)))
  # (c) Excluding every round with a payoff discrepancy
  out$gap.clean <- gap_test(subset(gv, pivotal == 1 & !mismatch))
  out$n.clean <- sum(gvp$mismatch)

  # (d) Experience: do the deviations shrink across the three paid rounds?
  lin_round <- function(d, y, cl) { m <- lm(d[[y]] ~ d$round); coef_cl(m, cl, "d$round") }
  vbn$any100 <- 100 * (vbn$offer > 0)
  vsan <- subset(vsa, pivotal == 0); vsan$acc100 <- 100 * vsan$accepts
  out$learn <- list(
    vb.np.any = vbn %>% group_by(round) %>% summarise(v = mean(any100)),
    vb.np.any.t = lin_round(vbn, "any100", vbn$pid),
    vs.np.acc = vsan %>% group_by(round) %>% summarise(v = mean(acc100)),
    vs.np.acc.t = lin_round(vsan, "acc100", vsan$pid),
    vs.p.req = vsp %>% group_by(round) %>% summarise(v = mean(request_p)),
    vs.p.req.t = lin_round(vsp, "request_p", vsp$voter))

  # ---------------------------------------------------------------- figures
  theme_pc <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          legend.position = "bottom", legend.title = element_blank(),
          strip.text = element_text(face = "bold"), axis.title.x = element_blank())

  # Figure: voter payoffs, observed (rule-implied, 95% session-clustered CI) vs benchmark
  f1 <- gv %>% group_by(game, pivotal) %>%
    group_modify(~ { m <- mean_cl(.x$rule_puntos, .x$session)
      data.frame(observed = m$est, lo = m$lo, hi = m$hi,
                 benchmark = if (.y$game == "Vote Buying") mean(.x$pred_vb) else mean(.x$pred_vs)) }) %>%
    ungroup() %>%
    mutate(game = factor(ifelse(game == "Vote Buying", "Price-setting\nparties", "Price-setting\nvoter"),
                         levels = c("Price-setting\nparties", "Price-setting\nvoter")),
           pivotal = factor(ifelse(pivotal == 1, "Pivotal voter", "Non-pivotal voter"),
                            levels = c("Pivotal voter", "Non-pivotal voter")))
  g1 <- ggplot(f1, aes(x = game)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.08, linewidth = 0.5, colour = "grey30") +
    geom_point(aes(y = observed, shape = "Observed (95% CI)"), size = 2.8, colour = "black") +
    geom_point(aes(y = benchmark, shape = "Benchmark prediction"), size = 3, stroke = 1, colour = "black") +
    scale_shape_manual(values = c("Observed (95% CI)" = 16, "Benchmark prediction" = 2)) +
    facet_wrap(~ pivotal) + labs(y = "Voter points per round") + theme_pc
  ggsave("/Users/hectorbahamonde/research/Exp_Vote_Selling/benchmark_payoffs.pdf", g1, width = 6.5, height = 3.6)

  # Figure: party and voter behaviour vs benchmark
  ci <- function(y, cl) { m <- mean_cl(y, cl); c(m$est, m$lo, m$hi) }
  rows <- list(
    c("A. Parties' offers\n(price-setting parties)", "Non-pivotal", ci(vbn$offer_p, vbn$pid), 0),
    c("A. Parties' offers\n(price-setting parties)", "Pivotal", ci(vbp$offer_p, vbp$pid),
      mean(ifelse(vbp$pref_party == 1, 100 * pmax(0, vbp$M - vbp$Delta) / vbp$M, 100))),
    c("B. Requests accepted\n(price-setting voter)", "Non-pivotal",
      ci(100 * vsa$accepts[vsa$pivotal == 0], vsa$pid[vsa$pivotal == 0]), 0),
    c("B. Requests accepted\n(price-setting voter)", "Pivotal",
      ci(100 * vsa$accepts[vsa$pivotal == 1], vsa$pid[vsa$pivotal == 1]), 100),
    c("C. Voter's requests\n(price-setting voter)", "Pivotal", ci(vsp$request_p, vsp$voter),
      100 * mean(vsp$credible)))
  f2 <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(f2) <- c("panel", "pivotal", "observed", "lo", "hi", "benchmark")
  f2[, 3:6] <- lapply(f2[, 3:6], as.numeric)
  f2$pivotal <- factor(f2$pivotal, levels = c("Pivotal", "Non-pivotal"))
  g2 <- ggplot(f2, aes(x = pivotal)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.08, linewidth = 0.5, colour = "grey30") +
    geom_point(aes(y = observed, shape = "Observed (95% CI)"), size = 2.8) +
    geom_point(aes(y = benchmark, shape = "Benchmark prediction"), size = 3, stroke = 1) +
    scale_shape_manual(values = c("Observed (95% CI)" = 16, "Benchmark prediction" = 2)) +
    scale_y_continuous(limits = c(0, 100)) +
    facet_wrap(~ panel, scales = "free_x") + labs(y = "Percent (of budget, or of requests)") + theme_pc
  ggsave("/Users/hectorbahamonde/research/Exp_Vote_Selling/benchmark_behaviour.pdf", g2, width = 6.5, height = 3.6)

  out$gv <- gv; out$pp <- pp
  out
})

# Formatting helpers for \Sexpr{}
neg <- function(s) sub("^-", "$-$", s)                   # typographic minus in LaTeX
f0 <- function(x) neg(format(round(x), big.mark = ",", trim = TRUE))
f1 <- function(x) neg(formatC(x, format = "f", digits = 1))
f2 <- function(x) neg(formatC(x, format = "f", digits = 2))
fp <- function(p) if (p < 0.001) "< 0.001" else paste("=", formatC(p, format = "f", digits = 3))
## ----

if (!interactive() && is.null(getOption("knitr.in.progress"))) {
  options(width = 160)
  print(rt[setdiff(names(rt), c("gv", "pp"))])
}
