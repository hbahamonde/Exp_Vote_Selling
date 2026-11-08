##############################################################################################
# Vote_Selling_Bahamonde_Canales_Paper_3.r
# Replication code for "Who Sets the Price of a Vote? Theory and Experimental Evidence on
# Vote Buying and Vote Selling" (Vote_Selling_Bahamonde_Canales_Paper_3.rnw).
# The .rnw reads the labeled chunks below with knitr::read_chunk() and runs them in order.
# Data in data/; oTree code in experiment/.
#
# Benchmark tests (chunk `revision.tests`) build a group-round panel straight from the raw oTree
# export and test the solved benchmark game:
#   * a vote is worth E = 2400 to either party iff the voter is pivotal, 0 otherwise;
#   * budgets M (900-1800) < E, so willingness to pay = M when pivotal (loss aversion has no bite);
#   * price-setting parties (vote buying): asymmetric Bertrand -> voter gets u* + max(0, M - Delta);
#   * price-setting voter (vote selling): voter asks M from both, both accept -> voter gets u* + M;
#   * predicted proposer gap for the voter (pivotal only) = min(Delta, M); 0 when non-pivotal.
# All benchmark results are stored in the list `rt`.
##############################################################################################

## ---- loadings:d:2 ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Exp_Vote_Selling/")


# K-adic Data Analyses
# https://www.paulpoast.com/stata-software/4587316769

# max() that ignores NAs but returns NA (not -Inf) when all values are NA.
# Used for group-level aggregation: voter requests are recorded on the voter's row only.
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# NOTE ON GROUP IDENTIFIERS
# Groups (one voter + Party A + Party B) were previously identified by the group budget
# (vote_s.k.group.presupuesto). Budgets are drawn as int(runif(900, 1800)), so two groups
# in the same round can share a budget, and the budget-only grouping also pooled groups
# across sessions. All fills/aggregations now group by session.code + group.id_in_subsession,
# which is the unique group identifier in oTree.

# function to to recover mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(rio,tibble)
# Prefer the local clone of the data repo; fall back to GitHub.
local.data <- "/Users/hectorbahamonde/research/Exp_Vote_Selling/data/data.xlsx"
dat <- if (file.exists(local.data)) rio::import(local.data) else
  rio::import("https://github.com/hbahamonde/Economic_Experiment_Vote_Selling/raw/master/data/data.xlsx")

# dropping obs
# drop if payoff is 0
# report how many participants this drops (needed for the flow table).
n.participants.raw <- nrow(dat)
dat <- subset(dat, participant.payoff > 0)
n.participants.dropped.zero.payoff <- n.participants.raw - nrow(dat)
message("Dropped ", n.participants.dropped.zero.payoff, " participant rows with payoff <= 0 (of ", n.participants.raw, ").")

## ID VARS
id.vars = c(
  "participant.code",
  "session.code",
  "participant.payoff"
)


## VOTE BUYING VARS
v.selling.vars = c(
  "vote_s.1.player.votanteOpartido",
  "vote_s.1.player.tipoAoB",
  "vote_s.1.player.p_oferta_choice_A",
  "vote_s.1.player.p_oferta_choice_B",
  "vote_s.1.player.p_oferta_amount_A",
  "vote_s.1.player.p_oferta_amount_B",
  "vote_s.1.player.partido_envia_puntos",
  "vote_s.1.player.votante_acepta_oferta",
  "vote_s.1.player.win_lose",
  "vote_s.1.player.win_losev",
  "vote_s.1.player.puntos",
  "vote_s.1.player.payoff",
  "vote_s.1.group.id_in_subsession",
  "vote_s.1.group.presupuesto",
  "vote_s.1.group.n_votantes",
  "vote_s.1.group.n_votantes_A",
  "vote_s.1.group.n_votantes_B",
  "vote_s.1.group.partido_elegido",
  "vote_s.1.group.tipo_votante",
  "vote_s.1.group.ubicacion_pA",
  "vote_s.1.group.ubicacion_pB",
  "vote_s.1.group.pje_win_cA",
  "vote_s.1.group.pje_win_cB",
  "vote_s.2.player.votanteOpartido",
  "vote_s.2.player.tipoAoB",
  "vote_s.2.player.p_oferta_choice_A",
  "vote_s.2.player.p_oferta_choice_B",
  "vote_s.2.player.p_oferta_amount_A",
  "vote_s.2.player.p_oferta_amount_B",
  "vote_s.2.player.partido_envia_puntos",
  "vote_s.2.player.votante_acepta_oferta",
  "vote_s.2.player.win_lose",
  "vote_s.2.player.win_losev",
  "vote_s.2.player.puntos",
  "vote_s.2.player.payoff",
  "vote_s.2.group.id_in_subsession",
  "vote_s.2.group.presupuesto",
  "vote_s.2.group.n_votantes",
  "vote_s.2.group.n_votantes_A",
  "vote_s.2.group.n_votantes_B",
  "vote_s.2.group.partido_elegido",
  "vote_s.2.group.tipo_votante",
  "vote_s.2.group.ubicacion_pA",
  "vote_s.2.group.ubicacion_pB",
  "vote_s.2.group.pje_win_cA",
  "vote_s.2.group.pje_win_cB",
  "vote_s.3.player.votanteOpartido",
  "vote_s.3.player.tipoAoB",
  "vote_s.3.player.p_oferta_choice_A",
  "vote_s.3.player.p_oferta_choice_B",
  "vote_s.3.player.p_oferta_amount_A",
  "vote_s.3.player.p_oferta_amount_B",
  "vote_s.3.player.partido_envia_puntos",
  "vote_s.3.player.votante_acepta_oferta",
  "vote_s.3.player.win_lose",
  "vote_s.3.player.win_losev",
  "vote_s.3.player.puntos",
  "vote_s.3.player.payoff",
  "vote_s.3.group.id_in_subsession",
  "vote_s.3.group.presupuesto",
  "vote_s.3.group.n_votantes",
  "vote_s.3.group.n_votantes_A",
  "vote_s.3.group.n_votantes_B",
  "vote_s.3.group.partido_elegido",
  "vote_s.3.group.tipo_votante",
  "vote_s.3.group.ubicacion_pA",
  "vote_s.3.group.ubicacion_pB",
  "vote_s.3.group.pje_win_cA",
  "vote_s.3.group.pje_win_cB"
)


# SOCIO-DEMO VARS
socio.dem.vars = c(
  "survey.1.player.q3",
  "survey.1.player.q4" ,
  "survey.1.player.q6",
  "survey.1.player.q7",
  "survey.1.player.q8",
  "survey.1.player.q9",
  "survey.1.player.q10")

# litle codebooking
# "survey.1.player.q3"  # gender
# "survey.1.player.q4" # Salario:  (1=Les alcanza bien y pueden ahorrar, 2=Les alcanza justo y sin grandes dificultades, 3=No les alcanza y tienen dificultades, 4=No les alcanza y tienen grandes dificultades)
# "survey.1.player.q5" # Ingresos: (1= Menos de $288.800, 2= Entre $288.801 - $312.001, 3=Entre $312.002 - $361.002, 4= Entre $361.003 - $410.003, 5=Entre $410.004 - $459.004, 6=Entre $459.005 - $558.005, 7= Entre $558.006 - $657.006, 8=Entre $657.007 - $756.007, 9= Entre $756.008 - $1.005.008, 10= Más de $1.005.008)                         
# "survey.1.player.q6" # Simpatiza con partido político 1=sí, 2=no
# "survey.1.player.q7" # Qué partido? (1=Partido Socialista de Chile, 2= Unión Demócrata Independiente, 3= Renovación Nacional, 4=Partido Demócrata Cristiano, 5= Partido Comunistica de Chile, 6= Revolución Democrática, 7= Evolución Política, 8= Otro, 9=No me siento representado)                         
# "survey.1.player.q8" # Escala tendencia política (1=izq a 10=derecha)
# "survey.1.player.q9" # Voto ultima elección 1=sí, 0=no)                    
# "survey.1.player.q10" # Piensa votar proxima elección 1=sí, 0=no

# ID df
dat.v.s.ID = dat[c(id.vars, socio.dem.vars)]
names(dat.v.s.ID) <- c(
  "participant.code",
  "session.code",
  "participant.payoff",
  "gender", # "survey.1.player.q3",
  "salary.enough", # "survey.1.player.q4",
  "party.like", #"survey.1.player.q6", 
  "party.id", #"survey.1.player.q7",
  "left.right", #"survey.1.player.q8",
  "vote.last.election", #"survey.1.player.q9",
  "vote.next.election" #"survey.1.player.q10"
)


################################################ 
# ************** VOTE SELLING DATA **************
################################################ 

# subsetting vars
v.selling.dat = dat[c(id.vars, v.selling.vars, socio.dem.vars)]
# dropping obs that dont belong to the vote buying exp
v.selling.dat <- subset(v.selling.dat, !is.na(vote_s.1.player.votanteOpartido))

# extend voter's participant.code to the dyad: the common point between the parties is the voter.
p_load(dplyr,tidyverse)
# 1
v.selling.dat$participant.code.dyad.1 = ifelse(v.selling.dat$vote_s.1.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.1.group.id_in_subsession) %>% fill(participant.code.dyad.1, .direction = "downup")
# 2
v.selling.dat$participant.code.dyad.2 = ifelse(v.selling.dat$vote_s.2.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.2.group.id_in_subsession) %>% fill(participant.code.dyad.2, .direction = "downup")
# 3
v.selling.dat$participant.code.dyad.3 = ifelse(v.selling.dat$vote_s.3.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.3.group.id_in_subsession) %>% fill(participant.code.dyad.3, .direction = "downup")

# voter sells to both parties A and B
v.selling.dat$voter.sells.to.both.1 = ifelse(v.selling.dat$vote_s.1.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.1.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat$voter.sells.to.both.2 = ifelse(v.selling.dat$vote_s.2.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.2.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat$voter.sells.to.both.3 = ifelse(v.selling.dat$vote_s.3.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.3.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.1.group.id_in_subsession) %>% fill(voter.sells.to.both.1, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.2.group.id_in_subsession) %>% fill(voter.sells.to.both.2, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.3.group.id_in_subsession) %>% fill(voter.sells.to.both.3, .direction = "downup")

# party.id.before.voter
p_load(dplyr,tidyverse)
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.1.group.id_in_subsession) %>% fill(vote_s.1.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.1 = v.selling.dat$vote_s.1.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.2.group.id_in_subsession) %>% fill(vote_s.2.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.2 = v.selling.dat$vote_s.2.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.3.group.id_in_subsession) %>% fill(vote_s.3.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.3 = v.selling.dat$vote_s.3.player.tipoAoB 


# ideo.distance dyadic
## 1
v.selling.dat$ideo.distance.1.a = abs(v.selling.dat$vote_s.1.group.tipo_votante - v.selling.dat$vote_s.1.group.ubicacion_pA)
v.selling.dat$ideo.distance.1.b = abs(v.selling.dat$vote_s.1.group.tipo_votante - v.selling.dat$vote_s.1.group.ubicacion_pB)
## 2
v.selling.dat$ideo.distance.2.a = abs(v.selling.dat$vote_s.2.group.tipo_votante - v.selling.dat$vote_s.2.group.ubicacion_pA)
v.selling.dat$ideo.distance.2.b = abs(v.selling.dat$vote_s.2.group.tipo_votante - v.selling.dat$vote_s.2.group.ubicacion_pB)
## 3
v.selling.dat$ideo.distance.3.a = abs(v.selling.dat$vote_s.3.group.tipo_votante - v.selling.dat$vote_s.3.group.ubicacion_pA)
v.selling.dat$ideo.distance.3.b = abs(v.selling.dat$vote_s.3.group.tipo_votante - v.selling.dat$vote_s.3.group.ubicacion_pB)


# voter.offer dyadic
## 1
p_load(dplyr,tidyverse)
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.1.group.id_in_subsession) %>% mutate(voter.offer.1.a = max_na(vote_s.1.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.1.group.id_in_subsession) %>% mutate(voter.offer.1.b = max_na(vote_s.1.player.p_oferta_amount_B))
## 2
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.2.group.id_in_subsession) %>% mutate(voter.offer.2.a = max_na(vote_s.2.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.2.group.id_in_subsession) %>% mutate(voter.offer.2.b = max_na(vote_s.2.player.p_oferta_amount_B))
## 3
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.3.group.id_in_subsession) %>% mutate(voter.offer.3.a = max_na(vote_s.3.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code, vote_s.3.group.id_in_subsession) %>% mutate(voter.offer.3.b = max_na(vote_s.3.player.p_oferta_amount_B))


# voter's own party dyadic (0,1)
## 1
v.selling.dat$voter.own.1.a = ifelse(v.selling.dat$vote_s.1.player.votanteOpartido=="Partido A" & v.selling.dat$vote_s.1.player.tipoAoB=="A", 1, 0)
v.selling.dat$voter.own.1.b = ifelse(v.selling.dat$vote_s.1.player.votanteOpartido=="Partido B" & v.selling.dat$vote_s.1.player.tipoAoB=="B", 1, 0)
## 2
v.selling.dat$voter.own.2.a = ifelse(v.selling.dat$vote_s.2.player.votanteOpartido=="Partido A" & v.selling.dat$vote_s.2.player.tipoAoB=="A", 1, 0)
v.selling.dat$voter.own.2.b = ifelse(v.selling.dat$vote_s.2.player.votanteOpartido=="Partido B" & v.selling.dat$vote_s.2.player.tipoAoB=="B", 1, 0)
## 3
v.selling.dat$voter.own.3.a = ifelse(v.selling.dat$vote_s.3.player.votanteOpartido=="Partido A" & v.selling.dat$vote_s.3.player.tipoAoB=="A", 1, 0)
v.selling.dat$voter.own.3.b = ifelse(v.selling.dat$vote_s.3.player.votanteOpartido=="Partido B" & v.selling.dat$vote_s.3.player.tipoAoB=="B", 1, 0)

## 1
p_load(dplyr)
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.1.group.id_in_subsession) %>%
  mutate(voter.own.1.a = max_na(voter.own.1.a))
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.1.group.id_in_subsession) %>%
  mutate(voter.own.1.b = max_na(voter.own.1.b))
## 2
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.2.group.id_in_subsession) %>%
  mutate(voter.own.2.a = max_na(voter.own.2.a))
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.2.group.id_in_subsession) %>%
  mutate(voter.own.2.b = max_na(voter.own.2.b))
## 3
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.3.group.id_in_subsession) %>%
  mutate(voter.own.3.a = max_na(voter.own.3.a))
v.selling.dat <- v.selling.dat %>%
  group_by(session.code, vote_s.3.group.id_in_subsession) %>%
  mutate(voter.own.3.b = max_na(voter.own.3.b))


# pivotal.voter dyad
# Coding matches the oTree vote-counting rule: the real voter is counted inside
# the initial count of the party in his label (tipoAoB), and the vote changes the outcome only if he
# defects. So the voter is pivotal iff his labelled party leads by exactly one (2-1 of 3, 3-2 of 5).
# A 1-2 split is NOT pivotal: defecting only widens the loss. Pivotality is identical for both
# parties (the outcome flips for both). Splits 3-0, 0-3, 5-0, 0-5 never occur (each side >= 1).
## 1A
v.selling.dat$pivotal.voter.1.a = ifelse(
  v.selling.dat$vote_s.1.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.1.player.tipoAoB=="A" & # party ID of voter
    v.selling.dat$vote_s.1.group.n_votantes_A==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.1.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.1.player.tipoAoB=="A" & # party ID of voter
      v.selling.dat$vote_s.1.group.n_votantes_A==2, 1, # vote share of that party
    0
    )
  )
## 1B
v.selling.dat$pivotal.voter.1.b = ifelse(
  v.selling.dat$vote_s.1.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.1.player.tipoAoB=="B" & # party ID of voter
    v.selling.dat$vote_s.1.group.n_votantes_B==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.1.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.1.player.tipoAoB=="B" & # party ID of voter
      v.selling.dat$vote_s.1.group.n_votantes_B==2, 1, # vote share of that party
    0
  )
)
## 2A
v.selling.dat$pivotal.voter.2.a = ifelse(
  v.selling.dat$vote_s.2.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.2.player.tipoAoB=="A" & # party ID of voter
    v.selling.dat$vote_s.2.group.n_votantes_A==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.2.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.2.player.tipoAoB=="A" & # party ID of voter
      v.selling.dat$vote_s.2.group.n_votantes_A==2, 1, # vote share of that party
    0
  )
)
## 2B
v.selling.dat$pivotal.voter.2.b = ifelse(
  v.selling.dat$vote_s.2.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.2.player.tipoAoB=="B" & # party ID of voter
    v.selling.dat$vote_s.2.group.n_votantes_B==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.2.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.2.player.tipoAoB=="B" & # party ID of voter
      v.selling.dat$vote_s.2.group.n_votantes_B==2, 1, # vote share of that party
    0
  )
)
## 3A
v.selling.dat$pivotal.voter.3.a = ifelse(
  v.selling.dat$vote_s.3.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.3.player.tipoAoB=="A" & # party ID of voter
    v.selling.dat$vote_s.3.group.n_votantes_A==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.3.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.3.player.tipoAoB=="A" & # party ID of voter
      v.selling.dat$vote_s.3.group.n_votantes_A==2, 1, # vote share of that party
    0
  )
)
## 3B
v.selling.dat$pivotal.voter.3.b = ifelse(
  v.selling.dat$vote_s.3.group.n_votantes==5 & # number of voters
    v.selling.dat$vote_s.3.player.tipoAoB=="B" & # party ID of voter
    v.selling.dat$vote_s.3.group.n_votantes_B==3, 1, # vote share of that party
  ifelse(
    v.selling.dat$vote_s.3.group.n_votantes==3 & # number of voters
      v.selling.dat$vote_s.3.player.tipoAoB=="B" & # party ID of voter
      v.selling.dat$vote_s.3.group.n_votantes_B==2, 1, # vote share of that party
    0
  )
)


# vote.intention.party dyadic
## 1
v.selling.dat$vote.intention.party.1.a = v.selling.dat$vote_s.1.group.n_votantes_A
v.selling.dat$vote.intention.party.1.b = v.selling.dat$vote_s.1.group.n_votantes_B
## 2
v.selling.dat$vote.intention.party.2.a = v.selling.dat$vote_s.2.group.n_votantes_A
v.selling.dat$vote.intention.party.2.b = v.selling.dat$vote_s.2.group.n_votantes_B
## 3
v.selling.dat$vote.intention.party.3.a = v.selling.dat$vote_s.3.group.n_votantes_A
v.selling.dat$vote.intention.party.3.b = v.selling.dat$vote_s.3.group.n_votantes_B

# % voters
## A
v.selling.dat$vote.intention.party.per.1.a = round((v.selling.dat$vote_s.1.group.n_votantes_A*100)/v.selling.dat$vote_s.1.group.n_votantes,0)
v.selling.dat$vote.intention.party.per.2.a = round((v.selling.dat$vote_s.2.group.n_votantes_A*100)/v.selling.dat$vote_s.2.group.n_votantes,0)
v.selling.dat$vote.intention.party.per.3.a = round((v.selling.dat$vote_s.3.group.n_votantes_A*100)/v.selling.dat$vote_s.3.group.n_votantes,0)
## B
v.selling.dat$vote.intention.party.per.1.b = round((v.selling.dat$vote_s.1.group.n_votantes_B*100)/v.selling.dat$vote_s.1.group.n_votantes,0)
v.selling.dat$vote.intention.party.per.2.b = round((v.selling.dat$vote_s.2.group.n_votantes_B*100)/v.selling.dat$vote_s.2.group.n_votantes,0)
v.selling.dat$vote.intention.party.per.3.b = round((v.selling.dat$vote_s.3.group.n_votantes_B*100)/v.selling.dat$vote_s.3.group.n_votantes,0)



# budget dyadic
v.selling.dat$budget.1.a = v.selling.dat$vote_s.1.group.presupuesto
v.selling.dat$budget.1.b = v.selling.dat$vote_s.1.group.presupuesto
v.selling.dat$budget.2.a = v.selling.dat$vote_s.2.group.presupuesto
v.selling.dat$budget.2.b = v.selling.dat$vote_s.2.group.presupuesto
v.selling.dat$budget.3.a = v.selling.dat$vote_s.3.group.presupuesto
v.selling.dat$budget.3.b = v.selling.dat$vote_s.3.group.presupuesto

# role
v.selling.dat$role.1 = v.selling.dat$vote_s.1.player.votanteOpartido
v.selling.dat$role.2 = v.selling.dat$vote_s.2.player.votanteOpartido
v.selling.dat$role.3 = v.selling.dat$vote_s.3.player.votanteOpartido
p_load(dplyr)
v.selling.dat = v.selling.dat %>% mutate(role.1=recode(role.1, "Partido A" = "Party A","Partido B" = "Party B","votantes" = "Voter"))
v.selling.dat = v.selling.dat %>% mutate(role.2=recode(role.2, "Partido A" = "Party A","Partido B" = "Party B","votantes" = "Voter"))
v.selling.dat = v.selling.dat %>% mutate(role.3=recode(role.3, "Partido A" = "Party A","Partido B" = "Party B","votantes" = "Voter"))

# points
v.selling.dat$points.this.round.1 = v.selling.dat$vote_s.1.player.puntos
v.selling.dat$points.this.round.2 = v.selling.dat$vote_s.2.player.puntos
v.selling.dat$points.this.round.3 = v.selling.dat$vote_s.3.player.puntos


# if party accepts offer from voter
v.selling.dat$accepts.offer.1 = v.selling.dat$vote_s.1.player.votante_acepta_oferta
v.selling.dat$accepts.offer.2 = v.selling.dat$vote_s.2.player.votante_acepta_oferta
v.selling.dat$accepts.offer.3 = v.selling.dat$vote_s.3.player.votante_acepta_oferta
##
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.1.group.id_in_subsession) %>% fill(accepts.offer.1, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.2.group.id_in_subsession) %>% fill(accepts.offer.2, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(session.code, vote_s.3.group.id_in_subsession) %>% fill(accepts.offer.3, .direction = "downup")
###
v.selling.dat$accepts.offer.1.a = ifelse(v.selling.dat$role.1=="Party A" & v.selling.dat$accepts.offer.1==1, 1, 0)
v.selling.dat$accepts.offer.1.b = ifelse(v.selling.dat$role.1=="Party B" & v.selling.dat$accepts.offer.1==2, 1, 0)
v.selling.dat$accepts.offer.2.a = ifelse(v.selling.dat$role.2=="Party A" & v.selling.dat$accepts.offer.2==1, 1, 0)
v.selling.dat$accepts.offer.2.b = ifelse(v.selling.dat$role.2=="Party B" & v.selling.dat$accepts.offer.2==2, 1, 0)
v.selling.dat$accepts.offer.3.a = ifelse(v.selling.dat$role.3=="Party A" & v.selling.dat$accepts.offer.3==1, 1, 0)
v.selling.dat$accepts.offer.3.b = ifelse(v.selling.dat$role.3=="Party B" & v.selling.dat$accepts.offer.3==2, 1, 0)
# NOTE: "accepts.offer" above is the VOTER'S FINAL CHOICE
# (votante_acepta_oferta: 1 = A, 2 = B, 3 = none), not the party's acceptance decision.
# The party's accept/reject decision is player.partido_envia_puntos, recorded on each party's
# own row. It is added below as party.accepts (needed to report rejection rates).
v.selling.dat$party.accepts.1 = as.numeric(v.selling.dat$vote_s.1.player.partido_envia_puntos)
v.selling.dat$party.accepts.2 = as.numeric(v.selling.dat$vote_s.2.player.partido_envia_puntos)
v.selling.dat$party.accepts.3 = as.numeric(v.selling.dat$vote_s.3.player.partido_envia_puntos)

# Ideological compensation required to vote for party i (in points).
# In the game, voter points = D - 20*|x - gamma_winner| (+ transfer): they depend on WHO WINS.
# Hence this compensation is only at stake when the voter is pivotal.
v.selling.dat$delta.req.1.a = pmax(0, v.selling.dat$vote_s.1.group.pje_win_cB - v.selling.dat$vote_s.1.group.pje_win_cA)
v.selling.dat$delta.req.1.b = pmax(0, v.selling.dat$vote_s.1.group.pje_win_cA - v.selling.dat$vote_s.1.group.pje_win_cB)
v.selling.dat$delta.req.2.a = pmax(0, v.selling.dat$vote_s.2.group.pje_win_cB - v.selling.dat$vote_s.2.group.pje_win_cA)
v.selling.dat$delta.req.2.b = pmax(0, v.selling.dat$vote_s.2.group.pje_win_cA - v.selling.dat$vote_s.2.group.pje_win_cB)
v.selling.dat$delta.req.3.a = pmax(0, v.selling.dat$vote_s.3.group.pje_win_cB - v.selling.dat$vote_s.3.group.pje_win_cA)
v.selling.dat$delta.req.3.b = pmax(0, v.selling.dat$vote_s.3.group.pje_win_cA - v.selling.dat$vote_s.3.group.pje_win_cB)

# Consistency between the voter's displayed party label (tipoAoB) and the
# voter's induced ideological payoffs. In the oTree code (models_vs.py / models_vb.py,
# Subsession.get_votantes) the voter label is set with `if g.tipo_votante > 50` AFTER the loop
# over groups, so `g` is the LAST group of the subsession: every voter in a session-round gets
# the label implied by the last group's voter. In the vote-buying data, all 30 session-rounds with
# more than one group show a single label, and ~30% of voters are labelled with the party that
# gives them the LOWER ideological payoff. Flag these dyads for robustness checks.
v.selling.dat$label.consistent.1 = as.numeric(v.selling.dat$vote_s.1.player.tipoAoB == ifelse(v.selling.dat$vote_s.1.group.pje_win_cA >= v.selling.dat$vote_s.1.group.pje_win_cB, "A", "B"))
v.selling.dat$label.consistent.2 = as.numeric(v.selling.dat$vote_s.2.player.tipoAoB == ifelse(v.selling.dat$vote_s.2.group.pje_win_cA >= v.selling.dat$vote_s.2.group.pje_win_cB, "A", "B"))
v.selling.dat$label.consistent.3 = as.numeric(v.selling.dat$vote_s.3.player.tipoAoB == ifelse(v.selling.dat$vote_s.3.group.pje_win_cA >= v.selling.dat$vote_s.3.group.pje_win_cB, "A", "B"))


## 1A
v.selling.dat.1.a = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.1,
    role.1,
    participant.payoff,
    ideo.distance.1.a, 
    vote.intention.party.1.a,
    vote.intention.party.per.1.a,
    voter.offer.1.a,
    voter.sells.to.both.1,
    accepts.offer.1.a,
    pivotal.voter.1.a, 
    voter.own.1.a,
    budget.1.a,
    points.this.round.1,
    party.accepts.1,
    delta.req.1.a,
    label.consistent.1
  )
)
v.selling.dat.1.a <- subset(v.selling.dat.1.a, role.1 == "Party A") # drop voters
colnames(v.selling.dat.1.a)[colnames(v.selling.dat.1.a)=="role.1"] <- "Dyad" # changes name to dyad
v.selling.dat.1.a$Dyad = as.factor(v.selling.dat.1.a$Dyad)
p_load(dplyr)
v.selling.dat.1.a = v.selling.dat.1.a %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.1.a) = sub(".1.*", "", colnames(v.selling.dat.1.a)) # remove extra characters in col names
v.selling.dat.1.a$round = 1

## 1B
v.selling.dat.1.b = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.1,
    role.1,
    participant.payoff,
    ideo.distance.1.b, 
    vote.intention.party.1.b,
    vote.intention.party.per.1.b,
    voter.offer.1.b,
    voter.sells.to.both.1,
    accepts.offer.1.b,
    pivotal.voter.1.b, 
    voter.own.1.b,
    budget.1.b,
    points.this.round.1,
    party.accepts.1,
    delta.req.1.b,
    label.consistent.1
  )
)
v.selling.dat.1.b <- subset(v.selling.dat.1.b, role.1 == "Party B") # drop voters
colnames(v.selling.dat.1.b)[colnames(v.selling.dat.1.b)=="role.1"] <- "Dyad" # changes name to dyad
v.selling.dat.1.b$Dyad = as.factor(v.selling.dat.1.b$Dyad)
p_load(dplyr)
v.selling.dat.1.b = v.selling.dat.1.b %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.1.b) = sub(".1.*", "", colnames(v.selling.dat.1.b)) # remove extra characters in col names
v.selling.dat.1.b$round = 1

## 2A
v.selling.dat.2.a = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.2,
    role.2,
    participant.payoff,
    ideo.distance.2.a, 
    vote.intention.party.2.a,
    vote.intention.party.per.2.a,
    voter.offer.2.a,
    voter.sells.to.both.2,
    accepts.offer.2.a,
    pivotal.voter.2.a, 
    voter.own.2.a,
    budget.2.a,
    points.this.round.2,
    party.accepts.2,
    delta.req.2.a,
    label.consistent.2
  )
)
v.selling.dat.2.a <- subset(v.selling.dat.2.a, role.2 == "Party A") # drop voters
colnames(v.selling.dat.2.a)[colnames(v.selling.dat.2.a)=="role.2"] <- "Dyad" # changes name to dyad
v.selling.dat.2.a$Dyad = as.factor(v.selling.dat.2.a$Dyad)
p_load(dplyr)
v.selling.dat.2.a = v.selling.dat.2.a %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.2.a) = sub(".2.*", "", colnames(v.selling.dat.2.a)) # remove extra characters in col names
v.selling.dat.2.a$round = 2

## 2B
v.selling.dat.2.b = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.2,
    role.2,
    participant.payoff,
    ideo.distance.2.b, 
    vote.intention.party.2.b,
    vote.intention.party.per.2.b,
    voter.offer.2.b,
    voter.sells.to.both.2,
    accepts.offer.2.b,
    pivotal.voter.2.b, 
    voter.own.2.b,
    budget.2.b,
    points.this.round.2,
    party.accepts.2,
    delta.req.2.b,
    label.consistent.2
  )
)
v.selling.dat.2.b <- subset(v.selling.dat.2.b, role.2 == "Party B") # drop voters
colnames(v.selling.dat.2.b)[colnames(v.selling.dat.2.b)=="role.2"] <- "Dyad" # changes name to dyad
v.selling.dat.2.b$Dyad = as.factor(v.selling.dat.2.b$Dyad)
p_load(dplyr)
v.selling.dat.2.b = v.selling.dat.2.b %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.2.b) = sub(".2.*", "", colnames(v.selling.dat.2.b)) # remove extra characters in col names
v.selling.dat.2.b$round = 2

## 3A
v.selling.dat.3.a = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.3,
    role.3,
    participant.payoff,
    ideo.distance.3.a, 
    vote.intention.party.3.a,
    vote.intention.party.per.3.a,
    voter.offer.3.a,
    voter.sells.to.both.3,
    accepts.offer.3.a,
    pivotal.voter.3.a, 
    voter.own.3.a,
    budget.3.a,
    points.this.round.3,
    party.accepts.3,
    delta.req.3.a,
    label.consistent.3
  )
)
v.selling.dat.3.a <- subset(v.selling.dat.3.a, role.3 == "Party A") # drop voters
colnames(v.selling.dat.3.a)[colnames(v.selling.dat.3.a)=="role.3"] <- "Dyad" # changes name to dyad
v.selling.dat.3.a$Dyad = as.factor(v.selling.dat.3.a$Dyad)
p_load(dplyr)
v.selling.dat.3.a = v.selling.dat.3.a %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.3.a) = sub(".3.*", "", colnames(v.selling.dat.3.a)) # remove extra characters in col names
v.selling.dat.3.a$round = 3

## 3B
v.selling.dat.3.b = subset(
  v.selling.dat, select = c(
    participant.code,
    participant.code.dyad.3,
    role.3,
    participant.payoff,
    ideo.distance.3.b, 
    vote.intention.party.3.b,
    vote.intention.party.per.3.b,
    voter.offer.3.b,
    voter.sells.to.both.3,
    accepts.offer.3.b,
    pivotal.voter.3.b, 
    voter.own.3.b,
    budget.3.b,
    points.this.round.3,
    party.accepts.3,
    delta.req.3.b,
    label.consistent.3
  )
)
v.selling.dat.3.b <- subset(v.selling.dat.3.b, role.3 == "Party B") # drop voters
colnames(v.selling.dat.3.b)[colnames(v.selling.dat.3.b)=="role.3"] <- "Dyad" # changes name to dyad
v.selling.dat.3.b$Dyad = as.factor(v.selling.dat.3.b$Dyad)
p_load(dplyr)
v.selling.dat.3.b = v.selling.dat.3.b %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.3.b) = sub(".3.*", "", colnames(v.selling.dat.3.b)) # remove extra characters in col names
v.selling.dat.3.b$round = 3

# stack up 3 games for A abd B dyads
dat.v.s = data.frame(
  rbind(v.selling.dat.1.a,
        v.selling.dat.1.b,
        v.selling.dat.2.a,
        v.selling.dat.2.b,
        v.selling.dat.3.a,
        v.selling.dat.3.b
        )
  )

# Dropping NAs
# drop_na() only on the variables used in the original analysis, so that
# the new auxiliary variables (party.accepts, delta.req, label.consistent) do not change the sample.
dat.v.s = dat.v.s %>% tidyr::drop_na(
  participant.code, participant.code.dyad, Dyad, participant.payoff, ideo.distance,
  vote.intention.party, vote.intention.party.per, voter.offer, voter.sells.to.both,
  accepts.offer, pivotal.voter, voter.own, budget, points.this.round)

# derived variables
dat.v.s$delta.req.p = (dat.v.s$delta.req * 100) / dat.v.s$budget  # required compensation, % of budget
dat.v.s$marginal.value = 2400 * dat.v.s$pivotal.voter              # Constants.E x change in Pr(win): the vote is worth E to EITHER party iff pivotal, 0 otherwise

# change in payoff
dat.v.s = dat.v.s %>%
  group_by(participant.code) %>%
  mutate(points.cumul.delta = points.this.round - lag(points.this.round))



# plotting dep variable plot BY HAND
p_load(gridExtra,lattice)
m1.dep.var = histogram(~dat.v.s$voter.offer, 
                       aspect = 1,
                       xlab = "Amount of Vote-Buying Demand (points)"
)


# pivotal to factor
dat.v.s$pivotal.voter = as.factor(dat.v.s$pivotal.voter)

# voter.offer to %
dat.v.s$voter.offer.p = (dat.v.s$voter.offer*100)/dat.v.s$budget

# load vote buying df from paper 1 manipulations
load("/Users/hectorbahamonde/research/Exp_Vote_Selling/data/vote_buying_df.RData")
## ----

## ---- hyp.one.vote.selling

# Dependent variable: percentage of budget offered
dat.v.b$offer.p <- (dat.v.b$offer.made.party * 100) / dat.v.b$budget

# Sanity check: distribution
# table(dat.v.b$offer.made.party)

# Because offers are mostly 0 or Δ, we use:
# (1) A logit model for any offer (party rows only; voter rows have NA offers and drop out: N = 204)
# (2) A linear model for the size of positive offers (N = 125)

# 1. Probability a party makes ANY offer > 0
dat.v.b$any_offer <- ifelse(dat.v.b$offer.made.party > 0, 1, 0)

p_load(sandwich, ggeffects, ggplot2)

## logit model
m_H1_prob <- glm(
  any_offer ~ ideo.distance2 + vote.intention.party.2 + pivotal.voter,
  family = binomial(link = "logit"),
  data   = dat.v.b
)

## clustered vcov (same as in the table)
vcov_logit <- sandwich::vcovCL(
  m_H1_prob,
  cluster = ~ participant.code,  # party participant (repeated across rounds)
  type    = "HC1"
)

## predictions with clustered SEs
pred_H1 <- ggpredict(
  m_H1_prob,
  terms = "ideo.distance2 [all]",
  vcov  = vcov_logit
)

p_load(sjPlot)
theme_set(theme_sjplot())  # keep your sjPlot look

logit_color <- "#B2182B"   # or whatever red you're using for the OLS plot

H1plot <- ggplot(pred_H1, aes(x = x, y = predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill  = logit_color,
    alpha = 0.2
  ) +
  geom_line(color = logit_color, linewidth = 1) +
  labs(
    title = "Effect of Ideological Distance on\nProbability of Any Vote-Buying Offer (logit)",
    x     = "Ideological distance",
    y     = "Predicted probability of any offer"
  ) +
  theme(
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_blank()
  )


pdf(
  file = file.path(tempdir(), "H1_ideo_distance_plot.pdf"),
  #type      = "cairo",
  #units     = "in",
  width     = 5.5,
  height    = 5#,
  #pointsize = 10,
  #res       = 600
)

print(H1plot)
dev.off()

# 2. Among those who offer >0, does ideological proximity predict offer size?
vb_pos <- subset(dat.v.b, offer.made.party > 0)

m_H1_size <- lm(
  offer.p ~ ideo.distance2 + vote.intention.party.2 + pivotal.voter,
  data = vb_pos
)

#summary(m_H1_size)

p_load(sandwich, sjPlot, ggeffects, ggplot2)

# The previous helper used model$model$participant.code, but model$model only
# contains the variables in the formula, so the cluster was NULL and sandwich::vcovCL() silently
# returned HC1 (unclustered) standard errors. Cluster explicitly on the party participant.
vcov_size <- sandwich::vcovCL(m_H1_size, cluster = ~ participant.code, type = "HC1")

## predictions with clustered SEs (95% CIs, as stated in the Figure caption)
pred_H1_size <- ggpredict(
  m_H1_size,
  terms = "ideo.distance2 [all]",
  vcov  = vcov_size
)

ols_color <- "#2166AC"

make_H1_size_plot <- function(pred) {
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = ols_color, alpha = 0.2) +
    geom_line(color = ols_color, linewidth = 1) +
    labs(
      title = "Effect of Ideological Distance on\nSize of Vote-Buying Offer (OLS)",
      x     = "Ideological distance",
      y     = "Predicted size of offer (% of budget)"
    ) +
    theme(
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.background = element_blank(),
      aspect.ratio     = 1
    )
}

H1plot_2    <- make_H1_size_plot(pred_H1_size)
H1plot_2_sq <- make_H1_size_plot(pred_H1_size)

p_load(gridExtra)

# Create combined PNG with SIZE on the left and PROBABILITY on the right
pdf(
  file = file.path(tempdir(), "H1_combined.pdf"),
  #type      = "cairo",
  #units     = "in",
  width     = 10,
  height    = 4.5#,
  #res       = 600
)

grid.arrange(
  H1plot,    # <-- PROBABILITY SECOND (right)
  H1plot_2,  # <-- SIZE FIRST (left)
  ncol = 2
)

dev.off()


# Create just the ols plot for presentation
pdf(
  file = file.path(tempdir(), "H1_ols_presentation.pdf"),
  #type      = "cairo",
  #units     = "in",
  width     = 5,
  height    = 5#,
  #res       = 600
)


print(H1plot_2_sq)
dev.off()
## ----

## ---- dep.var
# plotting dep variable plot BY HAND
p_load(gridExtra, lattice)

# Vote-selling dependent variable
m1.dep.var <- histogram(
  ~ dat.v.s$voter.offer.p,
  aspect = 1,
  breaks = 20,
  xlab   = "Vote-Selling Prices (voters, % of party budget)",
  ylab   = "Percent of observations"  # y-axis is the share of observations
)

# Vote-buying dependent variable
m2.dep.var <- histogram(
  ~ dat.v.b$offer.made.party.p,
  aspect = 1,
  breaks = 20,
  nint  = 7,
  xlab   = "Vote-Buying Offers (parties, % of party budget)",
  ylab   = "Percent of observations"
)

# Save 2-panel figure
pdf(file = "/Users/hectorbahamonde/research/Exp_Vote_Selling/depvarplot.pdf",
    #type      = "cairo",
    #units     = "in",
    width     = 8,     # wider for two plots
    height    = 4.5#,
    #pointsize = 10,
    #res       = 600
    )

grid.arrange(m2.dep.var, m1.dep.var, ncol = 2)

dev.off()
## ----

## ---- models
m1 = lm(voter.offer.p ~ ideo.distance*vote.intention.party.per + pivotal.voter, dat.v.s)

# Clustered vcov on the VOTER (participant.code.dyad): each voter submits
# requests to two parties in each round they are the voter.
p_load(sandwich)
vcov_m1 <- sandwich::vcovCL(m1, cluster = ~ participant.code.dyad, type = "HC1")

# Marginal effect of ideological distance at each observed vote share
#: dY/dIdeology = b1 + b3 * VoteShare, with clustered SEs, plus a formal test that
# the slopes differ between weak (20%) and strong (80%) parties.
me_ideology <- function(model, V, at) {
  b  <- coef(model)
  i1 <- "ideo.distance"; i3 <- "ideo.distance:vote.intention.party.per"
  est <- b[i1] + at * b[i3]
  se  <- sqrt(V[i1, i1] + at^2 * V[i3, i3] + 2 * at * V[i1, i3])
  df.res <- model$df.residual
  data.frame(
    vote.share = at,
    slope      = as.numeric(est),
    se         = as.numeric(se),
    ci.low     = as.numeric(est - qt(0.975, df.res) * se),
    ci.high    = as.numeric(est + qt(0.975, df.res) * se),
    p.value    = as.numeric(2 * pt(-abs(est / se), df.res))
  )
}
vote.shares.observed <- sort(unique(dat.v.s$vote.intention.party.per))
H2_marginal_effects  <- me_ideology(m1, vcov_m1, vote.shares.observed)

# Difference in slopes, strong (80) minus weak (20): 60 * b3
b3    <- coef(m1)["ideo.distance:vote.intention.party.per"]
se.b3 <- sqrt(vcov_m1["ideo.distance:vote.intention.party.per", "ideo.distance:vote.intention.party.per"])
H2_slope_diff <- data.frame(
  diff    = as.numeric(60 * b3),
  se      = as.numeric(60 * se.b3),
  p.value = as.numeric(2 * pt(-abs(b3 / se.b3), m1$df.residual))
)

# Robustness: dyads where the voter's displayed party label matches the
# party that gives the voter the higher ideological payoff (see oTree labelling bug above).
m1_consistent <- lm(voter.offer.p ~ ideo.distance*vote.intention.party.per + pivotal.voter,
                    data = subset(dat.v.s, label.consistent == 1))

# Alternative electoral-stake measure:
# marginal value of the vote = E x change in Pr(win) = 2400 x pivotal, identical for both parties.
m1_marginal <- lm(voter.offer.p ~ ideo.distance*marginal.value, data = dat.v.s)
## ----

## ---- flow.descriptives
# Observation flow and descriptives.
flow_table <- data.frame(
  Step = c(
    "Participant rows in raw data",
    "Dropped: participant payoff <= 0",
    "Vote-selling: participants",
    "Vote-selling: sessions",
    "Vote-selling: party-dyad observations analysed (requested-price model)",
    "Vote-selling: distinct voters in requested-price model",
    "Vote-buying: participants",
    "Vote-buying: sessions",
    "Vote-buying: party-dyad observations (any-offer logit)",
    "Vote-buying: positive offers (offer-size OLS)"
  ),
  N = c(
    n.participants.raw,
    n.participants.dropped.zero.payoff,
    length(unique(v.selling.dat$participant.code)),
    length(unique(v.selling.dat$session.code)),
    nobs(m1),
    length(unique(dat.v.s$participant.code.dyad)),
    length(unique(dat.v.b$participant.code)),
    length(unique(dat.v.b$session.code)),
    sum(!is.na(dat.v.b$offer.made.party)),
    sum(dat.v.b$offer.made.party > 0, na.rm = TRUE)
  )
)

# Pricing behaviour in the vote-selling game (party-dyad level)
vs_desc <- with(dat.v.s, data.frame(
  Statistic = c(
    "Requests equal to the full party budget (%)",
    "Requests of zero to the voter's non-labelled party (%)",
    "Requests of zero to the party with lower ideological payoff (%)",
    "Pivotal dyads: requests below required ideological compensation (%)",
    "  ... of which requests of zero (%)",  # zero requests have no accept/reject, so an acceptance rate is undefined here
    "Party accepted the request (%)",
    "Party accepted, pivotal dyads (%)",
    "Party accepted, non-pivotal dyads (%)",
    "Voter label consistent with induced ideology (%)"
  ),
  Value = round(100 * c(
    mean(voter.offer >= budget, na.rm = TRUE),
    mean(voter.offer[voter.own == 0] == 0, na.rm = TRUE),
    mean(voter.offer[delta.req > 0] == 0, na.rm = TRUE),
    mean(voter.offer[pivotal.voter == 1 & delta.req > 0] < delta.req[pivotal.voter == 1 & delta.req > 0], na.rm = TRUE),
    mean(voter.offer[pivotal.voter == 1 & delta.req > 0 & voter.offer < delta.req] == 0, na.rm = TRUE),
    mean(party.accepts == 1, na.rm = TRUE),
    mean(party.accepts[pivotal.voter == 1] == 1, na.rm = TRUE),
    mean(party.accepts[pivotal.voter == 0] == 1, na.rm = TRUE),
    mean(label.consistent == 1, na.rm = TRUE)
  ), 1)
))
# Note: requests above the budget are impossible (pages_vs.py rejects them with an error message).

# Same benchmark for the vote-buying game: offers when the vote cannot change the outcome
vb_desc <- with(subset(dat.v.b, !is.na(offer.made.party)), data.frame(
  pivotal          = c(0, 1),
  n                = c(sum(pivotal.voter == 0), sum(pivotal.voter == 1)),
  share.any.offer  = round(100 * c(mean(offer.made.party[pivotal.voter == 0] > 0), mean(offer.made.party[pivotal.voter == 1] > 0)), 1),
  mean.offer.pct   = round(c(mean(offer.made.party.p[pivotal.voter == 0]), mean(offer.made.party.p[pivotal.voter == 1])), 1)
))
## ----

## ---- int.plot.d
p_load(sjPlot, sjmisc, ggplot2)
theme_set(theme_sjplot())

# plot_model(robust = TRUE) is no longer supported by sjPlot, so the previous
# CIs were not cluster-robust despite the caption. Use ggpredict with the voter-clustered vcov.
# 95% CIs (update the Figure caption accordingly).
pred_m1 <- ggeffects::ggpredict(
  m1,
  terms = c("ideo.distance [all]", "vote.intention.party.per [20, 80]"),
  vcov  = vcov_m1
)

m1plot <- ggplot(pred_m1, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  labs(
    title  = "Partial Conditional Effect of Ideological Distance and\nVote Share On Vote-Selling Prices Made by Voters",
    x      = "Ideological Distance",
    y      = "Predicted Vote-Selling Price\nRequested by Voter (% of budget)",
    colour = "Vote Share (%)", fill = "Vote Share (%)"
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_blank()
  )

pdf(
  file = file.path(tempdir(), "m1plot.pdf"),
  #type = "cairo",
  #units = "in",
  width = 5.5,
  height = 5#,
  #pointsize = 10,
  #res = 600
)

print(m1plot)
dev.off()
## ----

## ---- payoffs.plot
# PAYOFF MEASURE CORRECTED.
# The previous version used dat$participant.payoff, which oTree defines as the participant's TOTAL
# payoff summed over all rounds (and roles) of the session (in vote_buying_df.RData,
# payoff = 5/12 x sum of the three rounds' points for every participant). A participant who was
# the voter in round 1 and a party in rounds 2-3 therefore contributed a "voter payoff" that was
# mostly party earnings, and the same total was repeated once per round in which they were a voter.
# We now use the ROUND-LEVEL points of the role actually played in that round (player.puntos).
build_payoffs <- function(dat, app, game.label) {
  out <- lapply(1:3, function(k) {
    role.col <- paste0(app, ".", k, ".player.votanteOpartido")
    pts.col  <- paste0(app, ".", k, ".player.puntos")
    if (!all(c(role.col, pts.col) %in% names(dat))) {
      warning("Missing columns for ", app, " round ", k); return(NULL)
    }
    data.frame(
      participant.code = dat$participant.code,
      session.code     = dat$session.code,
      round            = k,
      Game             = game.label,
      Role             = ifelse(dat[[role.col]] == "votantes", "Voters", "Parties"),
      Payoff           = as.numeric(dat[[pts.col]]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  subset(out, !is.na(Role) & !is.na(Payoff))
}

payoff.long <- rbind(
  build_payoffs(dat, "vote_b", "Vote Buying"),
  build_payoffs(dat, "vote_s", "Vote Selling")
)

# Is the institutional variant assigned at the session level (between-subject)?
sessions.by.game <- tapply(payoff.long$Game, payoff.long$session.code, function(g) length(unique(g)))
design.between.sessions <- all(sessions.by.game == 1)
message("Institutional variant assigned between sessions: ", design.between.sessions)

# Means and CIs clustered at the level of treatment assignment (session) -- 90% and 95%
p_load(sandwich)
cl.var <- if (design.between.sessions) "session.code" else "participant.code"
payoff_means <- function(df) {
  m  <- lm(Payoff ~ 0 + Game, data = df)
  V  <- sandwich::vcovCL(m, cluster = df[[cl.var]], type = "HC1")
  se <- sqrt(diag(V))
  data.frame(
    Game     = sub("^Game", "", names(coef(m))),
    Payoff   = as.numeric(coef(m)),
    SE       = as.numeric(se),
    Lower    = as.numeric(coef(m) - qnorm(0.95) * se),
    Upper    = as.numeric(coef(m) + qnorm(0.95) * se),
    Lower95  = as.numeric(coef(m) - qnorm(0.975) * se),
    Upper95  = as.numeric(coef(m) + qnorm(0.975) * se),
    N.obs    = as.numeric(table(df$Game)[sub("^Game", "", names(coef(m)))]),
    N.people = as.numeric(tapply(df$participant.code, df$Game, function(x) length(unique(x)))[sub("^Game", "", names(coef(m)))]),
    N.sessions = as.numeric(tapply(df$session.code, df$Game, function(x) length(unique(x)))[sub("^Game", "", names(coef(m)))])
  )
}
payoffs.d <- rbind(
  cbind(payoff_means(subset(payoff.long, Role == "Voters")),  Role = "Voters"),
  cbind(payoff_means(subset(payoff.long, Role == "Parties")), Role = "Parties")
)

# plot
p_load(ggplot2)

payoffs.p <- ggplot(
  payoffs.d,
  aes(
    Game,
    y = Payoff,
    ymin = Lower,
    ymax = Upper
  )
) +
  geom_pointrange(size = 1.5) +
  facet_wrap(~ Role) +
  ylab("Round payoff (points)") + xlab("") +
  theme(
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 3),
    legend.key.size = unit(0.9, "cm"),
    legend.spacing.x = unit(0.7, "cm"),
    strip.text.x = element_text(size = 15),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_blank()
  )

pdf(
  file = file.path(tempdir(), "payoffs_plot.pdf"),
  width = 9,
  height = 5
)

print(payoffs.p)   # << required for knitr + device

dev.off()
## ----

## ----ttest_payoffs
# Difference in voter round payoffs (Vote Selling - Vote Buying).
# (i) OLS with SEs clustered at the level of treatment assignment, two-sided;
# (ii) randomization inference that permutes the game label across sessions (valid with few
#      clusters when the variant is assigned by session).
diff_test <- function(df, R = 5000, seed = 2026) {
  df$vs <- as.numeric(df$Game == "Vote Selling")
  m  <- lm(Payoff ~ vs, data = df)
  V  <- sandwich::vcovCL(m, cluster = df[[cl.var]], type = "HC1")
  est <- as.numeric(coef(m)["vs"]); se <- sqrt(V["vs", "vs"])
  p.cl <- 2 * pnorm(-abs(est / se))
  p.ri <- NA_real_
  if (design.between.sessions) {
    set.seed(seed)
    s.lab <- unique(df[, c("session.code", "Game")])
    sims <- replicate(R, {
      perm <- sample(s.lab$Game)
      g <- perm[match(df$session.code, s.lab$session.code)]
      mean(df$Payoff[g == "Vote Selling"]) - mean(df$Payoff[g == "Vote Buying"])
    })
    p.ri <- mean(abs(sims) >= abs(est))
  }
  list(estimate = est, se = se, p.value = p.cl, p.value.ri = p.ri,
       n.obs = nrow(df), n.clusters = length(unique(df[[cl.var]])))
}
t.test.payoffs        <- diff_test(subset(payoff.long, Role == "Voters"))
t.test.payoffs.party  <- diff_test(subset(payoff.long, Role == "Parties"))
## ----

## ---- reg:table
p_load(modelsummary, sandwich)

## list of models
models_H1 <- list(
  "Offer size (OLS)"      = m_H1_size,
  "Any offer (logit)"     = m_H1_prob,
  "Requested price (OLS)" = m1
)

## Clustered SEs. The previous helper used model$model$participant.code,
## which is NULL (model$model holds only formula variables), so the table reported UNCLUSTERED
## HC1 errors labelled "Clustered" (e.g., offer-size ideology SE 0.103 = HC1; party-clustered
## SE is 0.115). Supply each model's clustered vcov explicitly:
##   vote-buying models: cluster = party participant; vote-selling model: cluster = voter.
vcov_list_H1 <- list(
  sandwich::vcovCL(m_H1_size, cluster = ~ participant.code,      type = "HC1"),
  sandwich::vcovCL(m_H1_prob, cluster = ~ participant.code,      type = "HC1"),
  vcov_m1
)
n_clusters_H1 <- c(
  length(unique(vb_pos$participant.code)),
  length(unique(dat.v.b$participant.code[!is.na(dat.v.b$any_offer)])),
  length(unique(dat.v.s$participant.code.dyad))
)
rows_H1 <- data.frame(
  term = c("Clustered by", "Number of clusters"),
  m1 = c("Party participant", n_clusters_H1[1]),
  m2 = c("Party participant", n_clusters_H1[2]),
  m3 = c("Voter participant", n_clusters_H1[3])
)

## map coefficient names to paper terminology
coef_map <- c(
  "(Intercept)"                            = "Constant",
  "ideo.distance2"                         = "Ideological distance",
  "vote.intention.party.2"                 = "Vote share",
  "pivotal.voter"                          = "Pivotal voter",
  "ideo.distance"                          = "Ideological distance",
  "vote.intention.party.per"               = "Vote share",
  "pivotal.voter1"                         = "Pivotal voter",
  "ideo.distance:vote.intention.party.per" = "Ideology $\\times$ vote share"
)

## build LaTeX *tabular* and store as character vector
H1_table_latex <- modelsummary(
  models_H1,
  vcov      = vcov_list_H1,      # clustered SEs (see above)
  coef_map  = coef_map,
  stars     = c("+" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  add_rows  = rows_H1,
  gof_omit  = "IC|RMSE",
  escape    = FALSE,
  output    = "latex_tabular"    # <- raw tabular, not tinytable/S4
)

## change "Custom" to "Clustered (participant)" in the Std.Errors row
H1_table_latex <- gsub(
  "Custom",
  "Clustered",
  H1_table_latex,
  fixed = TRUE
)

## Table notes. Print below the table in the .rnw.
H1_table_notes <- paste0(
  "Notes: Cluster-robust standard errors (HC1) in parentheses. ",
  "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001 (two-sided). ",
  "Columns 1--2 use the vote-buying game: the unit is a party--voter dyad, and only party rows enter (",
  sum(!is.na(dat.v.b$any_offer)), " party decisions: ", length(unique(dat.v.b$participant.code)),
  " participants $\\times$ 3 rounds, two of every three roles being parties); column 1 is restricted to strictly positive offers (",
  nobs(m_H1_size), "). Column 3 uses the vote-selling game: one observation per voter request to each party (",
  nobs(m1), " after listwise deletion). Vote share is the party's initial share of the electorate (\\%); ",
  "ideological distance is $|x_j - \\gamma_i|$ on the 1--100 scale."
)
## ----

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
