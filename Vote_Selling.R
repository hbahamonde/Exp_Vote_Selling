##############################################################################################
# Vote_Selling.R -- data preparation and descriptive analyses for
#   "Who Sets the Price of a Vote?" (Vote_Selling_Bahamonde_Canales_Paper_3.rnw).
# Benchmark tests are in Vote_Selling_revision_tests.R. Data in data/; oTree code in experiment/.
# The previous version of this script is in old/Vote_Selling.R.
##############################################################################################

############################## 
# Cleaning
##############################

## ---- loadings:d:2 ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Exp_Vote_Selling/")


# K-adic Data Analyses
# https://www.paulpoast.com/stata-software/4587316769

# [REVISION 2026-11] max() that ignores NAs but returns NA (not -Inf) when all values are NA.
# Used for group-level aggregation: voter requests are recorded on the voter's row only.
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# [REVISION 2026-11] NOTE ON GROUP IDENTIFIERS
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
# [REVISION 2026-11] Prefer the local clone of the data repo; fall back to GitHub.
local.data <- "/Users/hectorbahamonde/research/Exp_Vote_Selling/data/data.xlsx"
dat <- if (file.exists(local.data)) rio::import(local.data) else
  rio::import("https://github.com/hbahamonde/Economic_Experiment_Vote_Selling/raw/master/data/data.xlsx")

# dropping obs
# drop if payoff is 0
# [REVISION 2026-11] report how many participants this drops (needed for the flow table).
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
# [REVISION 2026-11] Coding matches the oTree vote-counting rule: the real voter is counted inside
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
# [REVISION 2026-11] NOTE: "accepts.offer" above is the VOTER'S FINAL CHOICE
# (votante_acepta_oferta: 1 = A, 2 = B, 3 = none), not the party's acceptance decision.
# The party's accept/reject decision is player.partido_envia_puntos, recorded on each party's
# own row. It is added below as party.accepts (needed to report rejection rates).
v.selling.dat$party.accepts.1 = as.numeric(v.selling.dat$vote_s.1.player.partido_envia_puntos)
v.selling.dat$party.accepts.2 = as.numeric(v.selling.dat$vote_s.2.player.partido_envia_puntos)
v.selling.dat$party.accepts.3 = as.numeric(v.selling.dat$vote_s.3.player.partido_envia_puntos)

# [REVISION 2026-11] Ideological compensation required to vote for party i (in points).
# In the game, voter points = D - 20*|x - gamma_winner| (+ transfer): they depend on WHO WINS.
# Hence this compensation is only at stake when the voter is pivotal.
v.selling.dat$delta.req.1.a = pmax(0, v.selling.dat$vote_s.1.group.pje_win_cB - v.selling.dat$vote_s.1.group.pje_win_cA)
v.selling.dat$delta.req.1.b = pmax(0, v.selling.dat$vote_s.1.group.pje_win_cA - v.selling.dat$vote_s.1.group.pje_win_cB)
v.selling.dat$delta.req.2.a = pmax(0, v.selling.dat$vote_s.2.group.pje_win_cB - v.selling.dat$vote_s.2.group.pje_win_cA)
v.selling.dat$delta.req.2.b = pmax(0, v.selling.dat$vote_s.2.group.pje_win_cA - v.selling.dat$vote_s.2.group.pje_win_cB)
v.selling.dat$delta.req.3.a = pmax(0, v.selling.dat$vote_s.3.group.pje_win_cB - v.selling.dat$vote_s.3.group.pje_win_cA)
v.selling.dat$delta.req.3.b = pmax(0, v.selling.dat$vote_s.3.group.pje_win_cA - v.selling.dat$vote_s.3.group.pje_win_cB)

# [REVISION 2026-11] Consistency between the voter's displayed party label (tipoAoB) and the
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
    party.accepts.1,      # [REVISION 2026-11]
    delta.req.1.a,      # [REVISION 2026-11]
    label.consistent.1    # [REVISION 2026-11]
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
    party.accepts.1,      # [REVISION 2026-11]
    delta.req.1.b,      # [REVISION 2026-11]
    label.consistent.1    # [REVISION 2026-11]
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
    party.accepts.2,      # [REVISION 2026-11]
    delta.req.2.a,      # [REVISION 2026-11]
    label.consistent.2    # [REVISION 2026-11]
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
    party.accepts.2,      # [REVISION 2026-11]
    delta.req.2.b,      # [REVISION 2026-11]
    label.consistent.2    # [REVISION 2026-11]
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
    party.accepts.3,      # [REVISION 2026-11]
    delta.req.3.a,      # [REVISION 2026-11]
    label.consistent.3    # [REVISION 2026-11]
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
    party.accepts.3,      # [REVISION 2026-11]
    delta.req.3.b,      # [REVISION 2026-11]
    label.consistent.3    # [REVISION 2026-11]
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
# [REVISION 2026-11] drop_na() only on the variables used in the original analysis, so that
# the new auxiliary variables (party.accepts, delta.req, label.consistent) do not change the sample.
dat.v.s = dat.v.s %>% tidyr::drop_na(
  participant.code, participant.code.dyad, Dyad, participant.payoff, ideo.distance,
  vote.intention.party, vote.intention.party.per, voter.offer, voter.sells.to.both,
  accepts.offer, pivotal.voter, voter.own, budget, points.this.round)

# [REVISION 2026-11] derived variables
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


############################################################
# *************   MODELING H1: CORE TARGETING   ************
############################################################

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

# [REVISION 2026-11] The previous helper used model$model$participant.code, but model$model only
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

######################################################################### 
# **************            D  E P   V A R                 **************
#########################################################################

## ---- dep.var
# plotting dep variable plot BY HAND
p_load(gridExtra, lattice)

# Vote-selling dependent variable
m1.dep.var <- histogram(
  ~ dat.v.s$voter.offer.p,
  aspect = 1,
  breaks = 20,
  xlab   = "Vote-Selling Prices (voters, % of party budget)",
  ylab   = "Percent of observations"  # [REVISION 2026-11] y-axis is the share of observations
)

# Vote-buying dependent variable
m2.dep.var <- histogram(
  ~ dat.v.b$offer.made.party.p,
  aspect = 1,
  breaks = 20,
  nint  = 7,
  xlab   = "Vote-Buying Offers (parties, % of party budget)",
  ylab   = "Percent of observations"  # [REVISION 2026-11]
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

######################################################################### 
# ************** M      O       D       E       L       S **************
#########################################################################

## ---- models
m1 = lm(voter.offer.p ~ ideo.distance*vote.intention.party.per + pivotal.voter, dat.v.s)

# [REVISION 2026-11] Clustered vcov on the VOTER (participant.code.dyad): each voter submits
# requests to two parties in each round they are the voter.
p_load(sandwich)
vcov_m1 <- sandwich::vcovCL(m1, cluster = ~ participant.code.dyad, type = "HC1")

# [REVISION 2026-11] Marginal effect of ideological distance at each observed vote share
# (Reviewer 2): dY/dIdeology = b1 + b3 * VoteShare, with clustered SEs, plus a formal test that
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

# [REVISION 2026-11] Robustness: dyads where the voter's displayed party label matches the
# party that gives the voter the higher ideological payoff (see oTree labelling bug above).
m1_consistent <- lm(voter.offer.p ~ ideo.distance*vote.intention.party.per + pivotal.voter,
                    data = subset(dat.v.s, label.consistent == 1))

# [REVISION 2026-11] Alternative electoral-stake measure suggested by Reviewer 2:
# marginal value of the vote = E x change in Pr(win) = 2400 x pivotal, identical for both parties.
m1_marginal <- lm(voter.offer.p ~ ideo.distance*marginal.value, data = dat.v.s)
## ----

## ---- flow.descriptives
# [REVISION 2026-11] Observation flow and descriptives requested by Reviewer 2 (comment 4).
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
    "  ... of which requests of zero (%)",  # [REVISION 2026-11] zero requests have no accept/reject, so an acceptance rate is undefined here
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

options(scipen=999)
summary(m1)

# Clustered Std Errors and Model info
# options(scipen=9999999) # turn off sci not
vcov <- vcovCL(m1, 
               cluster=dat.v.s$participant.code.dyad,
               multi0 = TRUE,
               cadjust = TRUE,
               type = "HC1",
               sandwich = TRUE
               )
p_load(sandwich,lmtest,lattice,latticeExtra)  # [REVISION 2026-11] DAMisc dropped: unused, not on CRAN for R 4.6
coeftest(m1, vcov. = vcovCL(m1, cluster = dat.v.s$participant.code.dyad, type = "HC1"))
# m1.clst.std.err = as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = dat.v.s$participant.code, type = "HC0"))[,2])[1:6]
# m1.clst.t.test = c(as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = dat.v.s$participant.code.dyad, type = "HC0"))[,3])[1:6])
# m1.clst.p.value = c(as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = dat.v.s$participant.code.dyad, type = "HC0"))[,4])[1:6])
# custom.model.names.m1 = "Amount of Vote-Buying Offer"



# p_load(effects)
# plot(effects::effect("ideo.distance*vote.intention.party.per", m1, confidence.level = 0.90),
# ylab="Predicted Amount of Vote-Selling Offer\nMade by Voter (points)",
#      xlab="Ideological Distance",
#      main = "Partial Conditional Effect of Ideological Distance and Vote Share\nOn Vote-Selling Offer Made Voters",
#      aspect = 1,
#      layout = c(5, 2)
#      )

# with panel corrected std errors
# p_load(ggeffects)
# plot(ggeffects::ggpredict(
#   model=m1,
#   terms=c("ideo.distance", "vote.intention.party.per [20, 80]"), 
#   vcov.fun = "vcovHC", 
#   vcov.type = "HC0")
# )


# p_load(effects)
# plot(predictorEffects(m1))


## Additional Interaction Stuff
### 1
# p_load(DAMisc)
# DAintfun2(m1, c("vote.intention.party.per", "ideo.distance"), varcov = vcov, hist=T, scale.hist=.3, level = 0.90) # plot.type="pdf"
# BGMtest(m1, vars=c("vote.intention.party.per", "ideo.distance"))
# DAintfun(m1, c("vote.intention.party.per", "ideo.distance"), theta=-45, phi=20)
### 2

## ---- int.plot.d
p_load(sjPlot, sjmisc, ggplot2)
theme_set(theme_sjplot())

# [REVISION 2026-11] plot_model(robust = TRUE) is no longer supported by sjPlot, so the previous
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




######################################################################### 
# ************** p  a y o f f s  **************
#########################################################################


## ---- payoffs.plot
# [REVISION 2026-11] PAYOFF MEASURE CORRECTED.
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
# [REVISION 2026-11] Difference in voter round payoffs (Vote Selling - Vote Buying).
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


#######################
#### META: Histogram
#######################

## ----meta_analyses
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(tidyverse,readxl)

# Load the data
meta.d.v.b = read_excel("/Users/hectorbahamonde/research/Exp_Vote_Selling/data/vote_buying_selling_meta.xlsx", sheet = "VB-WOS-PolSci")
meta.d.v.s = read_excel("/Users/hectorbahamonde/research/Exp_Vote_Selling/data/vote_buying_selling_meta.xlsx", sheet = "VS-WOS-PoliSci")

# rbind
meta.d = rbind(meta.d.v.b, meta.d.v.s)

# relabel
meta.d$Literature <- gsub("Vote-Buying", "Vote Buying", meta.d$Literature)
meta.d$Literature <- gsub("Vote-Selling", "Vote Selling", meta.d$Literature)

# plot
histogram.meta.plot <- ggplot(meta.d, aes(x = Year, fill = Literature)) +
  geom_histogram(binwidth = 1, colour = "white") +
  ylab("Frequency") + xlab("Year of Publication") +
  theme_bw() +
  theme(
    axis.text.y  = element_text(size = 15), 
    axis.text.x  = element_text(size = 15), 
    axis.title.y = element_text(size = 15), 
    axis.title.x = element_text(size = 15), 
    legend.text  = element_text(size = 15), 
    legend.title = element_text(size = 0),
    plot.title   = element_text(size = 3),
    legend.position   = "bottom",
    legend.key.size   = unit(0.9, "cm"),
    legend.spacing.x  = unit(0.7, "cm"),
    strip.text.x      = element_text(size = 15),
    aspect.ratio      = 1        # <- square panel
  )


pdf(file=file.path(tempdir(), "histogram_meta_plot.pdf"), 
    #type="cairo",
    #units="in", 
    width=9, 
    height=5#, 
    #pointsize=10, 
    #res=600
    )

print(histogram.meta.plot)
dev.off()

# wide plot for presentation
# plot
histogram.meta.plot.wide <- ggplot(meta.d, aes(x = Year, fill = Literature)) +
  geom_histogram(binwidth = 1, colour = "white") +
  ylab("Frequency") + xlab("Year of Publication") +
  theme_bw() +
  theme(
    axis.text.y  = element_text(size = 15), 
    axis.text.x  = element_text(size = 15), 
    axis.title.y = element_text(size = 15), 
    axis.title.x = element_text(size = 15), 
    legend.text  = element_text(size = 15), 
    legend.title = element_text(size = 0),
    plot.title   = element_text(size = 3),
    legend.position   = "bottom",
    legend.key.size   = unit(0.9, "cm"),
    legend.spacing.x  = unit(0.7, "cm"),
    strip.text.x      = element_text(size = 15)
  )


pdf(file="/Users/hectorbahamonde/research/Exp_Vote_Selling/histogram_meta_plot_wide.pdf", 
    #type="cairo",
    #units="in", 
    width=9, 
    height=5#, 
    #pointsize=10, 
    #res=600
)

print(histogram.meta.plot.wide)
dev.off()

## ----








#######################
#### META: Word cloud
#######################
p_load(tm, SnowballC, wordcloud, RColorBrewer)

# declare text data
v.buying.word.cloud =   as.character(meta.d.v.b$Abstract)
v.selling.word.cloud =   as.character(meta.d.v.s$Abstract)

###############
# vote buying
###############

v.buying.w.c <- Corpus(VectorSource(v.buying.word.cloud))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "/")
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "@")
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "\\|")
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "\\[")
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "\\]")
v.buying.w.c <- tm_map(v.buying.w.c, toSpace, "\\^")

# Convert the text to lower case
v.buying.w.c <- tm_map(v.buying.w.c, content_transformer(tolower))
# Remove numbers
v.buying.w.c <- tm_map(v.buying.w.c, removeNumbers)
# Remove english common stopwords
v.buying.w.c <- tm_map(v.buying.w.c, removeWords, stopwords("english"))
v.buying.w.c <- tm_map(v.buying.w.c, removeWords, c("that", "and", "the", "paper", "explain", "see", "also", "one", "two", "different", "span", "example", "however", "important", "hence", "argue", "since", "https", "well", "with", "are", "for")) 
# Remove punctuations
v.buying.w.c <- tm_map(v.buying.w.c, removePunctuation)
# Eliminate extra white spaces
v.buying.w.c <- tm_map(v.buying.w.c, stripWhitespace)

dtm.v.b <- TermDocumentMatrix(v.buying.w.c)
m.v.b <- as.matrix(dtm.v.b)
v.v.b <- sort(rowSums(m.v.b),decreasing=TRUE)
d.v.b <- data.frame(word = names(v.v.b),freq=v.v.b)
head(d.v.b, 10)

# [REVISION 2026-11] removed a stray dev.off() here: with no open device it errors
# ("cannot shut down device 1") and stops source(). res = 10 dpi also produced an unusable image.
png(filename=file.path(tempdir(), "wordcloud_buying.png"), 
    type = "cairo",
    width     = 9,
    height    = 9,
    units     = "in",
    res       = 300,
    pointsize = 10)

set.seed(2021)
wordcloud(words = d.v.b$word, freq = d.v.b$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()
###############
# vote selling 
###############

v.selling.w.c <- Corpus(VectorSource(v.selling.word.cloud))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Convert the text to lower case
v.selling.w.c <- tm_map(v.selling.w.c, content_transformer(tolower))
# Remove numbers
v.selling.w.c <- tm_map(v.selling.w.c, removeNumbers)
# Remove english common stopwords
v.selling.w.c <- tm_map(v.selling.w.c, removeWords, stopwords("english"))
v.selling.w.c <- tm_map(v.selling.w.c, removeWords, c("that", "and", "the", "paper", "explain", "see", "also", "one", "two", "different", "span", "example", "however", "important", "hence", "argue", "since", "https", "well", "with", "are", "for" )) 
# Remove punctuations
v.selling.w.c <- tm_map(v.selling.w.c, removePunctuation)
# Eliminate extra white spaces
v.selling.w.c <- tm_map(v.selling.w.c, stripWhitespace)

dtm.v.s <- TermDocumentMatrix(v.selling.w.c)
m.v.s <- as.matrix(dtm.v.s)
v.v.s <- sort(rowSums(m.v.s),decreasing=TRUE)
d.v.s <- data.frame(word = names(v.v.s),freq=v.v.s)
head(d.v.s, 10)

# [REVISION 2026-11] wordcloud() draws on the active device and returns NULL, so the previous
# code (draw first, open png, then print NULL) saved an empty file; the double dev.off() could also
# error. Open the device first, then draw.
png(filename=file.path(tempdir(), "wordcloud_selling.png"), 
    type="cairo",
    units="in", 
    width=9, 
    height=9, 
    pointsize=10, 
    res=600)

set.seed(2021)
wordcloud(words = d.v.s$word, freq = d.v.s$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()



################
#### REG TABLE
################


## ---- reg:table
p_load(modelsummary, sandwich)

## list of models
models_H1 <- list(
  "Offer size (OLS)"      = m_H1_size,
  "Any offer (logit)"     = m_H1_prob,
  "Requested price (OLS)" = m1
)

## [REVISION 2026-11] Clustered SEs. The previous helper used model$model$participant.code,
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

## [REVISION 2026-11] Table notes (Reviewer 2, minor comment 1). Print below the table in the .rnw.
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





################
#### ABSTRACT
################

## ---- abstract ----
# Abstract of the previous version (abstract.txt is now written by the Paper_3 .rnw)
abstract.c <- as.character(c("Most research on clientelism treats parties as active buyers and voters as passive targets. We argue that this misses vote selling, where voters initiate transactions and price their support. Building on prospect theory, we develop a formal model in which initiative shifts bargaining power, perceived electoral risk, and surplus allocation. Loss aversion provides one mechanism through which electorally exposed parties may overpay to avoid defeat. A laboratory experiment varying initiative, ideology, electoral strength, and pivotality supports the model’s behavioral implications. Party-initiated vote buying concentrates offers among ideologically proximate voters and generates higher voter payoffs. Voter-initiated vote selling shifts requested prices toward electorally advantaged parties, especially when they are ideologically distant, but exposes voters to rejection risk and does not necessarily increase voter surplus."))
## ----




## ---- a.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----