############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())

# function to to recover mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(rio,tibble)
dat <- rio::import("https://github.com/hbahamonde/Economic_Experiment_Vote_Selling/raw/master/data/data.xlsx")

# dropping obs
# drop if payoff is 0
dat <- subset(dat, participant.payoff > 0)

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

# party.id.before.voter
p_load(dplyr,tidyverse)
v.selling.dat = v.selling.dat %>% group_by(vote_s.1.group.presupuesto) %>% fill(vote_s.1.player.tipoAoB); v.selling.dat$party.id.before.voter.1 = v.selling.dat$vote_s.1.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(vote_s.2.group.presupuesto) %>% fill(vote_s.2.player.tipoAoB); v.selling.dat$party.id.before.voter.2 = v.selling.dat$vote_s.2.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(vote_s.3.group.presupuesto) %>% fill(vote_s.3.player.tipoAoB); v.selling.dat$party.id.before.voter.3 = v.selling.dat$vote_s.3.player.tipoAoB 


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
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.1.group.presupuesto) %>% mutate(voter.offer.1.a = max(vote_s.1.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.1.group.presupuesto) %>% mutate(voter.offer.1.b = max(vote_s.1.player.p_oferta_amount_B))
## 2
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.2.group.presupuesto) %>% mutate(voter.offer.2.a = max(vote_s.1.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.2.group.presupuesto) %>% mutate(voter.offer.2.b = max(vote_s.1.player.p_oferta_amount_B))
## 3
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.3.group.presupuesto) %>% mutate(voter.offer.3.a = max(vote_s.3.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.3.group.presupuesto) %>% mutate(voter.offer.3.b = max(vote_s.3.player.p_oferta_amount_B))


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
  group_by(vote_s.1.group.presupuesto) %>%
  mutate(voter.own.1.a = max(voter.own.1.a))
v.selling.dat <- v.selling.dat %>%
  group_by(vote_s.1.group.presupuesto) %>%
  mutate(voter.own.1.b = max(voter.own.1.b))
## 2
v.selling.dat <- v.selling.dat %>%
  group_by(vote_s.2.group.presupuesto) %>%
  mutate(voter.own.2.a = max(voter.own.2.a))
v.selling.dat <- v.selling.dat %>%
  group_by(vote_s.2.group.presupuesto) %>%
  mutate(voter.own.2.b = max(voter.own.2.b))
## 3
v.selling.dat <- v.selling.dat %>%
  group_by(vote_s.3.group.presupuesto) %>%
  mutate(voter.own.3.a = max(voter.own.3.a))
v.selling.dat <- v.selling.dat %>%
  group_by(vote_s.3.group.presupuesto) %>%
  mutate(voter.own.3.b = max(voter.own.3.b))


# pivotal.voter dyad
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


# HERE



v.selling.dat.1.a = subset(
  v.selling.dat, select = c(
    participant.code,
    role.1,
    participant.payoff,
    ideo.distance.1.a, 
    voter.offer.1.a,
    pivotal.voter.1.a, 
    voter.own.1.a,
    budget.1.a,
    points.this.round.1
  )
)


v.selling.dat.1.a <- subset(v.selling.dat.1.a, role.1 == "Party A") # drop voters
colnames(v.selling.dat.1.a)[colnames(v.selling.dat.1.a)=="role.1"] <- "Dyad" # changes name to dyad
v.selling.dat.1.a$Dyad = as.factor(v.selling.dat.1.a$Dyad)
p_load(dplyr)
v.selling.dat.1.a = v.selling.dat.1.a %>% mutate(Dyad=recode(Dyad, "Party A" = "Party A, Voter","Party B" = "Party B, Voter"))
colnames(v.selling.dat.1.a) = sub(".1.*", "", colnames(v.selling.dat.1.a)) # remove extra characters in col names
v.selling.dat.1.a$round = 1

# do.
## 1. compute vote share
## 2. compute points cumula







# K-adic Data Analyses
# https://www.paulpoast.com/stata-software/4587316769



