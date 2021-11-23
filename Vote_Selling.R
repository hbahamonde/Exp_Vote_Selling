############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Exp_Vote_Selling/")


# K-adic Data Analyses
# https://www.paulpoast.com/stata-software/4587316769

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

# extend voter's participant.code to the dyad: the common point between the parties is the voter.
p_load(dplyr,tidyverse)
# 1
v.selling.dat$participant.code.dyad.1 = ifelse(v.selling.dat$vote_s.1.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(vote_s.1.group.presupuesto) %>% fill(participant.code.dyad.1, .direction = "downup")
# 2
v.selling.dat$participant.code.dyad.2 = ifelse(v.selling.dat$vote_s.2.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(vote_s.2.group.presupuesto) %>% fill(participant.code.dyad.2, .direction = "downup")
# 3
v.selling.dat$participant.code.dyad.3 = ifelse(v.selling.dat$vote_s.3.player.votanteOpartido == "votantes", v.selling.dat$participant.code, NA)
v.selling.dat = v.selling.dat %>% group_by(vote_s.3.group.presupuesto) %>% fill(participant.code.dyad.3, .direction = "downup")

# voter sells to both parties A and B
v.selling.dat$voter.sells.to.both.1 = ifelse(v.selling.dat$vote_s.1.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.1.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat$voter.sells.to.both.2 = ifelse(v.selling.dat$vote_s.2.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.2.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat$voter.sells.to.both.3 = ifelse(v.selling.dat$vote_s.3.player.p_oferta_choice_A == 1 & v.selling.dat$vote_s.3.player.p_oferta_choice_B == 1, 1, 0)
v.selling.dat = v.selling.dat %>% group_by(vote_s.1.group.presupuesto) %>% fill(voter.sells.to.both.1, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(vote_s.2.group.presupuesto) %>% fill(voter.sells.to.both.2, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(vote_s.3.group.presupuesto) %>% fill(voter.sells.to.both.3, .direction = "downup")

# party.id.before.voter
p_load(dplyr,tidyverse)
v.selling.dat = v.selling.dat %>% group_by(vote_s.1.group.presupuesto) %>% fill(vote_s.1.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.1 = v.selling.dat$vote_s.1.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(vote_s.2.group.presupuesto) %>% fill(vote_s.2.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.2 = v.selling.dat$vote_s.2.player.tipoAoB 
v.selling.dat = v.selling.dat %>% group_by(vote_s.3.group.presupuesto) %>% fill(vote_s.3.player.tipoAoB, .direction = "downup"); v.selling.dat$party.id.before.voter.3 = v.selling.dat$vote_s.3.player.tipoAoB 


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
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.2.group.presupuesto) %>% mutate(voter.offer.2.a = max(vote_s.2.player.p_oferta_amount_A))
v.selling.dat <- v.selling.dat %>% dplyr::group_by(session.code,vote_s.2.group.presupuesto) %>% mutate(voter.offer.2.b = max(vote_s.2.player.p_oferta_amount_B))
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
v.selling.dat = v.selling.dat %>% group_by(vote_s.1.group.presupuesto) %>% fill(accepts.offer.1, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(vote_s.2.group.presupuesto) %>% fill(accepts.offer.2, .direction = "downup")
v.selling.dat = v.selling.dat %>% group_by(vote_s.3.group.presupuesto) %>% fill(accepts.offer.3, .direction = "downup")
###
v.selling.dat$accepts.offer.1.a = ifelse(v.selling.dat$role.1=="Party A" & v.selling.dat$accepts.offer.1==1, 1, 0)
v.selling.dat$accepts.offer.1.b = ifelse(v.selling.dat$role.1=="Party B" & v.selling.dat$accepts.offer.1==2, 1, 0)
v.selling.dat$accepts.offer.2.a = ifelse(v.selling.dat$role.2=="Party A" & v.selling.dat$accepts.offer.2==1, 1, 0)
v.selling.dat$accepts.offer.2.b = ifelse(v.selling.dat$role.2=="Party B" & v.selling.dat$accepts.offer.2==2, 1, 0)
v.selling.dat$accepts.offer.3.a = ifelse(v.selling.dat$role.3=="Party A" & v.selling.dat$accepts.offer.3==1, 1, 0)
v.selling.dat$accepts.offer.3.b = ifelse(v.selling.dat$role.3=="Party B" & v.selling.dat$accepts.offer.3==2, 1, 0)


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
    points.this.round.1
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
    points.this.round.2
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
    points.this.round.2
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
    points.this.round.3
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
    points.this.round.3
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
dat.v.s = dat.v.s %>% drop_na()

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

######################################################################### 
# ************** M      O       D       E       L       S **************
#########################################################################


m1 = lm(voter.offer.p ~ ideo.distance*vote.intention.party.per + pivotal.voter, dat.v.s)

options(scipen=9999999)
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
p_load(sandwich,lmtest,DAMisc,lattice,latticeExtra)
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


p_load(effects)
plot(predictorEffects(m1))


## Additional Interaction Stuff
### 1
# p_load(DAMisc)
# DAintfun2(m1, c("vote.intention.party.per", "ideo.distance"), varcov = vcov, hist=T, scale.hist=.3, level = 0.90) # plot.type="pdf"
# BGMtest(m1, vars=c("vote.intention.party.per", "ideo.distance"))
# DAintfun(m1, c("vote.intention.party.per", "ideo.distance"), theta=-45, phi=20)
### 2
p_load(sjPlot,sjmisc,ggplot2)
theme_set(theme_sjplot())
plot_model(m1, 
           type = "int",  # int / pred
           robust = T,
           ci.lvl = 0.90,
           #vcov.fun = "vcovCL",
           #vcov.args=list(cluster=dat.v.s$participant.code.dyad),
           title = "Partial Conditional Effect of Ideological Distance and Vote Share\nOn Vote-Selling Offer Made by Voters",
           axis.title = c("Ideological Distance","Predicted Amount of Vote-Selling Offer\nMade by Voter (%)"),
           legend.title = "Vote Intention (%)"
           )

# Voters
voters.payoff.v.b = data.frame(
  Payoff = c(
    dat$participant.payoff[dat$vote_b.1.player.votanteOpartido=="votantes"], 
    dat$participant.payoff[dat$vote_b.2.player.votanteOpartido=="votantes"], 
    dat$participant.payoff[dat$vote_b.3.player.votanteOpartido=="votantes"]),
  Role = "Voter",
  Game = "Vote Buying"
  )
  

voters.payoff.v.s = data.frame(
  Payoff = c(
    dat$participant.payoff[dat$vote_s.1.player.votanteOpartido=="votantes"], 
    dat$participant.payoff[dat$vote_s.2.player.votanteOpartido=="votantes"], 
    dat$participant.payoff[dat$vote_s.3.player.votanteOpartido=="votantes"]),
  Role = "Voter",
  Game = "Vote Selling"
  )
  


# Parties
parties.payoff.v.b = data.frame(
  Payoff = c(
    dat$participant.payoff[dat$vote_b.1.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_b.2.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_b.3.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_b.1.player.votanteOpartido=="Partido B"], 
    dat$participant.payoff[dat$vote_b.2.player.votanteOpartido=="Partido B"], 
    dat$participant.payoff[dat$vote_b.3.player.votanteOpartido=="Partido B"]),
  Role = "Party",
  Game = "Vote Buying"
  )
  
  
  
  
parties.payoff.v.s = data.frame(
  Payoff = c(
    dat$participant.payoff[dat$vote_s.1.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_s.2.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_s.3.player.votanteOpartido=="Partido A"], 
    dat$participant.payoff[dat$vote_s.1.player.votanteOpartido=="Partido B"], 
    dat$participant.payoff[dat$vote_s.2.player.votanteOpartido=="Partido B"], 
    dat$participant.payoff[dat$vote_s.3.player.votanteOpartido=="Partido B"]),
  Role = "Party",
  Game = "Vote Selling"
  )
  
  
payoffs.d = data.frame(
  rbind(voters.payoff.v.b,
        voters.payoff.v.s,
        parties.payoff.v.b,
        parties.payoff.v.s)
  )

# Dropping NAs
payoffs.d = payoffs.d %>% drop_na()
voters.payoff.v.b = voters.payoff.v.b %>% drop_na()
voters.payoff.v.s = voters.payoff.v.s %>% drop_na()
parties.payoff.v.b = parties.payoff.v.b %>% drop_na()
parties.payoff.v.s = parties.payoff.v.s %>% drop_na()


p_load(Rmisc,bayestestR)
# https://cran.r-project.org/web/packages/bayestestR/vignettes/credible_interval.html

payoffs.d = data.frame(
  Payoff = c(
    as.numeric(CI(voters.payoff.v.s$Payoff, ci = 0.90)[2]), # mean voters v.s 
    as.numeric(CI(voters.payoff.v.b$Payoff, ci = 0.90)[2]), # mean voters v.b 
    as.numeric(CI(parties.payoff.v.b$Payoff, ci = 0.90)[2]), # mean parties v.b 
    as.numeric(CI(parties.payoff.v.s$Payoff, ci = 0.90)[2]) # mean parties v.s 
  ),
  Upper = c(
    as.numeric(CI(voters.payoff.v.s$Payoff, ci = 0.90)[1]), # mean voters v.s 
    as.numeric(CI(voters.payoff.v.b$Payoff, ci = 0.90)[1]), # mean voters v.b 
    as.numeric(CI(parties.payoff.v.b$Payoff, ci = 0.90)[1]), # mean parties v.b 
    as.numeric(CI(parties.payoff.v.s$Payoff, ci = 0.90)[1]) # mean parties v.s
  ),
  Lower = c(
    as.numeric(CI(voters.payoff.v.s$Payoff, ci = 0.90)[3]), # mean voters v.s 
    as.numeric(CI(voters.payoff.v.b$Payoff, ci = 0.90)[3]), # mean voters v.b 
    as.numeric(CI(parties.payoff.v.b$Payoff, ci = 0.90)[3]), # mean parties v.b 
    as.numeric(CI(parties.payoff.v.s$Payoff, ci = 0.90)[3]) # mean parties v.s
  ),
  Role = c(rep("Voters",2), rep("Parties", 2)),
  Game = c("Vote Selling", "Vote Buying", "Vote Buying", "Vote Selling")
  
)



p_load(ggplot2)
ggplot(payoffs.d,
       aes(Game,
         y=Payoff,
  ymin=Lower,
  ymax=Upper))+
  geom_pointrange()+facet_wrap(~Role)


t.test(voters.payoff.v.b$Payoff,voters.payoff.v.s$Payoff, 
       conf.level = 0.95,
       alternative = "greater") # substantively significant (10%)


