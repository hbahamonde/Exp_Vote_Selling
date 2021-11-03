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

p_load(dplyr)
v.selling.dat = v.selling.dat %>% select(vote_s.1.player.votanteOpartido,
                                         vote_s.1.player.tipoAoB,
                                         vote_s.1.player.p_oferta_choice_A,
                                         vote_s.1.player.p_oferta_choice_B,
                                         vote_s.1.player.p_oferta_amount_A,
                                         vote_s.1.player.p_oferta_amount_B,
                                         voter.offer,
                                         vote_s.1.player.partido_envia_puntos,
                                         vote_s.1.player.votante_acepta_oferta,
                                         vote_s.1.player.win_lose,
                                         vote_s.1.player.win_losev,
                                         vote_s.1.player.puntos,
                                         vote_s.1.player.payoff,
                                         vote_s.1.group.id_in_subsession,
                                         vote_s.1.group.presupuesto,
                                         vote_s.1.group.n_votantes,
                                         vote_s.1.group.n_votantes_A,
                                         vote_s.1.group.n_votantes_B,
                                         vote_s.1.group.partido_elegido,
                                         vote_s.1.group.tipo_votante,
                                         vote_s.1.group.ubicacion_pA,
                                         vote_s.1.group.ubicacion_pB,
                                         vote_s.1.group.pje_win_cA,
                                         vote_s.1.group.pje_win_cB,
                                         everything())

# voter.offer
v.selling.dat$voter.offer.1 = (v.selling.dat$vote_s.1.player.p_oferta_amount_A-v.selling.dat$vote_s.1.player.p_oferta_amount_B)
v.selling.dat$voter.offer.2 = (v.selling.dat$vote_s.2.player.p_oferta_amount_A-v.selling.dat$vote_s.2.player.p_oferta_amount_B)
v.selling.dat$voter.offer.3 = (v.selling.dat$vote_s.3.player.p_oferta_amount_A-v.selling.dat$vote_s.3.player.p_oferta_amount_B)

# ideo.distance
v.selling.dat$ideo.distance.1 = (v.selling.dat$vote_s.1.group.ubicacion_pB-100)+(v.selling.dat$vote_s.1.group.ubicacion_pA)
v.selling.dat$ideo.distance.2 = (v.selling.dat$vote_s.2.group.ubicacion_pB-100)+(v.selling.dat$vote_s.2.group.ubicacion_pA)
v.selling.dat$ideo.distance.3 = (v.selling.dat$vote_s.3.group.ubicacion_pB-100)+(v.selling.dat$vote_s.3.group.ubicacion_pA)

# pivotal.voter
v.selling.dat$pivotal.voter.1 = abs(v.selling.dat$vote_s.1.group.n_votantes_A-v.selling.dat$vote_s.1.group.n_votantes_B)
v.selling.dat$pivotal.voter.1 = ifelse(v.selling.dat$pivotal.voter.1==1, 1, 0)
v.selling.dat$pivotal.voter.2 = abs(v.selling.dat$vote_s.2.group.n_votantes_A-v.selling.dat$vote_s.2.group.n_votantes_B)
v.selling.dat$pivotal.voter.2 = ifelse(v.selling.dat$pivotal.voter.2==1, 1, 0)
v.selling.dat$pivotal.voter.3 = abs(v.selling.dat$vote_s.3.group.n_votantes_A-v.selling.dat$vote_s.3.group.n_votantes_B)
v.selling.dat$pivotal.voter.3 = ifelse(v.selling.dat$pivotal.voter.3==1, 1, 0)

# vote.intention.party
v.selling.dat$vote.intention.party = round((v.selling.dat$vote.intention.party*100)/vote_s.1.group.n_votantes,0)

(v.selling.dat$vote_s.1.group.n_votantes_A-(v.selling.dat$vote_s.1.group.n_votantes_B))
