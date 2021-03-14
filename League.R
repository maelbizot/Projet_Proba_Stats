library(readr)
library(dplyr)
library(questionr)
library(OneTwoSamples)
library(rjson)
library(prob)




player_team_lol_data <- read_csv("C:/Users/maelb/Desktop/cours/L3/proba stat/cours lol/player_team_lol_data.csv")

lol <- player_team_lol_data

View(lol)



les_golds <- c(lol$gold1, lol$gold2, lol$gold3)
les_kills <- c(lol$kills1, lol$kills2, lol$kills3)



relation_gold_kills <- plot(les_golds, les_kills)
View(relation_gold_kills)

#on peut constater grace à ce graphique, qu'il y a une corrélation entre les golds gagné par un joueur et les kils qu'il fait

#cette corrélation peut etres démontré en calculant le coefficient de Pearson

correlation_gold_kills <- cor(les_golds, les_kills)
View(correlation_gold_kills)


les_wins <- filter(player_team_lol_data, winTeam1 == "Win")

View(les_wins)


les_loose <- filter(player_team_lol_data, winTeam1 == "Fail")

new <- filter(player_team_lol_data, winTeam1 == "Win")


View(les_loose)


#nous allons maintenant comparé les golds lorsqu'un equipe perd et les golds lorsqu'une equipe gagne
 moyenne_gold_gagnant <- mean(les_wins$gold1)
 
 print(moyenne_gold_gagnant)
 
 
 moyenne_gold_perdant <- mean(les_loose$gold1)
 
 print(moyenne_gold_perdant)
 
 #on se rend compte ici que les golds dans une partie joue un role, et que sur 1040 parties, l'équipe perdante a en moyenne moins de gold
 #que l'équipe gagnante. on peut donc supposer que les golds joue un role important par rapport au résultat de la partie

 
 
 bcp_gold <-(filter(lol, gold1 > 10000))

 freq(bcp_gold)
 
 freq(bcp_gold$winTeam1)
 
 
 bcp_gold2 <-(filter(lol, gold2 > 10000 & gold1 > 10000))
 freq(bcp_gold2$winTeam1)
 
 bcp_gold3 <-(filter(lol, gold2 > 10000 & gold1 > 10000 & gold3 > 10000))
 
 freq(bcp_gold3$winTeam1)
 
 
 #ici, nous avons voulu étudier le taux de victoire en fonction du nombre de joueur ayant
 #atteint 10K gold ou plus au cours de la partie. On constate bien grace aux fréquences
 #que le taux de victoire augmentent en fonction du nombre de joueurs dans une équipe ayant
 #attteint ce montant.
 #mais on voit que ce pourcetange ce stabilise lorsqu'on se rapproche d e5 joueurs ayant gagné 10K
 
 
 le_mid <- filter(lol, lane1 == "MIDDLE")
 mean(le_mid$gold1)
 le_top <- filter(lol, lane1 == "TOP")
 mean(le_top$gold1)
 le_jgl <- filter(lol, lane1 == "JUNGLE")
 mean(le_jgl$gold1)
 
 
tab_baron <- table(lol$winTeam1, lol$baronKillsTeam1) 

cprop(tab_baron)

#grace à ces analyses on constate que le team ayant tué le plus de barron nashor remporte la plupart du temps
# meme la probabilité d'avoir 3 baronkills au cours d'une partie est faible, on constate tout de meme
#que l'equipe ayant le plus de baronkills a un % de victoire superieur

#nous pouvons faire la meme analyse avec les dragons kills, en general, une equipe ayant 
#tué plus de dragons possède un taux de victoire supérieur

tab_2 <- table(lol$winTeam1, lol$dragonKillsTeam1) 

cprop(tab_2)
lprop(tab_2)




#parmis les 1040 matchs, nous souhaitons faire un test d'hyptohèse concernant le nombre de dragons tuées. nous allons determiner µ et ?? de notre
#population .


variance_drake <- var(lol$dragonKillsTeam1)
moyenne_drake <- mean(lol$dragonKillsTeam1)


#on veut affirmer avec un faible risque de se tromper que l'équipe qui gagne a tué plus de dragons que l'autre en moyenne

var_wins = var(les_wins$dragonKillsTeam1)
var_loose = var(les_wins$dragonKillsTeam2)
mean_test1(les_wins$dragonKillsTeam1, mean(lol$dragonKillsTeam1), var(lol$dragonKillsTeam1), side = 1)$p_value


#test qui prouve
mean_test2(les_wins$dragonKillsTeam1, les_wins$dragonKillsTeam2, sigma = c(var_wins, var_loose), side = -1)$p_value

sd(les_wins$dragonKillsTeam1)

mean_test2(les_wins$dragonKillsTeam1, les_wins$dragonKillsTeam2, sigma = c(1.318045, 1.166436))


timeline <- read_csv("C:/Users/maelb/Desktop/cours/L3/proba stat/cours lol/timeline.csv")


#Nous avons prouvé ulterierement que les golds influé sur le taux de victoire, nous allons
# à présent voir un evennement ui influe beacoup sur une équipe et nottement sur ses golds gagné
# je parle d'un Kill de Baron Nashor. à la 26eme minute on constate que le joueur D'id 4 
#tué un nashor, nous allons donc comparé les golds de l'équipe ayant tué le nash avant et après ce kill
# Les échantillons sont appariés car ce sont les mêmes individus qui tuent le barons nashors

#dans le fichier timeline, j'ai chercher les barons kills
#j'ai ensuite cherché l'id du joueur qui l'avait tué
#j'ai ensuite regardé les golds de chacun de ses coéquipier avant et après le kills du nashor


View(timeline$`events/20/monsterType`)
View(timeline$`events/20/killerId`)
View(timeline$`participantFrames/4/totalGold`)



gold_avant_nash = c(9676,9206,12715,10519,6682)

gold_après_nash = c(10585,9562,13785,11104,6983)

t.test(gold_avant_nash, gold_après_nash, paired = T, alternative = "less")$p.value

#la p-value étant très faible, on peut considerer que le nashor donne un certain avantage en gold
#à l'équipe qui le tue


champion_1 <-(filter(lol, championID1 == 236 | championID2 == 236 | championID3 == 30 | championID4 == 236 | championID5 == 236))

win_champions_1<- table(champion_1$winTeam1)

#ici on va chercher a voir si il y a un lien entre la présence d'un champion dans une équipe
#et le taux de victoire de cette équipe

champion_2 <-(filter(lol, championID1 == 22 | championID2 == 22 | championID3 == 22 | championID4 == 22 | championID5 == 22))

win_champions_2 <- table(champion_2$winTeam1)


champion_3 <-(filter(lol, championID1 == 202 | championID2 == 202 | championID3 == 202 | championID4 == 202 | championID5 == 202))

win_champions_3 <- table(champion_3$winTeam1)

#ici, nous avons crée des tables comportant le nombre de win et de defaite en fonction de trois champion 
#joué assé fréquement

#afin de trouvé ces champions joué recemment j'ai mis a tour de role dans la varibale "champions_joue"
# lol$championID1, lol$championID2, lol$championID3 afin d'avoir un estimatif des 3 champions frequemment joué
#en suite j'ai crée une table qui prend en compte uniquement les victoires et defaites de l'équipe comprtant ces champions
#jouée recemment
#grace à ces tableau, nous allons pouvoir construire une matrice qui nous permettra de faire un test d'indépendance
Independance_champion_victoire = matrix(c(64, 66, 76, 46, 50, 54), nrow = 2, byrow = T)
chisq.test(Independance_champion_victoire)$p.value

#ce résultat nous permet de pouvoir affirmer que les chapions choisis et le taux de victoire/defaite sont dépendants


#pour conclure, j'aimerais faire démontrer par une grace a une loi de probabilité que le dragon
# est un determinant dans la partie, et permet de d'en gagner une plus facilement
#en effet j'ai reuni tout les parties ou les 5 joueurs de l'équipe 1 avait plus de gold que l'équipe 2
#si on prend parmis tout ces matchs, quel est la  probabilité d'avoir au moins 3 dragons tué par l'équipe 1


test_loi <- filter(lol, gold1 > gold6, gold2 > gold7, gold3 > gold8, gold4 > gold9, gold5> gold10)


table_dragon_tue_equipe_gagnante <- table(test_loi$dragonKillsTeam1)


#ici on peut voir que la fréquence de 2 dragons tué est de 28.4%
#le pourcentage de chance est donc de 0.28
#nous allons donc calculer la probabilité de trouvé un match ou l'équipe 1 a tué 2 dragons 
#ici nous tirons au sort parmis les match ou l'équipe 1 a plus de gold que l'équipe 2 jusqu'a ce que nous obtenions un match
#ou l'équipe 1 a tué 2 dragons. 
1-dgeom(3, 0.28)

# ce test signifie que la probabilité de tirée au sort un match ou l'équipe 1 a tué deux dragons après 3
#relance est deux 89%. autrement dit, la probabilité est très élevé car les équipes remportant les matchs
# ont tendance a plus tuer les drakes

#nous allons a présent nous interessé a un critère qui a son importance dans une partie
# en effet lorsqu'un joueur ou plusieurs meurent un certain nombre de fois, leur pourcentage de chance de gagner
# se retrouvent très restreint

bcp_mort <- filter(lol, deaths1 > 5, deaths2 > 5, deaths3 > 5)

#si on regarde le nombre de partie gagné par l'équipe 1, il est faible. nous allons donc chercher
# la probabilité de tirer une partie "gagnante" parmi ce lot de parties ou trois joueurs ont au moins 5 morts


tab_win_mort = table(bcp_mort$winTeam1)



