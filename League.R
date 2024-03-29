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

#on peut constater grace � ce graphique, qu'il y a une corr�lation entre les golds gagn� par un joueur et les kils qu'il fait

#cette corr�lation peut etres d�montr� en calculant le coefficient de Pearson

correlation_gold_kills <- cor(les_golds, les_kills)
View(correlation_gold_kills)


les_wins <- filter(player_team_lol_data, winTeam1 == "Win")

View(les_wins)


les_loose <- filter(player_team_lol_data, winTeam1 == "Fail")

new <- filter(player_team_lol_data, winTeam1 == "Win")


View(les_loose)


#nous allons maintenant compar� les golds lorsqu'un equipe perd et les golds lorsqu'une equipe gagne
 moyenne_gold_gagnant <- mean(les_wins$gold1)
 
 print(moyenne_gold_gagnant)
 
 
 moyenne_gold_perdant <- mean(les_loose$gold1)
 
 print(moyenne_gold_perdant)
 
 #on se rend compte ici que les golds dans une partie joue un role, et que sur 1040 parties, l'�quipe perdante a en moyenne moins de gold
 #que l'�quipe gagnante. on peut donc supposer que les golds joue un role important par rapport au r�sultat de la partie

 
 
 bcp_gold <-(filter(lol, gold1 > 10000))

 freq(bcp_gold)
 
 freq(bcp_gold$winTeam1)
 
 
 bcp_gold2 <-(filter(lol, gold2 > 10000 & gold1 > 10000))
 freq(bcp_gold2$winTeam1)
 
 bcp_gold3 <-(filter(lol, gold2 > 10000 & gold1 > 10000 & gold3 > 10000))
 
 freq(bcp_gold3$winTeam1)
 
 
 #ici, nous avons voulu �tudier le taux de victoire en fonction du nombre de joueur ayant
 #atteint 10K gold ou plus au cours de la partie. On constate bien grace aux fr�quences
 #que le taux de victoire augmentent en fonction du nombre de joueurs dans une �quipe ayant
 #attteint ce montant.
 #mais on voit que ce pourcetange ce stabilise lorsqu'on se rapproche d e5 joueurs ayant gagn� 10K
 
 
 le_mid <- filter(lol, lane1 == "MIDDLE")
 mean(le_mid$gold1)
 le_top <- filter(lol, lane1 == "TOP")
 mean(le_top$gold1)
 le_jgl <- filter(lol, lane1 == "JUNGLE")
 mean(le_jgl$gold1)
 
 
tab_baron <- table(lol$winTeam1, lol$baronKillsTeam1) 

cprop(tab_baron)

#grace � ces analyses on constate que le team ayant tu� le plus de barron nashor remporte la plupart du temps
# meme la probabilit� d'avoir 3 baronkills au cours d'une partie est faible, on constate tout de meme
#que l'equipe ayant le plus de baronkills a un % de victoire superieur

#nous pouvons faire la meme analyse avec les dragons kills, en general, une equipe ayant 
#tu� plus de dragons poss�de un taux de victoire sup�rieur

tab_2 <- table(lol$winTeam1, lol$dragonKillsTeam1) 

cprop(tab_2)
lprop(tab_2)




#parmis les 1040 matchs, nous souhaitons faire un test d'hyptoh�se concernant le nombre de dragons tu�es. nous allons determiner � et ?? de notre
#population .


variance_drake <- var(lol$dragonKillsTeam1)
moyenne_drake <- mean(lol$dragonKillsTeam1)


#on veut affirmer avec un faible risque de se tromper que l'�quipe qui gagne a tu� plus de dragons que l'autre en moyenne

var_wins = var(les_wins$dragonKillsTeam1)
var_loose = var(les_wins$dragonKillsTeam2)
mean_test1(les_wins$dragonKillsTeam1, mean(lol$dragonKillsTeam1), var(lol$dragonKillsTeam1), side = 1)$p_value


#test qui prouve
mean_test2(les_wins$dragonKillsTeam1, les_wins$dragonKillsTeam2, sigma = c(var_wins, var_loose), side = -1)$p_value

sd(les_wins$dragonKillsTeam1)

mean_test2(les_wins$dragonKillsTeam1, les_wins$dragonKillsTeam2, sigma = c(1.318045, 1.166436))


timeline <- read_csv("C:/Users/maelb/Desktop/cours/L3/proba stat/cours lol/timeline.csv")


#Nous avons prouv� ulterierement que les golds influ� sur le taux de victoire, nous allons
# � pr�sent voir un evennement ui influe beacoup sur une �quipe et nottement sur ses golds gagn�
# je parle d'un Kill de Baron Nashor. � la 26eme minute on constate que le joueur D'id 4 
#tu� un nashor, nous allons donc compar� les golds de l'�quipe ayant tu� le nash avant et apr�s ce kill
# Les �chantillons sont appari�s car ce sont les m�mes individus qui tuent le barons nashors

#dans le fichier timeline, j'ai chercher les barons kills
#j'ai ensuite cherch� l'id du joueur qui l'avait tu�
#j'ai ensuite regard� les golds de chacun de ses co�quipier avant et apr�s le kills du nashor


View(timeline$`events/20/monsterType`)
View(timeline$`events/20/killerId`)
View(timeline$`participantFrames/4/totalGold`)



gold_avant_nash = c(9676,9206,12715,10519,6682)

gold_apr�s_nash = c(10585,9562,13785,11104,6983)

t.test(gold_avant_nash, gold_apr�s_nash, paired = T, alternative = "less")$p.value

#la p-value �tant tr�s faible, on peut considerer que le nashor donne un certain avantage en gold
#� l'�quipe qui le tue


champion_1 <-(filter(lol, championID1 == 236 | championID2 == 236 | championID3 == 30 | championID4 == 236 | championID5 == 236))

win_champions_1<- table(champion_1$winTeam1)

#ici on va chercher a voir si il y a un lien entre la pr�sence d'un champion dans une �quipe
#et le taux de victoire de cette �quipe

champion_2 <-(filter(lol, championID1 == 22 | championID2 == 22 | championID3 == 22 | championID4 == 22 | championID5 == 22))

win_champions_2 <- table(champion_2$winTeam1)


champion_3 <-(filter(lol, championID1 == 202 | championID2 == 202 | championID3 == 202 | championID4 == 202 | championID5 == 202))

win_champions_3 <- table(champion_3$winTeam1)

#ici, nous avons cr�e des tables comportant le nombre de win et de defaite en fonction de trois champion 
#jou� ass� fr�quement

#afin de trouv� ces champions jou� recemment j'ai mis a tour de role dans la varibale "champions_joue"
# lol$championID1, lol$championID2, lol$championID3 afin d'avoir un estimatif des 3 champions frequemment jou�
#en suite j'ai cr�e une table qui prend en compte uniquement les victoires et defaites de l'�quipe comprtant ces champions
#jou�e recemment
#grace � ces tableau, nous allons pouvoir construire une matrice qui nous permettra de faire un test d'ind�pendance
Independance_champion_victoire = matrix(c(64, 66, 76, 46, 50, 54), nrow = 2, byrow = T)
chisq.test(Independance_champion_victoire)$p.value

#ce r�sultat nous permet de pouvoir affirmer que les chapions choisis et le taux de victoire/defaite sont d�pendants


#pour conclure, j'aimerais faire d�montrer par une grace a une loi de probabilit� que le dragon
# est un determinant dans la partie, et permet de d'en gagner une plus facilement
#en effet j'ai reuni tout les parties ou les 5 joueurs de l'�quipe 1 avait plus de gold que l'�quipe 2
#si on prend parmis tout ces matchs, quel est la  probabilit� d'avoir au moins 3 dragons tu� par l'�quipe 1


test_loi <- filter(lol, gold1 > gold6, gold2 > gold7, gold3 > gold8, gold4 > gold9, gold5> gold10)


table_dragon_tue_equipe_gagnante <- table(test_loi$dragonKillsTeam1)


#ici on peut voir que la fr�quence de 2 dragons tu� est de 28.4%
#le pourcentage de chance est donc de 0.28
#nous allons donc calculer la probabilit� de trouv� un match ou l'�quipe 1 a tu� 2 dragons 
#ici nous tirons au sort parmis les match ou l'�quipe 1 a plus de gold que l'�quipe 2 jusqu'a ce que nous obtenions un match
#ou l'�quipe 1 a tu� 2 dragons. 
1-dgeom(3, 0.28)

# ce test signifie que la probabilit� de tir�e au sort un match ou l'�quipe 1 a tu� deux dragons apr�s 3
#relance est deux 89%. autrement dit, la probabilit� est tr�s �lev� car les �quipes remportant les matchs
# ont tendance a plus tuer les drakes

#nous allons a pr�sent nous interess� a un crit�re qui a son importance dans une partie
# en effet lorsqu'un joueur ou plusieurs meurent un certain nombre de fois, leur pourcentage de chance de gagner
# se retrouvent tr�s restreint

bcp_mort <- filter(lol, deaths1 > 5, deaths2 > 5, deaths3 > 5)

#si on regarde le nombre de partie gagn� par l'�quipe 1, il est faible. nous allons donc chercher
# la probabilit� de tirer une partie "gagnante" parmi ce lot de parties ou trois joueurs ont au moins 5 morts


tab_win_mort = table(bcp_mort$winTeam1)



