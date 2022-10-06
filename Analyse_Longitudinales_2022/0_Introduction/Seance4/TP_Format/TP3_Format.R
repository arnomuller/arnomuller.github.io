###############################
####     TP3 : FORMAT      ####
###############################




# Packages     ####
library(tidyverse)
library(TraMineR)

# Info         ####

# Travaux Pratique : Format de Donn?es

# On a la base individu*ann?e de l'enqu?te Peuplement et D?peuplement de Paris:
# "PDP_IND_YEAR.csv"
# Avec :
#	IDENT : Identifiant de l'individu enqu?t?
#	Age : Caract?ristiques de l'individu ? l'?ge x (Plusieurs observations par INDIVIDU)
#	ORIG_SOC : Origine et trajectoire sociale de l'individu
#	SO_LOG :  Statut d'occupation ? l'?ge X
#	SITMAT : Situation de l'individu ? l'enqu?te
#	GEO_1 : Positionnement g?ographique de l'individu ? l'?ge X


# QUESTION 0 : ####

# Ouverture et transformation de la base :
# - Ne garder que les lignes o? l'on connait le statut d'occupation 
# - Ne garder que les variables d'int?r?t : Identifiant, Age, Origine sociale, 
# Statut d'occupation, Situation matrimoniale, Position G?ographique, ainsi que
# les variable ann?es de naissances (ANNAIS) et generation (gen).
# - Transformer la variable SO_LOG, en enlevant le num?ro devant le statut
# - Faire de m?me pour SITMAT
table(PDP$SITMAT)

length(unique(PDP$IDENT))


setwd("D:/IDUP/1_Analyse_Sequence/Fichiers")
PDP = read.csv2("./TP_Format/PDP_IND_YEAR.csv",stringsAsFactors=F) %>%
  filter(SO_LOG != "") %>%                                      # On garde que les ann?es avec logements
  select(IDENT,Age,ORIG_SOC,SO_LOG,SITMAT,GEO_1,ANNAIS,gen) %>% # Selection des variables
  mutate(SO_LOG=substr(SO_LOG,3,5)) %>%          # On enleve les 3 premiers caract?res
  mutate(SITMAT=substr(SITMAT,4,7))              # On enleve les 4 premiers caract?res
# Voir : ?substr

# Nombre d'individus : 
length(unique(PDP$IDENT))

# length donne la longueur
# unique affiche chaque identifiant unique





# QUESTION 1 : ####



# On a un fichier INIDIVIDUS*ANNEES, on veut un fichier INIDIVIDU, qui renseigne
# les diff?rents statuts d'occupation de chaque enqu?t?
# On veut ?galement conna?tre les informations individuelles (ann?e naissance, origine sociale, g?n?ration).



# Transformer en FICHIER INDIVIDUEL (ou format large) :

table(PDP$Age)
table(PDP$SO_LOG)


# Obtenir la base longue sans autre information :

# ?pivot_wider
Z = PDP %>%
  select(IDENT,SO_LOG,Age) %>%                               
  # On ne garde que l'identifiant, et son logement ? chaque age
  mutate(Age = ifelse(Age < 10, paste0("0",Age), Age)) %>%   
  # la variable age va devenir le noms des variables, il faut donc la transformer
  # pour que tous les ages aient le m?me format (deux chiffres)
  arrange(IDENT, Age) %>%
  # On classe les lignes par age pour chaque individus
  pivot_wider(names_from = Age, 
              # q=Quelle variable doit donner le nom des colonnes : Age
              names_prefix = "AGE", 
              # J'ajoute le pr?fix AGE devant chaque Age pour de plus jolie nom de colonnes
              names_sort = TRUE, 
              # Je trie les colonnes dans l'ordre ici alphab?tique et num?rique
              values_from = SO_LOG) # Quelle valeur doit prendre la colonne ?

table(Z$'01')

# On connait pour chaque individu leur statut d'habitation ? chaque ?ge sur une seule ligne
# mais on veut ajouter des informations socio-d?mographique, comme l'ann?e de naissance
# et l'origine sociale :

# Ici on cr?er une base de donn?e sur les informations individuelles
Y <- PDP %>% 
  group_by(IDENT) %>% 
  slice(1) %>% # On garde la premi?re ligne, mais gr?ce ? group_by, on garde la 1ere ligne de chaque IDENT
  select(IDENT, ORIG_SOC, ANNAIS, gen)
# Comme les variables indiv sont fixes dans le temps leurs valeurs restent la m?me ? chaque ?ge
# Maintenant on veut les ajouter ? Z :

# On merge les informations
Z <- Z %>% 
  left_join(Y, by = "IDENT")
# Verif : 
addmargins(table(Z$ORIG_SOC,useNA = "always"))




# On aurait ?galement pu directement garder les informations individuelles en faisant :

Z2 = PDP %>%
  select(IDENT,SO_LOG,Age, ORIG_SOC, ANNAIS, gen) %>% 
  # On garde en plus les info individuelles
  mutate(Age = ifelse(Age < 10, paste0("0",Age), Age)) %>%   
  arrange(IDENT, Age) %>%
  pivot_wider(names_from = Age, 
              names_prefix = "AGE", 
              names_sort = TRUE, 
              values_from = SO_LOG)






# QUESTION 2 : ####



#	A partir de ce fichier INDIVIDU, reconstruire un fichier INDIVIDU*AGE, 
# avec une observation pour chaque changement d'?ge d'un individu


# Fichier INDIVIDU => INDIVIDU*AGE (format long)

# ?pivot_longer


A = Z %>%
  pivot_longer(#cols = c(AGE00:AGE61),
               # O? sont les Status d'Occupation ? marche aussi avec les num?ros de colonnes
               names_to = "Age",
               # Nom de la nouvelle variable pour l'age
               names_prefix = "AGE",
               # Indiquer le pr?fixe ? enlever ? chaque ?ge
               values_to = "SO"
               # Nom de la nouvelle variable pour les statuts d'occupations ? chaque ?ge
               ) %>% 
  arrange(IDENT,Age) %>% # Tri ?tait d?j? fait, je le met juste au cas o?
  mutate(Age = as.numeric(Age)) # Mieux


# On voit qu'il y a beaucoup de NA pour SO, on peut les enlever avec un simple filter
# ou alors directement en faisant : 


A = Z %>%
  pivot_longer(cols = c(AGE00:AGE61),
               names_to = "Age",
               names_prefix = "AGE",
               values_to = "SO",
               values_drop_na = TRUE
               # On ne garde pas les ages avec NA
               ) %>% 
  arrange(IDENT,Age) %>% 
  mutate(Age = as.numeric(Age))




# QUESTION 3 : ####


#	A partir du fichier INDIVIDU (Z), reconstruire un fichier INDIVIDU*SO, 
# avec une observation pour chaque changement de statut d'occupation d'un individu. 
# On s'int?resse ici aux TRANSITIONS


# Fichier INDIVIDU => INDIVIDU*EVENT (format semi-long)


E = Z %>%
  pivot_longer(cols = c(AGE00:AGE61),
               names_to = "Age",
               names_prefix = "AGE",
               values_to = "SO",
               values_drop_na = TRUE) %>% 
  mutate(Age = as.numeric(Age))       %>% 
  arrange(IDENT,Age)                  %>% 
  # Jusque l? on a fait comme avant
  # Maintenant on enleve toutes les lignes o? il n'y a pas de chgmt de SO avec l'ann?e d'avant
  group_by(IDENT)                     %>% 
  # Pour chaque individu, je resneigne ? chaque ?ge qu'elle ?tait le SO de l'age pr?c?dent dans SO_PREC
  mutate(SO_PREC=lag(SO))             %>% 
  # ?lag renvoie la colonne au dessus
  filter(row_number()==1 | SO_PREC != SO) %>% 
  #On conserve la 1ere ligne ainsi que toutes celles o? SO_Prec diff?rent de SO
  select(-SO_PREC)




# On peut aussi ajouter d'autres informations comme : 
# - l'?ge au moment de l'enqu?te, dans notre cas ce serait l'age max
# - le temps pass? dans chaque statut d'occupation

mutate(Age_Enq=max(Age))                      %>%
  filter(row_number()==1 | SO_PREC != Statut) %>%
  mutate(Nb_Ans=ifelse(row_number()!= n(),lead(Age)-Age,Age_Enq-Age))

Ebis = Z %>%
  pivot_longer(cols = c(AGE00:AGE61),
               names_to = "Age",
               names_prefix = "AGE",
               values_to = "SO",
               values_drop_na = TRUE)     %>% 
  mutate(Age = as.numeric(Age))           %>% 
  arrange(IDENT,Age)                      %>% 
  group_by(IDENT)                         %>% 
  mutate(SO_PREC=lag(SO))                 %>% 
  mutate(Age_Enq = max(Age))              %>% # On ajoute l'age max avant de filtrer
  filter(row_number()==1 | SO_PREC != SO) %>% 
  select(-SO_PREC)                        %>% 
  mutate(Nb_Ans= ifelse(row_number()!= n(), #si au sein de chaque IDENT, la ligne n'est pas la derni?re ligne de cet IDENT, alors :
                        lead(Age)-Age, # Nb_Ans = Age dans la colonne en dessous - Age dans cette ligne
                        Age_Enq-Age))  # Sinon c'est qu'on est dans derni?re ligne et donc qu'au moment de l'enqu?te il est toujours dans cette SO,
                                       # Nb_Ans = AgeMax - Age dans cette ligne

# ?n()
# ?lead



# Remise en format individuel
B = E %>%
  mutate(SO_num=paste0("SO_",row_number())) %>%
  pivot_wider(id_cols = c(IDENT, SO,SO_num, ANNAIS, gen, ORIG_SOC),
              names_from = SO_num, 
              names_sort = TRUE, 
              values_from = SO)



# QUESTION 4 : ####



#	A partir du fichier INDIVIDU*ANNEE (PDP), reconstruire un fichier INDIVIDU*SO, 
# avec une observation pour chaque changement de statut d'occupation d'un individu


# Fichier INDIVIDU*ANNEE => INDIVIDU*EVENT

E2 = PDP                                %>%
  select(IDENT,Age,SO_LOG)              %>%
  arrange(IDENT,Age)                    %>%
  group_by(IDENT)                       %>%
  mutate(SO_PREC=lag(SO_LOG))           %>%
  filter(row_number()==1 | SO_PREC != SO_LOG)
# On garde que les lignes avec un changement

# Remise en format individuel
B2 = E2                                     %>%
  select(IDENT,SO_LOG)                      %>%
  mutate(SO_num=paste0("SO_",row_number())) %>%
  pivot_wider(names_from = SO_num, 
            names_sort = TRUE, 
            values_from = SO_LOG)





# QUESTION 5 ####



# Fichier INDIVIDU*EVENT => INDIVIDU*ANNEE

str(E)
PDP2 = E                        %>%
  ungroup                       %>%   # Important
  complete(IDENT, Age)          %>%   # fonction complete permet de cr?er les combinaisons manquantes entre les variables IDENT et Age
  group_by(IDENT)               %>%   # pour reprendre l'operation fill ? chaque nouvel individu
  fill(SO,ORIG_SOC,ANNAIS,gen) 











############

############
# Activit? suppl?mentaire  ####

# Quels sont les d?terminants de l'acc?s ? la propri?t? ?




# Au moment de l'enqu?te ? ####

# On garde que la derni?re ligne pour chaque individu
# On prend le format long car c'est le plus pr?cis
Enq <- A %>% 
  group_by(IDENT) %>% 
  filter(row_number()==n()) %>% 
  mutate(SO_c = as.factor(ifelse(SO == "Proprietaire", 1, 0)),
         ORIG_SOC_c = as.factor(ifelse(ORIG_SOC %in% c("Classe moyenne ascendante", "Classe moyenne stable"), "Moyenne",
                                       ifelse(ORIG_SOC %in% c("Populaire", "Classe populaire descendante"), "Populaire",
                                              "Superieur"))))

table(Enq$SO)
table(Enq$SO_c)
table(Enq$ORIG_SOC)
table(Enq$ORIG_SOC_c)
table(Enq$ANNAIS)


# On change la modalit? de r?f?rence pour ORIG_SOC_c
?relevel
Enq$ORIG_SOC_c <- relevel(Enq$ORIG_SOC_c, "Moyenne")

# On fait la r?gression
reg_enq <- glm(SO_c ~ ORIG_SOC_c + ANNAIS, family = "binomial", data = Enq)
summary(reg_enq) 

# Interpr?ter :




# A 35 ans ?               ####

# On garde que la ligne des 35ans
A35ans <- A %>% 
  group_by(IDENT) %>% 
  filter(Age == 35) %>% 
  mutate(SO_c = as.factor(ifelse(SO == "Proprietaire", 1, 0)),
         ORIG_SOC_c = as.factor(ifelse(ORIG_SOC %in% c("Classe moyenne ascendante", "Classe moyenne stable"), 
                                       "Moyenne",
                                       ifelse(ORIG_SOC %in% c("Populaire", "Classe populaire descendante"), 
                                              "Populaire","Superieur"))))

table(A35ans$SO)
table(A35ans$SO_c)
table(A35ans$ORIG_SOC)
table(A35ans$ORIG_SOC_c)
table(A35ans$ANNAIS)


# On change la modalit? de r?f?rence pour ORIG_SOC_c
?relevel
A35ans$ORIG_SOC_c <- relevel(A35ans$ORIG_SOC_c, "Moyenne")

# On fait la r?gression
reg_35ans <- glm(SO_c ~ ORIG_SOC_c + ANNAIS, family = "binomial", data = A35ans)
summary(reg_35ans) 

# Interpr?ter :



################

################

# Avec %in%
d2 <- d %>% 
  filter(CSP %in% c("Agric","Arti","ProfLib","Ouvrier"))

# Sans %in%
d2 <- d %>% 
  filter(CSP == "Agric"| CSP =="Arti"| CSP =="ProfLib"| CSP =="Ouvrier")


# C'est surtout utile pour les valeurs num?riques
d2 <- d %>% 
  filter(Taille %in% c(170:180))
# Ne garde que les valeurs comprises entre 170cm et 180cm




