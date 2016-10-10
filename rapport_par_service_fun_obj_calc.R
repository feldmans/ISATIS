      ########################
      # Rapports par service #
      ########################

library (xlsx)
library(rJava)
library(car)
library(RMySQL)
library(DBI)
library(Hmisc)
library(gtools) #smartbind
library(psy)
library (dplyr)
library (boot)



#############FONCTIONS####################### 

#############pour le décodage des scores de satisfaction
decode_answer2 <- function(answer_ori)
  return(as.numeric (ifelse (substr (answer_ori, 2, 2)=="_", substr (answer_ori, 1, 1), substr (answer_ori, 1, 2))))
#Fonction sans return : decode_answer2 <- function(answer_ori) as.numeric (ifelse (substr (answer_ori, 2, 2)=="_", substr (answer_ori, 1, 1), substr (answer_ori, 1, 2)))

#############calcul des scores de satisfaction et IC par service
#pour test:
#.theme<-
#.dat<-.df
#.service<-"CHIRURGIE DIGESTIVE"

fun.mean <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  #.int <- .dat[.dat$group=="int", "score"]
  #.tel <- .dat[.dat$group=="tel", "score"]
  mymean<-by(.dat$score,.dat$group,mean)
  return(mymean)
}

fun.meantot <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  mymeantot <- mean(.dat$score)
  return(mymeantot)
}

#Je cree la fonction boot. 
#le tableau .df defini l'argument data de la fonction fun mean. 
#boot repete la fonction fun.mean R fois : il genere un nouveau .df[indices,] et fait la moyenne. 
boot.theme.rap <- function (.service,.theme,R){
  .df <- data.frame (
    score=c (d[d$service.recode==.service, .theme], f[f$service.recode==.service, .theme]),
    group=c (rep ("int", nrow (d[d$service.recode==.service,])), rep ("tel", nrow (f[f$service.recode==.service,])))
  )
  .res <- boot(data=.df,statistic=fun.mean,R=R)
  #meantot <- mean(.df$score, na.rm=T)
  .restot <- boot(data=.df, statistic=fun.meantot,R=R)
  #return(list(.res,meantot))
  return(list(.res,.restot))
}

#Je cree une fonction BootMCi qui reintegre resultat de boot.theme et ajoute les IC et met en forme
BootMCi.rap <- function (.service,.theme, R) {
  #  browser()
  .boottemp <- boot.theme.rap (.service=.service,.theme=.theme, R=R)
  .bootres <-.boottemp[[1]]
  .restot <-.boottemp[[2]]
  ############
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0) #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .res$n<-c(sum(!is.na(d[d$service.recode==.service,.theme])),sum(!is.na(f[f$service.recode==.service,.theme]))) #true(non manquant) vaut 1, donc nb de non NA
  ###########
  .n <- length (.restot$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.restot,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .tot <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.tot) <- "tot" #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.tot) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .tot$est <- as.numeric (.restot$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .tot$n<-sum(!is.na(d[d$service.recode==.service,.theme]))+ sum(!is.na(f[f$service.recode==.service,.theme])) #true(non manquant) vaut 1, donc nb de non NA
  ############
  .res <- rbind(.res,.tot)
  .res <- .res[, c (4,3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, meanCI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ], total=.ans[3,]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #.ans <- c(service=.service,.ans) #pour avoir nom du service mais lourd
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}

#################calcul des scores de satisfaction et IC tous services confondus
#Je cree la fonction boot. Les fonctions stat fun.mean et fun.meantot sont inchangées
boot.theme.rap.tot <- function (.theme,R){
  .df <- data.frame (
    score=c (d[, .theme], f[, .theme]),
    group=c (rep ("int", nrow (d)), rep ("tel", nrow (f)))
  )
  .res <- boot(data=.df,statistic=fun.mean,R=R)
  #meantot <- mean(.df$score, na.rm=T)
  .restot <- boot(data=.df, statistic=fun.meantot,R=R)
  #return(list(.res,meantot))
  return(list(.res,.restot))
}

#Je cree une fonction BootMCi qui reintegre resultat de boot.theme et ajoute les IC et met en forme
BootMCi.rap.tot <- function (.theme, R) {
  #  browser()
  .boottemp <- boot.theme.rap.tot (.theme=.theme, R=R)
  .bootres <-.boottemp[[1]]
  .restot <-.boottemp[[2]]
  
  ############tableau pour int et tel séparé
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0) #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .res$n<-c(sum(!is.na(d[,.theme])),sum(!is.na(f[,.theme]))) #true(non manquant) vaut 1, donc nb de non NA
  
  ###########tableau pour int et tel confondus
  .n <- length (.restot$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.restot,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .tot <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.tot) <- "tot" #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.tot) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .tot$est <- as.numeric (.restot$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .tot$n<-sum(!is.na(d[,.theme]))+ sum(!is.na(f[,.theme])) #true(non manquant) vaut 1, donc nb de non NA
  
  ############réunion des tableaux
  .res <- rbind(.res,.tot)
  .res <- .res[, c (4,3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, meanCI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ], total=.ans[3,]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #.ans <- c(service=.service,.ans) #pour avoir nom du service mais lourd
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}


############## CALCUL DES SCORES #################

#si gilles : Attention il faudra reenregistrer les tables sous csv
#filenameinternet<-"/Users/gilles_travail/Documents/Travauxscientifiques/SarahFeldman_Isatis/patients_Internet_150202_0955_Isatis.xlsx"
#filenametelephone<-"/Users/gilles_travail/Documents/Travauxscientifiques/SarahFeldman_Isatis/patients_telephone_150202_0955_Isatis.xlsx"

#si Sarah
#filenameinternet<-"/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/questionnaires ISATIS/patients_Internet_150202_0955_complet_Isatis.xlsx"
#filenametelephone<-"/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/questionnaires ISATIS/patients_telephone_150202_0955_Isatis_original.xlsx"

# d <- read.xlsx (filenameinternet, sheetName="patients_internet_150202_0952.c")
# g <- read.xlsx(filenametelephone, sheetName="patients_telephone_150202_0955.")
# f <- g[g$Groupe%in%"T",]
# rm(g)

####################################################################
#VARIABLES INTERNET

d$nitems <- rowSums(!is.na(d[,paste0("ISatis_Q",1:32)])) ;table(d$nitems)

a <- as.Date(d$Date_Entree, format = "%j/%m/%y")
b<- as.Date(d$Date_Sortie, format = "%j/%m/%y");
d$Duree <- b-a
d$Duree <- as.numeric(d$Duree)
d$Date_Sortie2 <- as.Date(d$Date_Sortie, format = "%j/%m/%y")
d$Isatis_Date2 <- as.Date(d$ISatis_Date, format = "%j/%m/%y")
d$groupe <- rep("int",nrow(d))

#Repondant = a repondu à 16 Questions ou plus. non repondant = reponse a moins de 16 questions  
#d$repondant <- ifelse(d$ISATIS%in%"Complet","oui","non")
d$repondant <- ifelse (rowSums(!is.na(d[,paste0("ISatis_Q",1:32)]))>15,"oui",NA)
d$repondant <- ifelse (rowSums(!is.na(d[,paste0("ISatis_Q",1:32)]))<=15,"non",d$repondant)

d$nonHDJ <- ifelse(d$Duree>=2 & d$repondant%in%"oui","repondant",NA) #les repondant sont non HDJ et complet
d$nonHDJ <- ifelse(d$Duree>=2 & d$repondant%in%"non","non_repondant",d$nonHDJ)#les non repondant sont non HDJ et incomplet

d$service.recode <- d$Service
d$service.recode<- recode(d$service.recode,"'1_Chir_Dig'='CHIRURGIE DIGESTIVE';'2_Gastro'='GASTRO';'3_Hepato'='HEPATO';'4_Maladies_Inf'='MALADIES INFECTIEUSES';'5_Med_Int'='MEDECINE INTERNE'")

#calcul des scores
for(i in 1:32) d[,paste0("decQ",i)] <- decode_answer2(d[,paste0("ISatis_Q",i)]) #decode...2 = as num
for(i in 1:32) d[ ,paste0("val.decQ",i)] <- recode(d[ ,paste0("decQ",i)],"1=0;2=25;3=50;4=75;5=100;6:11=NA;0=NA")
vI1 <- d[ ,paste0("val.decQ",c(1,2,4,13:15))]
vI2 <- d[ ,paste0("val.decQ",c(16,18,27:30))]
vI3 <- d[ ,paste0("val.decQ",c(3,5,6,17,20))]
vI4 <- d[ ,paste0("val.decQ",c(7:11))]
vI5 <- d[ ,paste0("val.decQ",c(21:24))]
vI6 <- d[ ,paste0("val.decQ",c(25,26))]

for (x in 1:6) assign (paste0 ("nb.quest", x), rowSums(!is.na(get(paste0("vI",x))))) #On ne peut créer plusieurs pbjets qu'avec une boucle

#THEME CALCULE UNIQUEMENT SI DUREE >=2, ET SI PLUS DE 15 QUESTIONS REPONDUES
#Un patient avec que des NA pour res.theme est soit un HDJ soit un non repondant (moins de 16 questions repondues)
d[ ,paste0("theme.valid", 1:4)] <- sapply (1:4, function (x) ifelse(get(paste0("nb.quest",x))>=3 & d$Duree>=2 & d$repondant %in% "oui", "valide", "non"))
d[ ,paste0("theme.valid", 5:6)] <- sapply (5:6, function (x) ifelse(get(paste0("nb.quest",x))>=2 & d$Duree>=2 & d$repondant %in% "oui", "valide", "non"))

d[ ,paste0("res.theme", 1:6)] <- sapply (1:6, function(x)ifelse( d[,paste0("theme.valid",x)]%in%"valide",rowMeans(get(paste0("vI",x)),na.rm=TRUE),NA))
vItot <- d[,paste0("res.theme", 1:6)]


# d$score.valid <- ifelse(d$theme.valid1%in%"valide" & d$theme.valid2%in%"valide" & d$theme.valid3%in%"valide"
#                         & d$theme.valid4%in%"valide" & d$theme.valid5%in%"valide" & d$theme.valid6%in%"valide","valide","non")
# d$res.score.final<- ifelse (d$score.valid%in%"valide",rowMeans(d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )
d$score.valid <- rowSums (d[ ,paste0("theme.valid", 1:6)] == "valide") == 6 #TRUE si aucune reponse non a "theme valide"
d$res.score.final<- ifelse (d$score.valid,rowMeans(d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )

#découpage en quantile 
d$quant_del <- NA
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2<3,1, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2 >= 3,2, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2>=6,3, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2 >= 15.75,4,d$quant_del)

####################################################################################
#VARIABLES TELEPHONE
f$nitems <- rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))

a.tel <- as.Date(f$Date_Entree, format = "%j/%m/%y")
b.tel<- as.Date(f$Date_Sortie, format = "%j/%m/%y")
f$Duree <- as.numeric(b.tel-a.tel)
f$Date_Sortie2 <- as.Date(f$Date_Sortie, format = "%j/%m/%y")
f$Isatis_Date2 <- as.Date(f$ISatis_Date, format = "%j/%m/%y")
f$groupe <- rep("tel",nrow(f))

#Repondant = a repondu à 16 Questions ou plus. non repondant = reponse a moins de 16 questions 
#f$repondant <- ifelse(f$ISATIS%in%"Complet","oui","non") #code en complet/incomplet/non sans savoir def de chaque codage.
f$repondant <- ifelse (rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))>15,"oui",NA)
f$repondant <- ifelse (rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))<=15,"non",f$repondant)

f$nonHDJ <- ifelse(f$Duree>=2 & f$repondant%in%"oui","repondant",NA)
f$nonHDJ <- ifelse(f$Duree>=2 & f$repondant%in%"non","non_repondant",f$nonHDJ)
f$service.recode <- f$Service
f$service.recode<- recode(f$service.recode,"'1_Chir_Dig'='CHIRURGIE DIGESTIVE';'2_Gastro'='GASTRO';'3_Hepato'='HEPATO';'4_Maladies_Inf'='MALADIES INFECTIEUSES';'5_Med_Int'='MEDECINE INTERNE'")

#Calcul score
for(i in 1:32) f[,paste0("decQ",i)] <- decode_answer2(f[,paste0("ISatis_Q",i)])
for(i in 1:32) f[ ,paste0("val.decQ",i)] <- recode(f[ ,paste0("decQ",i)],"1=0;2=25;3=50;4=75;5=100;6:11=NA;0=NA")
vT1 <- f[ ,paste0("val.decQ",c(1,2,4,13:15))]
vT2 <- f[ ,paste0("val.decQ",c(16,18,27:30))]
vT3 <- f[,paste0("val.decQ",c(3,5,6,17,20))]
vT4 <- f[ ,paste0("val.decQ",c(7:11))]
vT5 <- f[ ,paste0("val.decQ",c(21:24))]
vT6 <- f[,paste0("val.decQ",c(25,26))]

for (x in 1:6) assign (paste0 ("nb.quest.tel", x), rowSums(!is.na(get(paste0("vT",x))))) 

#THEME CALCULE UNIQUEMENT SI DUREE >=2, ET SI PLUS DE 15 QUESTIONS REPONDUES
f[ ,paste0("theme.valid", 1:4)] <- sapply (1:4, function (x) ifelse(get(paste0("nb.quest.tel",x))>=3 & f$Duree>=2 & f$repondant %in% "oui", "valide", "non"))
f[ ,paste0("theme.valid", 5:6)] <- sapply (5:6, function (x) ifelse(get(paste0("nb.quest.tel",x))>=2 & f$Duree>=2 & f$repondant %in% "oui", "valide", "non"))
f[ ,paste0("res.theme", 1:6)] <- sapply (1:6, function(x)ifelse( f[,paste0("theme.valid",x)]%in%"valide",rowMeans(get(paste0("vT",x)),na.rm=TRUE),NA))
vTtot <- f[,paste0("res.theme", 1:6)]

f$score.valid <- rowSums (f[ ,paste0("theme.valid", 1:6)] == "valide") == 6 #TRUE si aucune reponse non a "theme valide"
f$res.score.final<- ifelse (f$score.valid,rowMeans(f[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )



##################### RESULTATS DES THEMES PAR SERVICE ######################

.set<- "C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/rapport par service/"

#NB : pour que le sink "resultats_themes_tous_services.txt" marche, il faut mettre en silence le sink au sein de la boucle
#Donc impression une page par service(avec sink dans la boucle) puis tous les services sur la même page (en retirant sink de la boucle)

######################################

#CALCUL DES SCORES DE SATISFACTION
set.seed(1234)

#satisfaction tous services confondus: je rappellerai les objets crées dans la boucle (mais je ne les crées qu'une seule fois)
MCi.rap.tot <- t(sapply(1:6,function(x)BootMCi.rap.tot (paste0("res.theme",x),2000)))
MCi.rap.tot<- data.frame(rbind(MCi.rap.tot,BootMCi.rap.tot("res.score.final",2000)))
dimnames<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
            "commodité de la chambre","restauration hospitalière","score global")
int_tel.tot<-data.frame(dimnames,MCi.rap.tot[,1:4]); colnames(int_tel.tot) <-c("thème","int_N","int_Mean[95%CI]","tel_N","tel_Mean[95%CI]")
tot.tot<-data.frame(dimnames,MCi.rap.tot[,5:6]); colnames(tot.tot) <-c("thème","N","Mean[95%CI]")

for (.s in levels (d$service.recode)) {
  MCi.rap <- t(sapply(1:6,function(x)BootMCi.rap (.s,paste0("res.theme",x),2000)))
  MCi.rap<- data.frame(rbind(MCi.rap,BootMCi.rap(.s,"res.score.final",2000)))
  
  dimnames<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
              "commodité de la chambre","restauration hospitalière","score global")
  int_tel<-data.frame(dimnames,MCi.rap[,1:4]); colnames(int_tel) <-c("thème","int_N","int_Mean[95%CI]","tel_N","tel_Mean[95%CI]")
  assign(paste0("int_tel",.s),int_tel)
  tot <- data.frame(dimnames,MCi.rap[,5:6]); colnames(tot) <-c("thème","N","Mean[95%CI]")
  assign(paste0("tot",.s),tot)
  write.xlsx (MCi.rap, file=paste0(.set,"ISATIS_",.s,".xlsx"), col.names=T,row.names=F, sheetName="resultats ISATIS", append=F)    
}

#RAPPORTS : une page par service
for (.s in levels (d$service.recode)) {
  
  #####calculs par service
  n2ni <- d %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nri <- d %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Pri <- round(nri/n2ni *100 ,2)
  n2nt <- f %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nrt <- f %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Prt <- round(nrt/n2nt *100,2)
  n2ntot <- n2ni+n2nt
  nrtot <- nri+nrt
  Prtot <- round(nrtot/n2ntot*100,2)
  
  # MCi.rap <- t(sapply(1:6,function(x)BootMCi.rap (.s,paste0("res.theme",x),2000)))
  # MCi.rap<- data.frame(rbind(MCi.rap,BootMCi.rap(.s,"res.score.final",2000)))
  # 
  # dimnames<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
  #             "commodité de la chambre","restauration hospitalière","score global")
  # int_tel<-data.frame(dimnames,MCi.rap[,1:4]); colnames(int_tel) <-c("thème","int_N","int_Mean[95%CI]","tel_N","tel_Mean[95%CI]")
  # assign(paste0("int_tel",.s),int_tel)
  # tot <- data.frame(dimnames,MCi.rap[,5:6]); colnames(tot) <-c("thème","N","Mean[95%CI]")
  # assign(paste0("tot",.s),tot)
  # write.xlsx (MCi.rap, file=paste0(.set,"ISATIS_",.s,".xlsx"), col.names=T,row.names=F, sheetName="resultats ISATIS", append=F)    
  
  int_tel<-get(paste0("int_tel",.s))  #la satisfaction par service a déjà été calculée à l'étape précédente
  tot<-get(paste0("tot",.s))          #la satisfaction par service a déjà été calculée à l'étape précédente
  
  ###RAPPORT
  sink(paste0(.set,"ISATIS_",.s,".txt"))
  
  ###rapport par service
  cat (paste (.s, "\n\n"))
  cat(paste0("Patients hostitalisés au moins 2 nuits consécutives (total/internet/telephone) : ",
             n2ntot,"/",n2ni, "/",n2nt,
             "\nPatients répondeurs (total/internet/telephone) : ",
             nrtot,"(",Prtot,"%)","/",nri,"(",Pri,"%)","/",nrt,"(",Prt,"%)"))
  cat("\n\nScore des patients du service (tous modes d'administration confondus)\n")
  print(tot,right=F)
  cat("\n\nDétail par mode d'administration du questionnaire (internet ou téléphone)\n")
  print(int_tel,right=F) #aligne les noms des colonnes à gauche
  cat ("\n\n\n")
  ###
  
  #fréquence tous services confondus: je les crées ici pour éviter de changer tous les noms (très rapide de les recréer à chaque fois)
  n2ni <- d %>% filter (Duree>=2) %>% summarise(n()) 
  nri <- d %>% filter (Duree>=2) %>% filter(repondant =="oui" ) %>% summarise(n()) 
  Pri <- round(nri/n2ni *100 ,2)
  n2nt <- f %>% filter (Duree>=2)  %>% summarise(n()) 
  nrt <- f %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% summarise(n()) 
  Prt <- round(nrt/n2nt *100,2)
  n2ntot <- n2ni+n2nt
  nrtot <- nri+nrt
  Prtot <- round(nrtot/n2ntot*100,2)
  
  ###rapport tous services confondus
  cat ("\n\n\n")
  cat("Résultats tous services confondus: \n\n")
  cat(paste0("Patients hostitalisés au moins 2 nuits consécutives (total/internet/telephone) : ",
             n2ntot,"/",n2ni, "/",n2nt,
             "\nPatients répondeurs (total/internet/telephone) : ",
             nrtot,"(",Prtot,"%)","/",nri,"(",Pri,"%)","/",nrt,"(",Prt,"%)"))
  cat("\n\nScore des patients tous services et tous modes d'administration confondus)\n")
  print(tot.tot,right=F)
  cat("\n\nDétail par mode d'administration du questionnaire (internet ou téléphone)\n")
  print(int_tel.tot,right=F) #aligne les noms des colonnes à gauche
  ###
  
  sink()
}

###################################
#RAPPORTS : tous les services sur la même page


######satisfaction tous services confondus (attention l'ordre est important par rapport au seed : calcul tous confondus puis calcul par service)
#déjà calculé: MCi.rap.tot splité en int_tel.tot et tot.tot

######ouverture du rapport
sink (paste0(.set,"resultats_themes_tous_services.txt"))

######calculs et rapport par service
set.seed(1234)
for (.s in levels (d$service.recode)) {
  #calculs par service
  n2ni <- d %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nri <- d %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Pri <- round(nri/n2ni *100 ,2)
  n2nt <- f %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nrt <- f %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Prt <- round(nrt/n2nt *100,2)
  n2ntot <- n2ni+n2nt
  nrtot <- nri+nrt
  Prtot <- round(nrtot/n2ntot*100,2)
  
  int_tel<-get(paste0("int_tel",.s))  #la satisfaction par service a déjà été calculée à l'étape précédente
  tot<-get(paste0("tot",.s))          #la satisfaction par service a déjà été calculée à l'étape précédente

  #rapport par service (sink hors de la boucle et pas d'écriture de fichier xl)
  cat (paste (.s, "\n\n"))
  cat(paste0("Patients hostitalisés au moins 2 nuits consécutives (total/internet/telephone) : ",
             n2ntot,"/",n2ni, "/",n2nt,
             "\nPatients répondeurs (total/internet/telephone) : ",
             nrtot,"(",Prtot,"%)","/",nri,"(",Pri,"%)","/",nrt,"(",Prt,"%)"))
  cat("\n\nScore des patients du service (tous modes d'administration confondus)\n")
  print(tot,right=F)
  cat("\n\nDétail par mode d'administration du questionnaire (internet ou téléphone)\n")
  print(int_tel,right=F) #aligne les noms des colonnes à gauche
  cat ("\n\n\n")
}

######rapports tous services confondus: je les crés ici pour éviter qu'il soit écrasé par fréquence par service

n2ni <- d %>% filter (Duree>=2) %>% summarise(n()) 
nri <- d %>% filter (Duree>=2) %>% filter(repondant =="oui" ) %>% summarise(n()) 
Pri <- round(nri/n2ni *100 ,2)
n2nt <- f %>% filter (Duree>=2)  %>% summarise(n()) 
nrt <- f %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% summarise(n()) 
Prt <- round(nrt/n2nt *100,2)
n2ntot <- n2ni+n2nt
nrtot <- nri+nrt
Prtot <- round(nrtot/n2ntot*100,2)

cat ("\n\n\n")
cat("Résultats tous services confondus: \n\n")
cat(paste0("Patients hostitalisés au moins 2 nuits consécutives (total/internet/telephone) : ",
           n2ntot,"/",n2ni, "/",n2nt,
           "\nPatients répondeurs (total/internet/telephone) : ",
           nrtot,"(",Prtot,"%)","/",nri,"(",Pri,"%)","/",nrt,"(",Prt,"%)"))
cat("\n\nScore des patients tous services et tous modes d'administration confondus)\n")
print(tot.tot,right=F)
cat("\n\nDétail par mode d'administration du questionnaire (internet ou téléphone)\n")
print(int_tel.tot,right=F) #aligne les noms des colonnes à gauche

#####fermeture du rapport
sink()

