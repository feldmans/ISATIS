#######################################
#                                     #
#     Rapports Isatis par service     #
#                                     #
#######################################



#charger fonctions et librairies
source("C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/scripts ISATIS/ISATIS/fonctions_ISATIS.R")
#charger aussi les objects necessaires:
#source("C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/scripts ISATIS/ISATIS/objects Isatis.R")



#RAPPORT SOCIO AVEC SOCIO ET RESULTAT PAR SERVICE

for(.s in levels(d$service.recode)) {
  #.s <-  "CHIRURGIE DIGESTIVE"
  sink(paste0("./res/1",.s,"_ISATIS.txt"))
  
  cat (paste (.s, "\n\n********************\n\n"))
  cat("DESCRIPTION VARIABLES SOCIO : telephone/internet\n\n")
  fun.socio5(.s,f,d)
  
  cat(paste("RESULTATS SCORES TELEPHONE","\n\n\n"))
  .out <-  fun.score(service=.s, data=f)
  .out <- apply (.out[, -1], 2, function (x) round (as.numeric (x), 2))
  row.names(.out)<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                     "commodité de la chambre","restauration hospitalière","score final")
  print (.out)
  cat("\n\n********************\n\n")
  
  
  cat(paste("RESULTATS SCORES INTERNET","\n\n\n"))
  .out2 <-  fun.score(service=.s, data=d)
  .out2 <- apply (.out[, -1], 2, function (x) round (as.numeric (x), 2))
  row.names(.out2)<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                      "commodité de la chambre","restauration hospitalière","score final")
  print (.out2)
  cat("\n\n********************\n\n")
  
  sink()
}

#DESCRIPTION SOCIO PAR SERVICE
#Enregistre variables socio par service dans un fichier texte
#internet
sink ("./res/internet.service.socio.txt")
sapply (levels (d$service.recode), fun.socio3, data=d)
sink()
#telephone
sink ("./res/telephone.service.socio.txt")
sapply (levels (d$service.recode), fun.socio3, data=f)
sink()
#ensemble
sink ("./res/all.telint.service.socio.txt")
sapply (levels (d$service.recode), fun.socio4, data1=f, data2=d)
sink()


#####################RESULTATS DES THEMES PAR SERVICE

.set<- "C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/rapport par service/"

#NB : pour que le sink "resultats_themes_tous_services.txt" marche, il faut mettre en silence le sink au sein de la boucle
sink (paste0(.set,"resultats_themes_tous_services.txt"))

for (.s in levels (d$service.recode)) {
  
  n2ni <- d %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nri <- d %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Pri <- round(nri/n2ni *100 ,2)
  n2nt <- f %>% filter (Duree>=2) %>% filter(service.recode==.s) %>% summarise(n()) 
  nrt <- f %>% filter (Duree>=2) %>% filter(repondant =="oui" & service.recode==.s) %>% summarise(n()) 
  Prt <- round(nrt/n2nt *100,2)
  n2ntot <- n2ni+n2nt
  nrtot <- nri+nrt
  Prtot <- round(nrtot/n2ntot,2)
  
  MCi.rap <- t(sapply(1:6,function(x)BootMCi.rap (.s,paste0("res.theme",x),200)))
  MCi.rap<- data.frame(rbind(MCi.rap,BootMCi.rap(.s,"res.score.final",200)))
  dimnames<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                        "commodité de la chambre","restauration hospitalière","score global")
  int_tel<-data.frame(dimnames,MCi.rap[,1:4]); colnames(int_tel) <-c("thème","N_int","Mean_95%CI_int","N_tel","Mean_95%CI_tel")
  tot<-data.frame(dimnames,MCi.rap[,5:6]); colnames(tot) <-c("thème","N","Mean_95%CI")
  
  sink(paste0(.set,"ISATIS_",.s,".txt"))
  cat (paste (.s, "\n\n"))
  cat(paste0("Patients hostitalisés au moins 2 nuits consécutives (total/internet/telephone) : ",
             n2ntot,"/",n2ni, "/",n2nt,
             "\nPatients répondeurs (total/internet/telephone) : ",
             nrtot,"(",Prtot,"%)","/",nri,"(",Pri,"%)","/",nrt,"(",Prt,"%)"))
  cat("\n\nScore des patients du service (tous modes d'administration confondus)\n")
  print(tot)
  cat("\n\nDétail par mode d'administration du questionnaire (internet ou téléphone)\n")
  print(int_tel)
  cat ("\n\n\n")
  write.xlsx (MCi.rap, file=paste0(.set,"ISATIS_",.s,".xlsx"), col.names=T,row.names=F, sheetName="resultats ISATIS", append=F)    
  sink()
}
sink()





##################### RESULTATS DES THEMES PAR SERVICE : plus complet (version rendue à gilles) ######################


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







#############RESULTAT DSE THEMES PAR SERVICE : NON UTILISE###################
#plusieurs doc excel et 1 feuille txt
sink ("resultats themes.txt")
for (.s in levels (d$service.recode)) {
  .out <-  fun.score(service=.s, data=d)
  write.xlsx (.out, file=paste0("./res/Isatis.", .s, ".xlsx"), row.names=F, sheetName="resultats themes", append=F)
  cat (paste (.s, "\n"))
  .out <- apply (.out[, -1], 2, function (x) round (as.numeric (x), 3))
  row.names(.out)<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                     "commodité de la chambre","restauration hospitalière","score final")
  print (.out)
  cat ("\n\n")
}
sink()

#plusieurs doc excel : par service, telephone et internet dans une meme feuille 

for (.s in levels (d$service.recode)) {
  
  .out <- fun.score(service=.s,data=f)
  .out2 <-  fun.score(service=.s, data=d)
  #rownames(.out)
  .out$result <-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                  "commodité de la chambre","restauration hospitalière","score final")
  #rownames(.out2)
  .out2$result <-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                   "commodité de la chambre","restauration hospitalière","score final")
  for (.c in colnames (.out)) .out[, .c] <- as.vector (.out[, .c])
  for (.c in colnames (.out2)) .out2[, .c] <- as.vector (.out2[, .c])
  .jumpline <- rep ("", ncol (.out)) #sauter une ligne a la fin du tableau
  .title <- c (.s, rep ("", ncol (.out)-1)) 
  .title2 <- c("Resultat score telephone",rep ("", ncol (.out)-1))
  .title3 <- c("Resultat score internet",rep ("", ncol (.out2)-1))
  
  .bind <- rbind(.title,.jumpline,.title2,colnames(.out),.out,.jumpline,.title3,colnames(.out2),.out2)
  
  write.xlsx (.bind, file=paste0("./res/2Isatis.", .s, ".xlsx"), col.names=F,row.names=F, sheetName="resultats themes", append=F)    
}

#EXPORTER SOUS UNE SEULE FEUILLE excel
.res <- data.frame (do.call (rbind, lapply (levels (d$service.recode), function (.s) {   
  .out <-  fun.score(service=.s, data=d)
  for (.c in colnames (.out)) .out[, .c] <- as.vector (.out[, .c]) #car sinon facteurs et ne peut pas mettre des facteurs l'un sur l'autres
  .out$result <-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                  "commodité de la chambre","restauration hospitalière","score final")    
  .title <- c (.s, rep ("", ncol (.out)-1)) #nom du service
  .jumpline <- rep ("", ncol (.out)) #sauter une ligne a la fin du tableau
  #  browser()
  .out <- rbind (as.character (.title), colnames (.out), .out, .jumpline) #pourquoi as.character?      
})))
write.xlsx (.res, file=paste0("Isatis.themes4.xlsx"), row.names=F, col.names=F, sheetName="resultats themes", append=F)
write.xlsx (.res, file="./res/all.Isatis.themes.xlsx", row.names=F, col.names=F, sheetName="resultats themes", append=F) #pour mettre dans sous dossier .res



#EXPORTER RESULTATS DANS FEUILLE TXT
#telephone
sink ("./res/all.telephone.service.restheme.txt")
cat(paste("RESULTATS TELEPHONE","\n\n\n"))
for (.s in levels (f$service.recode)) {
  cat (paste (.s, "\n"))
  .out <-  fun.score(service=.s, data=f)
  .out <- apply (.out[, -1], 2, function (x) round (as.numeric (x), 2))
  rownames(.out)<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                    "commodité de la chambre","restauration hospitalière","score final")
  print (.out)
  cat ("\n\n")
}
sink()

#internet
sink ("./res/all.internet.service.restheme.txt")
cat(paste("RESULTATS INTERNET","\n\n\n"))
for (.s in levels (d$service.recode)) {
  cat (paste (.s, "\n"))
  .out <-  fun.score(service=.s, data=d)
  .out <- apply (.out[, -1], 2, function (x) round (as.numeric (x), 2))
  rownames(.out)<-c("Prise en charge globale","information du patient","communication avec professionnels","attitude des professionnels",
                    "commodité de la chambre","restauration hospitalière","score final")
  print (.out)
  cat ("\n\n")
}
sink()
