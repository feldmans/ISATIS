#########################
#                       #
#     CALCULS ISATIS    #
#                       #
#########################



#charger fonctions et librairies
source("C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/scripts ISATIS/ISATIS/fonctions ISATIS.R") 
#charger aussi les objects necessaires:
source("C:/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/scripts ISATIS/ISATIS/objects Isatis.R")


#CB DE PATIENTS REPONDENT A COMBIEN D'ITEMS

#Liste des modalites de reponse par question (liste 1 = question 1)
listQrep<-sapply(1:32,function(i)print(names(table(d[,paste0("ISatis_Q",i)]))))
grep("1_",listQrep)#dit dans quelle liste se trouve le terme
# listrep<-sapply(c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26),function(x)levels(d[,paste0("ISatis_Q",x)]))
# table(unlist(listrep))

#Pour chaque question, combien de patient ont repondu
class_fun <- function (.dat){
  classement_rep<- function (x) {
    Q <-as.factor(.dat[, paste0("decQ",x)])
    Q <-Q[.dat$Duree>=2]
    Q<-table(Q,useNA = "a")
    #Q <- table(.dat[.dat$Duree>=2,paste0("decQ",x)],useNA="a")
    nbNA <- sum(tail(Q,1))
    nbrep <- sum(Q)-sum(tail(Q,1))
    nbconcerne <- sum(Q[1:5])
    nbnonconcerne <- sum(Q[-c(1:5)])-sum(tail(Q,1))
    dfQ<- data.frame(theme = "t", question = x, missing = nbNA, non_concerned = nbnonconcerne, concerned = nbconcerne, non_missing = nbrep,N_calculated_theme = "N")
    return(dfQ)
  }
  listeQ<-c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26)
  nbQrep<-data.frame(t(sapply( listeQ, function(x)classement_rep(x))))
  nbQrep$theme<-rep(c(1,2,3,4,5,6), c(6,6,5,5,4,2))
  nbQrep$N_calculated_theme <- rep(apply(.dat[,paste0("res.theme",1:6)],2,function(x)sum(!is.na(x))), c(6,6,5,5,4,2))
  for (.c in colnames (nbQrep)) nbQrep[, .c] <- unlist (nbQrep[, .c])
  write.table(print(nbQrep),file="clipboard",sep="\t",dec=",",row.names=F)
}

class_fun(d)


#SUIVI DES PATIENTS : MISSING/NON CONCERNE/OK

listeQ<-c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26)
listtheme<-rep(c("id","groupe",1,2,3,4,5,6), c(1,1,6,6,5,5,4,2))

for(i in 1:32) d[ ,paste0("decQ_class",i)] <- recode(d[ ,paste0("decQ",i)],"1:5='ok';6:11='nonconc';0='nonconc'")
d_pat <-d[d$Duree>=2 ,c("id","groupe",paste0("decQ_class",listeQ))]
names(d_pat)[c(-1,-2)]<-paste0("Q",substr(names(d_pat),11,length(names(d_pat))),"_theme",listtheme)[c(-1,-2)]

for(i in 1:32) f[ ,paste0("decQ_class",i)] <- recode(f[ ,paste0("decQ",i)],"1:5='ok';6:11='nonconc';0='nonconc'")
f_pat <-f[f$Duree>=2 ,c("id","groupe",paste0("decQ_class",listeQ))]
names(f_pat)[c(-1,-2)]<-paste0("Q",substr(names(f_pat),11,length(names(f_pat))),"_theme",listtheme)[c(-1,-2)]

ITpat<-rbind(d_pat,f_pat)
ITpat$decQ12<- c(d[d$Duree>=2,"decQ12"],f[f$Duree>=2,"decQ12"]) ; names(ITpat)[names(ITpat) == 'decQ12'] <- 'Q12'
ITpat$decQ19<- c(d[d$Duree>=2,"decQ19"],f[f$Duree>=2,"decQ19"]) ; names(ITpat)[names(ITpat) == 'decQ19'] <- 'Q19'

#changer ordre des colonnes (mettre 12 et 19 avant 13 et 20):
ITpat<-ITpat[,c(1:5,31,6:18,32,19:30)]

#rajouter les colonnes resultat
for(i in 1:6) ITpat[ ,paste0("result_theme",i)] <- c(d[d$Duree>=2,paste0("res.theme",i)],f[f$Duree>=2,paste0("res.theme",i)])
#ITpat[ ,paste0("result_themebis",1:6)]<- sapply(1:6,function(i)c(d[d$Duree>=2,paste0("res.theme",i)],f[f$Duree>=2,paste0("res.theme",i)]))
ITpat$global_score<-c(d[d$Duree>=2,"res.score.final"],f[f$Duree>=2,"res.score.final"])

write.xlsx(ITpat, file=paste(getwd(), "/patientflow20162107.xlsx", sep=""), sheetName="Sheet1",
           col.names=TRUE, row.names=F, append=FALSE, showNA=F)

#Legende des reponses aux questions 12 et 19 
table(ITpat$Q12) # 1: concerne 2: non concerne, sauter question 13, 10: ne souhaite pas repondre
table(ITpat$Q19) # 1: concerne 2: non concerne, sauter question 20, 10: ne souhaite pas repondre

#COMPTE DSE NA OK ET NC

# count_NA<- function (x){
#   .row<-x
#   .row_count<-ifelse(is.na(.row),1,0)
#   sumNA<-rowSums(.row_count)
#   return(sumNA)
# }
# count_ok<- function (x){
#      .row<-x
#     .row_count<-ifelse(.row=="ok",1,0)
#      sumok<-rowSums(.row_count,na.rm=T)
#      return(sumok)
# }
# count_NC<-function (x){
#   .row<-x
#   .row_count<-ifelse(.row=="nonconc",1,0)
#   sumok<-rowSums(.row_count,na.rm=T)
#   return(sumok)
# }
# 
# ITpat$sumNA<-sapply(1:nrow(ITpat),function (x)count_NA(ITpat[x,]))
# ITpat$sumok<-sapply(1:nrow(ITpat),function (x)count_ok(ITpat[x,]))
# ITpat$sumNC<-sapply(1:nrow(ITpat),function (x)count_NC(ITpat[x,]))



#CARACTERISTIQUES DES PATIENTS
#cf fonctions Des... et variables r1 ? 8 dans intro
write.table(print(rbind(r1,r8,r2,r3,r4,r5,r6,r7)),file="clipboard",sep="\t",dec=",",row.names=TRUE)

#verif des legendes
summary (d$socio1)
summary (d[d$Duree>=2 & d$repondant%in%"non", "socio1"])
table (d[d$Duree>=2,"socio3"],d[d$Duree>=2, "repondant"]) #remplace socio pour chaque caracteristique et verif avec tableau du dessus

#FLOW CHART

#Les repondants ont-ils bien repondu a toutes les questions? A combien d'item ont repondu les non repondants?
table(rowSums(!is.na(d[,paste0("ISatis_Q",1:32)][d$Duree>=2,])),d$repondant[d$Duree>=2])
table(rowSums(!is.na(f[,paste0("ISatis_Q",1:32)][f$Duree>=2,])),f$repondant[f$Duree>=2])
#sortir les 2 telephones qui n'ont repondu qu'à 2 et 11 questions
f[rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))==2 & f$Duree>=2,"id"]
f[f$id=="46706z",]
f[rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))==11 & f$Duree>=2,"id"]
f[f$id=="48154h",]


#nombre de patients
#nombre total
length(d$id)
length (f$Groupe)
#nombre de plus de 2 nuits
table(d$Duree>=2)
table(f$Duree>=2)
#repondeurs par groupe
table(d$Duree>=2, d$repondant)
table(f$Duree>=2, f$repondant)
#N par service
#eligible
table(d$Service) + table(f$Service)
#par groupe
table(d$Service)
table(f$Service)
#par responders et par groupe pour duree >=2 nuits
.grpfc <- d %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% group_by(Service) %>% summarise(n())
colSums(.grpfc[,2]) #verif : la somme vaut bien le nb de responders internet
.intfc <- as.numeric(unlist(.grpfc[2]))
.grpfc <- f %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% group_by(Service) %>% summarise(n())
colSums(.grpfc[,2]) #verif : la somme vaut bien le nb de responders internet
.telfc <- as.numeric(unlist(.grpfc[2]))
.intfc+.telfc
#type d'hospit
.a<-table(d$Duree>=2, d$Type_Hospi)
.b<-table(f$Duree>=2, f$Type_Hospi)
.a+.b
table(f[f$Duree>=2 & f$Type_Hospi=="HC","repondant"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC","repondant"])
table(f[f$Duree>=2 & f$Type_Hospi=="HDS","repondant"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS","repondant"])
#nb d'item completes
table(rowSums(!is.na(f[ f$Duree>=2 & f$Type_Hospi=="HC", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(d[ d$Duree>=2 & d$Type_Hospi=="HC", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(f[ f$Duree>=2 & f$Type_Hospi=="HDS", paste0("ISatis_Q",1:32)]))) + table(rowSums(!is.na(d[ d$Duree>=2 & d$Type_Hospi=="HDS", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(f[ f$Duree>=2, paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(d[ d$Duree>=2, paste0("ISatis_Q",1:32)])))
#nombre de patients par service selon type hospit (pour duree >= 2)
HCtot <- data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HC","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC" ,"Service"]))
HCresp <- data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HC" & f$repondant=="oui","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC" & d$repondant=="oui","Service"]))
HDStot <-data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HDS","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS","Service"]))
HDSresp <-data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HDS"& f$repondant=="oui","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS"& d$repondant=="oui","Service"]))
serv.names<- c("Gastrointestinal surgery","Gastroentorology","Hepatology","Infectious diseases","internal medicine")

resHC<-paste0(HCresp[,2]," ",serv.names,"(",round(HCresp[,2]/HCtot[,2]*100,1),"%, ",HCresp[,2],"/",HCtot[,2],")")
cat(paste(resHC,collapse="\n"))
resHDS <- paste0(HDSresp[,2]," ",serv.names,"(",round(HDSresp[,2]/HCtot[,2]*100,1),"%, ",HDSresp[,2],"/",HDStot[,2],")")
cat(paste(resHDS,collapse="\n"))

nrow(f[f$repondant=="oui" & f$Duree>=2 & f$Type_Hospi=="HC",])#equivalent a table(f$nonHDJ) et table(f$repondant,f$Duree>=2)

nrow(d[d$repondant=="oui" & d$Duree>=2,]) #equivalent a table(d$nonHDJ) et table(d$repondant,d$Duree>=2)


#ANALYSES SOCIO


#TESTS
num_col <- paste0("socio",c(1,3,6,8,2,4,5,7))
myname_col <- c("Age","Education","Revenu","Assurance","Sexe","Statut marital","Emploi","Profession")
name_test <- c(rep("Wilcoxon",4),rep("fisher",4))

#tableau test internet
s1i <- sapply(c(1,3,6,8), function(x) wilcox.test(d[d$nonHDJ%in%"non_repondant", paste0("socio",x)],d[d$nonHDJ%in%"repondant",paste0("socio",x)])$p.value)
dti <- sapply(c(2,4,5,7), function(x) fisher.test(table(d$nonHDJ,d[,paste0("socio",x)]),workspace = 800000000)$p.value)
s1_8i<- data.frame(c(s1i,dti))
tabi <- cbind(num_col,myname_col,s1_8i,name_test)
colnames(tabi)<-c("socio","variable","p.value rep/nrep","test")
write.table(print(tabi),file="clipboard",sep="\t",dec=".",row.names=FALSE)

#tableau test telephone
s1t <- sapply(c(1,3,6,8), function(x) wilcox.test(f[f$nonHDJ%in%"non_repondant", paste0("socio",x)],f[f$nonHDJ%in%"repondant",paste0("socio",x)])$p.value)
dtt <- sapply(c(2,4,5,7), function(x) fisher.test(table(f$nonHDJ,f[,paste0("socio",x)]),workspace = 800000000)$p.value)
s1_8t<- data.frame(c(s1t,dtt))
tabt <- cbind(num_col,myname_col,s1_8t,name_test)
colnames(tabt)<-c("socio","variable","p.value rep/nrep","test")
write.table(print(tabt),file="clipboard",sep="\t",dec=".",row.names=FALSE)


#repI/repT
s1it <- sapply(c(1,3,6,8),function(x)wilcox.test(d[d$nonHDJ%in%"repondant", paste0("socio",x)],f[f$nonHDJ%in%"repondant", paste0("socio",x)])$p.value)
dtit <- sapply(c(2,4,5,7), function(x) fisher.test(
  smartbind(table(d[d$nonHDJ=="repondant",paste0("socio",x)]),table(f[f$nonHDJ=="repondant",paste0("socio",x)]),fill=0),
  workspace=100000000)$p.value)
s1_8it<- data.frame(c(s1it,dtit))
tab_it <- cbind(num_col,myname_col,s1_8it,name_test)
colnames(tab_it)<-c("socio","num","p.value repI/repT")
write.table(print(tab_it),file="clipboard",sep="\t",dec=".",row.names=FALSE)



#MEAN IC DES THEMES ET SCORES FINAUX PAR BOOTSTRAP
set.seed(123) 
#N,moyenne(IC) des themes
MCi.theme <- t(sapply(1:6,function(x)BootMCi (paste0("res.theme",x),20000)))
MCi.theme<- data.frame(rbind(MCi.theme,BootMCi("res.score.final",20000)))
# 
#Effect size
ES.theme <- sapply(1:6,function(x)BootCi(.theme=paste0("res.theme",x),R=20000))
ES.theme <- data.frame(c(ES.theme,BootCi(.theme="res.score.final",R=20000))) ;names(ES.theme)<- "Effect size"
# Effect size en passant par log avec retour a l'expo ensuite
#  ES.theme <- sapply(1:6,function(x)BootCi.log(.theme=paste0("res.theme",x),R=2000))
#  ES.theme <- data.frame(c(ES.theme,BootCi.log(.theme="res.score.final",R=2000))) ;names(ES.theme)<- "Effect size"

# #Effect size pour les themes 5 et 6 (les 2 posant pb) en prenant la meme graine
# for(i in 1:10){
# set.seed(i)
# ES.theme.1 <- sapply(5:6,function(x)BootCi.log(.theme=paste0("res.theme",x),R=2000))
# set.seed(i)
# ES.theme.2 <- sapply(5:6,function(x)BootCi(.theme=paste0("res.theme",x),R=2000))
# print(c(paste("seed",i),ES.theme.1,ES.theme.2))
# }

#Calcul difference de moyenne et CI
difMCi<- sapply(1:6,function(x)boot.diff.ci(paste0("res.theme",x),R=20000))
difMCi <- data.frame(c(difMCi,boot.diff.ci("res.score.final",R=20000))) ;names(difMCi)<- "Difference telephone-internet"


# #MEAN IC DES THEMES ET SCORES FINAUX SANS BOOTSTRAP
# 
# #N, moyenne (IC)
# MCi.theme <- t(sapply(1:6, function(x)NMCI (paste0("res.theme",x))))
# MCi.theme <- data.frame(rbind(MCi.theme, NMCI("res.score.final")))
# 
# #Effect size
# ES.theme <- sapply(1:6,function(x)ESfun(.theme=paste0("res.theme",x)))
# ES.theme <- data.frame(c(ES.theme, ESfun("res.score.final"))) ; colnames(ES.theme)<-"ES CI"
# 
# #difference de moyenne
# difMCi<- sapply(1:6,function(x)diffmfun(paste0("res.theme",x)))
# difMCi <- data.frame(c(difMCi,diffmfun("res.score.final"))) ;names(difMCi)<- "Difference telephone-internet"


# #COMPARAISON DES THEMES PAR WILCOXON :repI vs repT. on garde lignes avec theme valide meme si score final invalide 
# r16<-sapply(1:6, function(x) wilcox.test(na.omit(d[,paste0("res.theme",x)]),na.omit(f[,paste0("res.theme",x)]))$p.value)
# rf<- wilcox.test(na.omit(d$res.score.final),na.omit(f$res.score.final))$p.value  #NB : na sont automatiquement "omitted" par wilcox.test
# #t.test(d$res.score.final,f$res.score.final)

#ATTENTION : ancien code : j'avais retirer les lignes na des scores globaux pour les wilcox des themes
# r16 <- data.frame(sapply(1:6,function(x) wilcox.test(
# d[!is.na(paste0("res.theme",x)) & !is.na(d$res.score.final), paste0("res.theme",x)],
# f[!is.na(paste0("res.theme",x)) & !is.na(f$res.score.final),paste0("res.theme",x)]
# )$p.value))


#COMPARAISON DES THEMES PAR TEST DE PERMUTATION
#cf fonction dans script "test de permutation 2016 07 12.R"
r16<- sapply(c(1:6),function(.x)perm.moyenne.th(x=.x,d,f,100000)$p_value)
rf<-perm.moyenne.fin(100000)$p_value 

pval.tot <- data.frame(c(r16,rf)) ; pval.tot<- round(pval.tot,3) ; names(pval.tot) <- "p value"

#TABLEAU SCORE, ES ET P
tab.theme <- cbind(MCi.theme,difMCi,ES.theme,pval.tot)
write.table(print(tab.theme),file="clipboard",sep="\t",dec=".",row.names=FALSE)


#COEFFICIENT DE CROHNBACH
set.seed(12345)
crontab<- data.frame(t(sapply(c(1:6,"tot"), function(i)BootCronCi(i,20000))))
#Test de permutation cronbach

#genere les 10E6 permutations cronbach et enregistre le resultat du test et les valeurs de tous les elements de l'echantillon
set.seed(1234)
for (i in 1:6){
  res.perm<-perm.cronbach.theme(x=i,1000000)
  saveRDS(res.perm,file=paste0("perm10E6.pval.cronbach.theme",i,".Rdata"))
}
res.perm<-perm.cronbach.fin(1000000)
saveRDS(res.perm,file=paste0("perm10E6.pval.cronbach.fin.Rdata"))

#assigne le tableau de cronbach a res.perm1:6 et res.permfin
for (i in 1:6){
  assign((paste0("res.perm",i)),readRDS(paste0("perm10E6.pval.cronbach.theme",i,".Rdata"))[[2]])
}
res.permfin<-readRDS("perm10E6.pval.cronbach.fin.Rdata")[[2]]

#colle tous les tableaux cronbach
for (i in c(1:6,"fin")){
  tab<-get(paste0("res.perm",i))
  if (i == 1)tabcron<-tab else tabcron<-rbind(tabcron,tab)
}

write.table(print(tabcron),file="clipboard",sep="\t",dec=".",row.names=FALSE)



#EXPORTER LES RESULTATS : nouveau tableau de données excel
d$Groupe <- rep("I",length(d$Duree>=2))
myexporti<- d[d$Duree>=2, c("Groupe","id","NIP","Duree",paste0("res.theme", 1:6),"res.score.final")]
myexportt<-f[f$Duree>=2, c("Groupe","id","NIP","Duree",paste0("res.theme", 1:6),"res.score.final")]
write.table(print(myexporti),file="clipboard",sep="\t",dec=".",row.names=FALSE)
write.table(print(myexportt),file="clipboard",sep="\t",dec=".",row.names=FALSE)


#TEST DE DISTRIBUTION NORMALE
distrib_th_I<- data.frame(sapply(1:6,function(x)ks.test(d[ ,paste0("res.theme",x)],"pnorm",
                                                        mean(d[, paste0("res.theme",x)],na.rm=TRUE),
                                                        sd(d[, paste0("res.theme",x)],na.rm=TRUE))$p.value))
distrib_fin_I<-ks.test(d$res.score.final,"pnorm",mean(d$res.score.final,na.rm=TRUE),sd(d$res.score.final,na.rm=TRUE))$p.value
#ks.test(na.omit(d$res.score.final),"pnorm",mean = mean(na.omit(d$res.score.final)), sd= sd(na.omit(d$res.score.final)))
theme_I <- c(paste0("theme",1:6),"score final")
tabdisI<-rbind(distrib_th_I,distrib_fin_I)
tabdistribI<- cbind(theme_I,tabdisI)
colnames(tabdistribI) <- c("theme_I","KS_test")
write.table(print(tabdistribI),file="clipboard",sep="\t",dec=".",row.names=FALSE)



distrib_th_T<- data.frame(sapply(1:6,function(x)ks.test(f[, paste0("res.theme",x)],"pnorm",
                                                        mean(f[, paste0("res.theme",x)],na.rm=TRUE),
                                                        sd(f[, paste0("res.theme",x)],na.rm=TRUE))$p.value))
distrib_fin_T<-ks.test(f$res.score.final,"pnorm",mean(f$res.score.final,na.rm=TRUE),sd(f$res.score.final,na.rm=TRUE))$p.value
theme_T <- c(paste0("theme",1:6),"score final")
tabdisT<-rbind(distrib_th_T,distrib_fin_T)
tabdistribT<- cbind(theme_T,tabdisT)
colnames(tabdistribT) <- c("theme_T","KS_test")
write.table(print(tabdistribT),file="clipboard",sep="\t",dec=".",row.names=FALSE)



#AUTRES TESTS POUR ARTICLE
#comparaison taux de reponse
chisq.test(matrix(c(154,237,344,43),2))
afs<-fisher.test(rbind(table(d[d$Duree>=2,"repondant"]),table(f[f$Duree>=2,"repondant"]))) #faux! inclus a tort les questionnaires incomplets dans les nonresponders dans les 

#date d'inclusion
summary(d$Date_Inclusion)
summary(f$Date_Inclusion)

#moyenne du nb de nuits
DesDureebis(2)
#mais les variables sont-elles symetriques?
summary(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]))
hist(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]), breaks = c(1:20,138))
ks.test(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),"pnorm",mean=mean(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),na.rm=T),sd= sd(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),na.rm=T))
.dd<-d %>% filter (Duree>=duree) %>% select(Duree)
#.dd<-d %>% filter (Duree>=duree & (nitems==0 | nitems>=15 )) %>% select(Duree)
table(.dd)
#non symetrique donc j'exprime en median(IQR)

#temps entre sortie et ISATIS
,d$Duree>=2
num.dur1 <-as.numeric(d$Isatis_Date2[d$Duree>=2 & d$nitems>=15]-d$Date_Sortie2[d$Duree>=2 & d$nitems>=15])
.DF <-data.frame(date=as.numeric(d$Isatis_Date2-d$Date_Sortie2), critere= d$Duree>=2 & d$nitems>=15, index = d$id)
.DF[.DF$date%in%0,"index"]
num.dur2 <- as.numeric(f$Isatis_Date2[f$Duree>=2 & f$nitems>=15]-f$Date_Sortie2[f$Duree>=2 & f$nitems>=15])
summary(num.dur1)
table(num.dur2)
summary(num.dur2)
wilcox.test(num.dur1,num.dur2)
summary(c(num.dur1,num.dur2))


