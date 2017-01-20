source("src/fonctions_Isatis.R")
source("src/objects_Isatis.R")

#early responder : délai de réponse strict inferieur à 7 jours
d$early_resp <- NA
d$early_resp <- ifelse(d$Isatis_Date2-d$Date_Sortie2<7, 1, d$early_resp)
d$early_resp <- ifelse(d$Isatis_Date2-d$Date_Sortie2>=7, 0, d$early_resp)


#comparaison internet rep avant 1 sem et internet rep après 1 semaine
# d : rep internet <  7 jours
# f : rep internet >= 7 jours
f <- d[d$early_resp==0 & !is.na(d$early_resp), ]
d <- d[d$early_resp==1 & !is.na(d$early_resp), ]

# #verif legende
# source("src/objects_Isatis.R")
# #late responders, res theme 1
# mean(d[d$Isatis_Date2-d$Date_Sortie2>=7 & !is.na(d$Isatis_Date2) & !is.na(d$Date_Sortie2), "res.theme1"],na.rm = T)
# length(d[d$Isatis_Date2-d$Date_Sortie2>=7 & !is.na(d$Isatis_Date2) & !is.na(d$Date_Sortie2) & !is.na(d$res.theme1), "res.theme1"])
# #early responders, res theme 1
# mean(d[d$Isatis_Date2-d$Date_Sortie2<7 & !is.na(d$Isatis_Date2) & !is.na(d$Date_Sortie2), "res.theme1"],na.rm = T)
# length(d[d$Isatis_Date2-d$Date_Sortie2<7 & !is.na(d$Isatis_Date2) & !is.na(d$Date_Sortie2) & !is.na(d$res.theme1), "res.theme1"])

#median IQR du délai
median(d$Isatis_Date2-d$Date_Sortie2, na.rm=T)
quantile(d$Isatis_Date2-d$Date_Sortie2, na.rm=T)
median(f$Isatis_Date2-f$Date_Sortie2, na.rm=T)
quantile(f$Isatis_Date2-f$Date_Sortie2, na.rm=T)
