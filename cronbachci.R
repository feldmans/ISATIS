#CI CRONBACH theme
cronbach.no.omit<- function(v1){
  nv1<-ncol(v1)
  pv1 <- nrow(v1)
  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var, na.rm=T))/var(apply(v1, 1, sum, na.rm=T)))
  return(alpha)
}

cronbach3.boot<- function(data,indices){
  .dat<- data[indices,]
  #.int<- .dat[.dat$group=="int",][-1]
  #.int<- cronbach3(.int)
  #.tel<- .dat[.dat$group=="tel",][-1]
  #.tel<- cronbach3(.tel)
  cron<-by(.dat[-1],.dat$group,cronbach.no.omit)
  #return(list(.int,.tel))
  return(cron)
}



bootcron<- function (x , R)  {    #x = "1":6 ou x="tot"
  
  xI<-get(paste0("vI",x))
  xT<-get(paste0("vT",x))
  if(!is.na(as.numeric(x))){
    keepI <- xI[!is.na(d[,paste0("res.theme",x)]),]
    keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
  } else {
    keepI <- na.omit(xI)
    keepT <- na.omit(xT)
  }
  .df <- data.frame(
    group=rep(c("int","tel"),c(nrow(keepI),nrow(keepT))),
    items=rbind(keepI,keepT)
  )
  .res<-boot(data=.df,statistic = cronbach3.boot ,R=R)
}

BootCronCi <- function(x,R)  {
  .bootres <- bootcron (x=x, R=R)
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0)
  colnames (.res) <- c ("CI_L", "CI_U")
  .res$est <- as.numeric (.bootres$t0)
  if(!is.na(as.numeric(x))).res$n<-c(sum(!is.na(d[,paste0("res.theme",x)])),sum(!is.na(f[,paste0("res.theme",x)])))
  else .res$n <- c(sum(!is.na(d$res.score.final)),sum(!is.na(f$res.score.final)))
  .res <- .res[, c (4,3, 1, 2)]
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, alpha_CI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}

set.seed(12345)
crontab<- data.frame(t(sapply(c(1:6,"tot"), function(i)BootCronCi(i,20000))))

