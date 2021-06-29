#================================#
#====    Cluster Analysis    ====#
#================================#

#-----    cluster_tatiana    ----

cluster_tatiana <- function(data, parent = c("mother", "father")){
  parent <- match.arg(parent)

  if(parent == "mother"){
    cols_num <- 60:71
  } else {
    cols_num <- 99:110
  }
  dist_matrix <- dist(data[,cols_num], method = "euclidean")
  fit_cluster <- hclust(dist_matrix, method="ward.D")

  return(fit_cluster)
}

#----    get_cluster_var    ----

get_cluster_var <- function(fit_cluster){
  as.factor(cutree(fit_cluster, k=4))
}

#-----   plot_scores    ----

plot_scores_mother <- function(data){
  fattore=rep(c("Ansia","Evitamento"),rep(dim(data)[1],2))
  y=c(scale(data$Anxm),scale(data$Avm))
  y1=c(data$Anxm,data$Avm)
  gruppo=rep(data$cluster_mother_tat,2)
  dprof=data.frame(y,y1,fattore,gruppo)


  dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 231)","Gruppo 2: Ansiosi (n = 286)","Gruppo 3: Ansiosi-Evitanti (n = 100)","Gruppo 4: Evitanti (n = 230)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

plot_scores_father <- function(data){
  fattore=rep(c("Ansia","Evitamento"),rep(dim(data)[1],2))
  y=c(scale(data$Anxp),scale(data$Avp))
  y1=c(data$Anxp,data$Avp)
  gruppo=rep(data$cluster_father_tat,2)
  dprof=data.frame(y,y1,fattore,gruppo)

  dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 206)","Gruppo 2: Ansiosi (n = 230)","Gruppo 3: Evitanti (n = 311 )","Gruppo 4: Ansiosi-Evitanti (n = 100)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

#----    mclust_BIC ----

mclust_BIC <- function(data, parent = c("mother", "father")){
  parent <- match.arg(parent)

  if(parent == "mother"){
    cols_num <- 60:71
    class <- data$cluster_mother_tat
  } else {
    cols_num <- 99:110
    class <- data$cluster_father_tat
  }

  data_no_NA <- data[, cols_num]
  data_no_NA[is.na(data_no_NA)] <- 1

  X <- data_no_NA

  res <- mclust::mclustBIC(X)

  return(res)
}
#-----


