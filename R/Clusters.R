#================================#
#====    Cluster Analysis    ====#
#================================#

#-----    get_cluster_fit    ----

get_cluster_fit <- function(data, parent = c("mother", "father")){
  parent <- match.arg(parent)

  if(parent == "mother"){
    cols_num <- grepl("o_ecr[[:digit:]]+m", names(data))  # select type o_ecr..m
  } else {
    cols_num <- grepl("o_ecr[[:digit:]]+p", names(data))  # select type o_ecr..p
  }
  dist_matrix <- dist(data[,cols_num], method = "euclidean")
  fit_cluster <- hclust(dist_matrix, method="ward.D")

  return(fit_cluster)
}

#----    get_data_cluster    ----

get_data_cluster <- function(data, cluster_mother_fit, cluster_father_fit){

  # Get cluster levels
  cluster_mother_lev <- as.factor(cutree(cluster_mother_fit, k=4))
  cluster_father_lev <- as.factor(cutree(cluster_father_fit, k=4))

  data_cluster <- cbind(data,
                        cluster_mother = cluster_mother_lev,
                        cluster_father = cluster_father_lev) %>%
    mutate(cluster_mother = fct_recode(cluster_mother,
                                       secure = "1",
                                       anxious = "2",
                                       anxious_avoidant = "3",
                                       avoidant = "4"),
           cluster_mother = fct_relevel(cluster_mother,
                                        c("secure", "anxious", "avoidant", "anxious_avoidant")),
           cluster_father = fct_recode(cluster_father,
                                       secure = "1",
                                       anxious = "2",
                                       avoidant = "3",
                                       anxious_avoidant = "4")) %>%
    select(-starts_with("o_ecr"))

  return(data_cluster)
}

#-----   plot_scores    ----

plot_scores_mother <- function(data){
  fattore=rep(c("Ansia","Evitamento"),rep(dim(data)[1],2))
  y=c(scale(data$Anxm),scale(data$Avm))
  y1=c(data$Anxm,data$Avm)
  gruppo=rep(data$cluster_mother,2)
  dprof=data.frame(y,y1,fattore,gruppo)


  dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 213)","Gruppo 2: Ansiosi (n = 278)", "Gruppo 3: Evitanti (n = 241)", "Gruppo 4: Ansiosi-Evitanti (n = 122)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

plot_scores_father <- function(data){
  fattore=rep(c("Ansia","Evitamento"),rep(dim(data)[1],2))
  y=c(scale(data$Anxp),scale(data$Avp))
  y1=c(data$Anxp,data$Avp)
  gruppo=rep(data$cluster_father,2)
  dprof=data.frame(y,y1,fattore,gruppo)

  dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 207)","Gruppo 2: Ansiosi (n = 253)","Gruppo 3: Evitanti (n = 294)","Gruppo 4: Ansiosi-Evitanti (n = 100)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

#----    mclust_BIC ----

mclust_BIC <- function(data, class, parent = c("mother", "father")){
  parent <- match.arg(parent)

  if(parent == "mother"){
    cols_num <- grepl("o_ecr[[:digit:]]+m", names(data))  # select type o_ecr..m
  } else {
    cols_num <- grepl("o_ecr[[:digit:]]+p", names(data))  # select type o_ecr..p
  }

  data_no_NA <- data[, cols_num]
  data_no_NA[is.na(data_no_NA)] <- 1

  X <- data_no_NA

  res <- mclust::mclustBIC(X)

  return(res)
}
#-----


