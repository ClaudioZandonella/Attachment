#================================#
#====    Cluster Analysis    ====#
#================================#

#-----    get_cluster_fit    ----

#' Get Cluster Fit
#'
#' Given the data with ECR questionnaire items, compute the clusters. Euclidean
#' item distances are considered using the "ward.D" method.
#'
#' @param data dataframe with ECR questionnaire items for mother ("o_ecr**_m")
#'   and father ("o_ecr**_p")
#' @param parent character indicating which parent is considered to compute
#'   clusters: "mother" or "father".
#'
#' @return an "hclust" class object with the resulting clusters
#'
#' @examples
#' drake::loadd(data_munged)
#' data <- data_munged
#' kk <- get_cluster_fit(data, parent = "mother")#'

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

#' Get the Data with the Cluster Classification
#'
#' Given the data and the mother and father cluster fit objects, add the columns
#' indicating the cluster group of each subject. New columns are "mother"
#' (mother attachment cluster) "father" (father attachment cluster) and
#' "interaction" (considering together mother and father attachment cluster).
#' Moreover, colums with ECR items are removed.
#'
#'
#' @param data dataframe with the data used in the analysis
#' @param cluster_mother_fit "hclust" class object with the resulting mother
#'   clusters
#' @param cluster_father_fit "hclust" class object with the resulting father
#'   clusters
#'
#' @return dataframe with three new columns "mother" (mother attachment cluster)
#'   "father" (father attachment cluster) and "interaction" (considering
#'   together mother and father attachment cluster). Moreover, colums with ECR
#'   items are removed.
#'
#' @examples
#' drake::loadd(data_munged)
#' drake::loadd(cluster_mother_fit)
#' drake::loadd(cluster_father_fit)
#'
#' get_data_cluster(data = data_munged, cluster_mother_fit, cluster_father_fit)
#'

get_data_cluster <- function(data, cluster_mother_fit, cluster_father_fit){

  # Get cluster levels
  cluster_mother_lev <- as.factor(cutree(cluster_mother_fit, k=4))
  cluster_father_lev <- as.factor(cutree(cluster_father_fit, k=4))

  levels_interaction <- c(
    'M_Secure_F_Secure',
    'M_Secure_F_Anxious',
    'M_Secure_F_Avoidant',
    'M_Secure_F_Fearful',
    'M_Anxious_F_Secure',
    'M_Anxious_F_Anxious',
    'M_Anxious_F_Avoidant',
    'M_Anxious_F_Fearful',
    'M_Avoidant_F_Secure',
    'M_Avoidant_F_Anxious',
    'M_Avoidant_F_Avoidant',
    'M_Avoidant_F_Fearful',
    'M_Fearful_F_Secure',
    'M_Fearful_F_Anxious',
    'M_Fearful_F_Avoidant',
    'M_Fearful_F_Fearful'
    )

  data_cluster <- cbind(data,
                        mother = cluster_mother_lev,
                        father = cluster_father_lev) %>%
    mutate(mother = fct_recode(mother,
                               Secure = "1",
                               Anxious = "2",
                               Fearful = "3",
                               Avoidant = "4"),
           mother = fct_relevel(mother,
                                c("Secure", "Anxious", "Avoidant", "Fearful")),
           father = fct_recode(father,
                               Secure = "1",
                               Anxious = "2",
                               Avoidant = "3",
                               Fearful = "4"),
           interaction = factor(paste0("M_", mother, "_F_",father),
                                levels = levels_interaction)) %>%
    select(-starts_with("o_ecr")) # remove colums with ECR items

  return(data_cluster)
}

#-----   plot_scores    ----

#' Plot Cluster Scores for the Mother/Father
#'
#' @param data dataframe with cluster classification
#'
#' @return NULL
#'
#' @examples
#' drake::loadd(data_cluster)
#' plot_scores_mother(data_cluster)
#' plot_scores_father(data_cluster)
#'

plot_scores_mother <- function(data){
  fattore=rep(c("Anxiety","Avoidance"),rep(dim(data)[1],2))
  y <- c(scale(data$Anxm),scale(data$Avm))
  y1 <- c(data$Anxm,data$Avm)
  gruppo <- rep(data$mother,2)
  dprof <- data.frame(y,y1,fattore,gruppo)


  dprof$nomi.gruppi <- factor(dprof$gruppo,labels=c("Group 1: Secure (n = 231)","Group 2: Anxious (n = 286)", "Group 3: Avoidant (n = 230)", "Group 4: Fearful (n = 100)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Standardized Scores",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

plot_scores_father <- function(data){
  fattore=rep(c("Anxiety","Avoidance"),rep(dim(data)[1],2))
  y=c(scale(data$Anxp),scale(data$Avp))
  y1=c(data$Anxp,data$Avp)
  gruppo=rep(data$father,2)
  dprof=data.frame(y,y1,fattore,gruppo)

  dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Group 1: Secure (n = 206)","Group 2: Anxious (n = 230)","Group 3: Avoidant (n = 311)","Group 4: Fearful (n = 100)"))
  sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,2.1),x.leg=1.2, y.leg=-1.2,ylab="Standardized Scores",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))
}

#----    mclust_BIC ----

#' Get Cluster Analysis BIC with mclust
#'
#' @param data dataframe with ECR questionnaire items for mother ("o_ecr**_m")
#'   and father ("o_ecr**_p")
#' @param parent character indicating which parent is considered to compute
#'   clusters: "mother" or "father".
#'
#' @return an object of class "mclustBIC"
#'
#' @examples
#' drake::loadd(data_munged)
#' mclust_BIC(data = data_munged, parent = "mother")
#' mclust_BIC(data = data_munged, parent = "father")
#'

mclust_BIC <- function(data, parent = c("mother", "father")){
  parent <- match.arg(parent)

  if(parent == "mother"){
    cols_num <- grepl("o_ecr[[:digit:]]+m", names(data))  # select type o_ecr..m
  } else {
    cols_num <- grepl("o_ecr[[:digit:]]+p", names(data))  # select type o_ecr..p
  }

  data_no_NA <- data[, cols_num]

  # Substitute NA with item mean
  row_with_NA <- apply(data_no_NA, 1, anyNA)
  NA_per_row <- apply(is.na(data_no_NA), 1, sum)[row_with_NA] # Number of NA for subject with missing values
  item_mean <- apply(data_no_NA[row_with_NA, ], 1, mean, na.rm = TRUE) # item_mean for subject with missing values

  data_no_NA[is.na(data_no_NA)] <- rep(item_mean, times = NA_per_row)


  res <- mclust::mclustBIC(data_no_NA)

  return(res)
}

#-----
