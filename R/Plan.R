#############################
####    Analysis Plan    ####
#############################


#----    get_analysis_plan    ----

#' @title Plan of the Analysis
#'
#' @description Plan of the Analysis
#'
#' @return a \code{drake_plan} object of class \code{tbl_df} with the plan of
#'   the analysis
#'

get_analysis_plan <- function(){
  drake::drake_plan(

    #----    Munge Data    ----
    raw_data = readRDS("Data/data_ECR.rds"),
    data_munged = munge_data(data = raw_data),


    #----    Cluster Analysis    ----
    cluster_mother_tat = cluster_tatiana(data = data_munged, parent = "mother"),
    cluster_father_tat = cluster_tatiana(data = data_munged, parent = "father"),

    data_cluster_tat = cbind(data_munged,
                             cluster_mother_tat = get_cluster_var(cluster_mother_tat),
                             cluster_father_tat = get_cluster_var(cluster_father_tat)),

    # mclust check
    mclust_mother = mclust_BIC(data = data_cluster_tat, parent = "mother"),
    mclust_father = mclust_BIC(data = data_cluster_tat, parent = "father"),


    #----    brms Models    ----
    fit_int = zip_brms(data_cluster_tat),
    stan_data = make_stan_data(data = data_cluster_tat)
  )
}

#----
