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
    # Get cluster fit
    cluster_mother_fit = get_cluster_fit(data = data_munged, parent = "mother"),
    cluster_father_fit = get_cluster_fit(data = data_munged, parent = "father"),

    # Get cluster data
    data_cluster = get_data_cluster(data = data_munged,
                                    cluster_mother_fit = cluster_mother_fit,
                                    cluster_father_fit = cluster_father_fit),

    # mclust check
    mclust_mother = mclust_BIC(data = data_munged,
                               class = data_cluster$cluster_mother,
                               parent = "mother"),
    mclust_father = mclust_BIC(data = data_munged,
                               class = data_cluster$cluster_father,
                               parent = "father"),

    #----    ZIP analysis    ----

    #----    brms Models    ----
    fit_int_additive = zip_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = "gender + cluster_mother + cluster_father"),
    stan_data = make_stan_data(data = data_cluster)
  )
}

#----
