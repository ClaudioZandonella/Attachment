#=====================#
#====    Utils    ====#
#=====================#

#----    my_round    ----

my_round <- function(x, digits = 2){
  format(round(x, digits), nsmall = digits)
}

#----    my_perc    ----

my_perc <- function(x, digits = 2){
  format(round(x*100, digits), nsmall = digits)
}

#----    my_comment    ----

my_comment <- function(x){
  cat(paste0("  # ", x, "\n"))
}
#=============


