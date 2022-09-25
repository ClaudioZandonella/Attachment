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

#----    make_my_book    ----

#' Make My Bookdown
#'
#' Render Bookdown html and pdf. It allows setting the correct working directory
#' to build the documents.
#'
#' @param subdir string indicating the path to the bookdown main file. It is
#'   used as working directory.
#'
#' @return builded bookdown is returned in "docs/" folder
#'

make_my_book <- function(subdir = "Documents/Bookdown/") {

  origwd <- setwd(file.path(subdir))
  on.exit(setwd(origwd))
  bookdown::render_book(input='_bookdown.yml', config_file='_bookdown.yml',
                        output_format = "bookdown::gitbook",
                        params = list(format = "html"))
  bookdown::render_book(input='_bookdown.yml', config_file='_bookdown.yml',
                        output_format = "bookdown::pdf_book",
                        params = list(format = "latex"))
}

#=============


