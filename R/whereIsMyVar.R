

#' Title
#'
#' @param z variable a chercher dans les dataframes
#'
#' @return  le nom du dataframe est affiché à l'écran et la variable moduel_var contient le nom du dataframe
#' @export
#'
#' @examples
whereIsMyVar <- function(z){
  pos <- 1
  ls(envir = .GlobalEnv) |>

    purrr::walk(.f=function(x){
      df <- get(x)

      if(is.data.frame(df)){

        if(z %in% names(df)){
          message(x)
          y <- "module_var"
          assign( y, x, envir =as.environment(pos) )

        }


      }

    })
}
