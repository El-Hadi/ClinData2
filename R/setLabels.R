
#' Title
#'
#' @param fichier fichier
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
setLabels <- function(fichier){


  pos <- 1

  fich <- readr::read_file(fichier, locale = readr::locale(encoding = "ISO-8859-1"))

  test <- stringr::str_split(fich, "Label", simplify = T)

  x <- stringr::str_split(test[2], "\n", simplify = T)

  y = t(x)

  #
  # y %>%
  #   as.data.frame() %>%
  #   head(-2) %>%
  #   tail(-1) %>%
  #   dplyr::filter(grepl("=", V1)) %>%
  #   tidyr::separate(col = "V1", sep = "=", into = c("vari", "label"), extra = "merge") %>%
  #   filter(str_detect(label, "=")) %>%
  #   print()


  libel <- y %>%
    as.data.frame() %>%
    utils::head(-2) %>%
    utils::tail(-1) %>%
    dplyr::filter(grepl("=", .data$V1)) %>%
    tidyr::separate(col = "V1", sep = "=", into = c("vari", "label"), extra = "merge") %>%
    dplyr::mutate(vari = stringr::str_trim(.data$vari),
                  label = stringr::str_replace_all(.data$label, "\"", "'")) %>%
    dplyr::mutate(label = stringr::str_remove_all(.data$label, "\"")) %>%
    dplyr::select(.data$label) %>%
    dplyr::mutate(label=stringr::str_remove_all(.data$label, "'$")) %>%
    dplyr::mutate(label=stringr::str_trim(.data$label, side="both")) %>%
    dplyr::mutate(label=stringr::str_remove_all(.data$label, "^'")) %>%
    purrr::as_vector() %>%
    unname()

  donne <- stringr::str_remove(fichier, "\\.sas") %>%
    stringr::str_remove(pattern = get("path"))

  #message(donne)

  temp <- get(donne)

  length(temp)
  length(libel)

  sjlabelled::set_label(temp) <- libel

  nom <- stringr::str_split(as.name(donne), "_ANSI",
                            simplify = T)[1]
  nom <- stringr::str_to_lower(nom)
  #print(paste("dataframe ", nom, "created", sep = " "))
  assign(nom, temp, envir = as.environment(pos))

  rm(list=donne, envir = as.environment(pos))


}



