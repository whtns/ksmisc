#' Transmute Pairwise
#'
#' return a dataframe consisting of all combinations of arbitrary pairwise operations across a selection of columns
#'
#' @param df
#' @param fun
#' @param ...
#' @param associative
#'
#' @return
#' @export
#'
#' @examples mpg %>%
#' select(where(is.numeric)) %>%
#'  mutate(select(cur_data(), c(displ, cty, hwy)) %>%
#'           transmute_pairwise(`/`, associative = FALSE) %>%
#'           rename_with(~paste0(.x, "_ratio")) %>%
#'           identity()
#'  )
transmute_pairwise <- function(df, fun, ..., associative = TRUE){

  transmute2 <- function(df, .x, .y, fun, ...){

    dplyr::transmute(df, "{{ .x }}_{{ .y }}" := fun({{ .x }}, {{ .y }}, ...))
  }

  var_pairs <- t(combn(names(df), 2)) %>%
    tibble::as_tibble() %>%
    setNames(c(".x", ".y"))

  if(!associative){
    var_pairs <- dplyr::bind_rows(
      var_pairs,
      rename(var_pairs, .y = .x, .x = .y)
    )
  }

  dplyr::mutate(var_pairs, dplyr::across(dplyr::everything(), syms)) %>%
    purrr::pmap_dfc(transmute2, df = df, fun = fun, ...)
}

