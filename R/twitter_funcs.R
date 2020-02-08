#' retrieve my twitter favorites from a given user
#'
#' @param likes_user
#' @param twitter_user
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fav_by_author <- function(likes_user = "stchlk", twitter_user, ...){
  my_favs <- rtweet::get_favorites(likes_user, ...) %>%
    dplyr::filter(screen_name == twitter_user) %>%
    dplyr::select(text, status_url)
}

