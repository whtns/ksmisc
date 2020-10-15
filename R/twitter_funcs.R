#' retrieve my twitter favorites from a given user
#'
#' @param likes_user the user whose favorite we'll query
#' @param twitter_user the user whose tweets were favorited
#' @param ... additional arguments passed to `rtweet::get_favorites`
#'
#' @return
#' @export
#'
#' @examples
fav_by_author <- function(twitter_user, likes_user = "stchlk", ...){
  my_favs <- rtweet::get_favorites(likes_user, ...) %>%
    dplyr::filter(screen_name == twitter_user) %>%
    dplyr::select(text, status_url)
}

