#' Determine when all numbers in a vector are greater than zero
#'
#' @param x numeric vector
#' @param num greater than some number, default 0
#'
#' @return logical vector - TRUE when all numbers start to be GT zero
#' @export
#'
#' @rdname gt-zero
#' @examples
#' when_all_gt(c(0, 1, 0, 1, 1, 2, 3))
#' when_all_gt(c(0, 0, 0, 0))
#' when_all_gt(c(1, 0, 1, 1, 0, 2, 3, 4))
#' where_all_gt(c(1, 0, 1, 1, 0, 2, 3, 4))
when_all_gt <- function(x, num = 0){
  purrr::accumulate(x > num,
                    .f = all,
                    .dir = "backward")
}

#' @export
#' @name gt-zero
where_all_gt <- function(x, num = 0){
  which(when_all_gt(x, num))[1]
}

#' @export
#' @name gt-zero
when_all_gte <- function(x, num = 0){
  purrr::accumulate(x >= num,
                    .f = all,
                    .dir = "backward")
}

#' @export
#' @name gt-zero
where_all_gte <- function(x, num = 0){
  which(when_all_gte(x, num))[1]
}
