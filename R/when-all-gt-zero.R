#' Determine when all numbers in a vector are greater than zero
#'
#' @param x numeric vector
#'
#' @return logical vector - TRUE when all numbers start to be GT zero
#' @export
#'
#' @rdname gt-zero
#' @examples
#' when_all_gt_zero(c(0, 1, 0, 1, 1, 2, 3))
#' when_all_gt_zero(c(0, 0, 0, 0))
#' when_all_gt_zero(c(1, 0, 1, 1, 0, 2, 3, 4))
when_all_gt_zero <- function(x){

  purrr::accumulate(x > 0,
                    .f = all,
                    .dir = "backward")
}

#' @export
#' @name gt-zero
where_all_gt_zero <- function(x){
  which(when_all_gt_zero(x))[1]
}
