#' Methods for class \code{mtbb}
#' @name mtbb-class
#' @aliases mtbb
NULL

#' @rdname mtbb-class
#' @export
#' @param x the \code{mtbb} object to plot
#' @param X,Y numeric; the x and y positions to plot the beachball at
#' @param M numeric; the magnitude of the earthquake
#' @param M.max numeric; the maximum magnitude to normalize the sizes by
#' @param bb.unit character; the \code{\link[grid]{unit}} to convert to
#' @param add.M logical; should \code{M} be shown with the beachball?
#' @param ... additional parameters sent to \code{\link{text}} if \code{add.M=TRUE}
#' @examples
#' \dontrun{
#' b1 <- bb.sdr(45,80,70)
#' plot(-1:1, -1:1, type="b")
#' points(b1)
#' points(b1, X=0.1, add.M=TRUE)
#' }
points.mtbb <- function(x, X, Y, M=rep(1, length(x)), M.max=max(M, na.rm=TRUE), bb.unit='cm', add.M=FALSE, ...){
  # plot related coords
  if (missing(X)) X <- 0
  if (missing(Y)) Y <- 0
  X. <- grconvertX(X, from='user', to='nic')
  Y. <- grconvertY(Y, from='user', to='nic')
  Img <- x[['image']]
  Mn <- M/M.max
  H. <- grid::unit(Mn, bb.unit)
  Img <- grid::grid.raster(Img, height=H., x=X., y=Y.)
  if (add.M) text(X, Y, round(M,1), ...)
  return(invisible(Img))
}

# @inheritParams foo