#' Create a plot of a beachball
#' @name bb
#' @param strike,dip,rake numeric; the strike, dip, and rake of the moment tensor, in degrees
#' @param filename character; the name of the file to create
#' @param load.results logical; should the resuling file be loaded for plotting, etc?
#' @param verbose logical; should warnings and messages be given?
#' @param ... additional parameters
#' @note 
#' \code{\link{bb.sdr}} allows creation a beach ball without deviatoric components by specifying
#' the strike, dip, and rake of the earthquake.
#' 
#' \code{obspy-mopad} will rearrange the angles if they don't make sense!
#' 
#' @return An object of class \code{\link{mtbb}}
#' @seealso \code{\link{mtbb-class}}
NULL

#' @rdname bb
#' @export
#' @examples
#' \dontrun{
#' #
#' # Create "beachball" representations with  
#' #       ** Strike, Dip, Rake **  
#' # of earthquake moment tensor
#' #
#' b1 <- bb.sdr(10,1,10) # succeeds
#' b2 <- bb.sdr(1,10,10) # values are rearranged internally
#' all.equal(b1, b2)
#' 
#' # change output
#' bb.sdr(10,1,10, filename='beachball') # writes to 'beachball.svg'
#' 
#' # Visualize:
#' bb <- bb.sdr(45,80,10)
#' plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "", asp=1)
#' rasterImage(b1[['image']], 100, 400, 150, 450)
#' rasterImage(bb[['image']], 170, 370, 220, 320)
#' }
bb.sdr <- function(strike, dip, rake, filename=NULL, load.results=TRUE, verbose=TRUE, ...){
  #
  s. <- as.numeric(strike)
  d. <- as.numeric(dip)
  r. <- as.numeric(rake)
  f. <- as.character(filename)
  #
  func <- "MTP.sdr"
  # ^^^ name of the script doing the work
  n.expected <- 4
  # ^^^ this could change if `func` changes output
  #
  package.dir <- find.package('beachball')
  script <- file.path(package.dir, "MTP", func)
  cmd <- paste(script, s., d., r., f.)
  #
  # run the command
  if (verbose) message(sprintf("MT plot:  Strike  %s   Dip  %s   Rake  %s", s., d., r.))
  results <- try(paste(system(cmd, intern=TRUE)))
  success <- !inherits(results, "try-error")
  #
  # do something afterwards
  if (success){
    nr <- length(results)
    if (nr > n.expected & verbose) warning(paste(c("additionally, obspy-mopad: ", results[(n.expected+1):nr])))
    # filename
    fi <- results[n.expected]
    # get file extension
    fi.e <- tools::file_ext(fi)
    # append it if empty
    fi <- paste0(fi, ifelse(nchar(fi.e)==0, ".svg", ""))
    #
    # Load in results
    fi.e <- tolower(tools::file_ext(fi))
    res <- list(file=fi, type=fi.e, mt=c(s.,d.,r.))
    res[['image']] <- if (load.results){
      # read in
      mti. <- if (fi.e == "png"){
        # PNG image
        grDevices::as.raster(png::readPNG(fi))
      } else {
        if (verbose) warning("Cannot load file of type ", fi.e)
        NA
      }
      mti.
    } else {
      NA
    }
    if (verbose) message('       :  ', fi)
    class(res) <- 'mtbb'
    return(res)
  } else {
    stop('MT plot generation failed.')
  }
}