#' Fade out the areas in a surface without data.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @description Add a transparency Rug to a contour plot or image.
#' 
#' @param x Observations on x-axis.
#' @param y Observations on y-axis.
#' @param n.grid Resolution of Rug. Defaults to 30, 
#' which means that the x- and y-axis are divided in 30 bins.
#' @param gradual Logical: whether or not to use the number of 
#' observations in an area, i.e., more transparent equals more 
#' observations. Default is FALSE, which means that the function only 
#' distinguishes between observations in a certain region or not, 
#' regardless how many observations.
#' @param max.alpha Maximum of transparency, number between 0 (completely 
#' transparent) and 1 (non-transparent). Defaults to .75.
#' @param col Color value. Defaults to "white".
#' @return Plots a shaded image over the contour plot or image.
#' @author Jacolien van Rij
#' @section Warning:
#' On Linux \code{\link{x11}} devices may not support transparency. 
#' In that case, a solution might be to write the plots immediately to a file 
#' using functions such as \code{\link{pdf}}, or \code{\link{png}}.
#' @seealso 
#' \code{\link[graphics]{rug}}, \code{\link[graphics]{contour}}, 
#' \code{\link[graphics]{image}}
#' @examples
#' data(simdat)
#' 
#' # Introduce extreme values:
#' set.seed(123)
#' newdat <- simdat[sample(which(simdat$Time < 1500),
#'     size=round(.5*length(which(simdat$Time < 1500)))),]
#' newdat <- rbind(newdat, 
#'     simdat[sample(which(simdat$Time > 1500),
#'     size=5),])
#' # Some simple GAM with tensor:
#' m1 <- bam(Y ~ te(Time, Trial), data=newdat)
#' # plot summed effects:
#' fvisgam(m1, view=c("Time", "Trial"), zlim=c(-15,15),
#'     add.color.legend=FALSE)
#' # add rug:
#' fadeRug(newdat$Time, newdat$Trial)
#' # compare with default rug:
#' rug(newdat$Time)
#' rug(newdat$Trial, side=2)
#' # add color legend:
#' gradientLegend(c(-15,15), pos=.875)
#' # add data points (for checking the grid):
#' points(newdat$Time, newdat$Trial)
#' 
#' # change x- and y-grid:
#' fvisgam(m1, view=c("Time", "Trial"), zlim=c(-15,15))
#' points(newdat$Time, newdat$Trial)
#' fadeRug(newdat$Time, newdat$Trial, n.grid=c(100,10), col='gray')
#' @family Functions for plotting
fadeRug <- function(x, y, n.grid = 30, gradual=FALSE, 
    max.alpha = 0.75, col='white') {
    n.grid.x <- n.grid.y <- n.grid[1]
    if(length(n.grid)==2){
        n.grid.y <- n.grid[2]
    }
    xlim <- c(par()$usr[1], par()$usr[2])
    ylim <- c(par()$usr[3], par()$usr[4])
    x.step <- diff(seq(xlim[1], xlim[2], length=n.grid.x))[1]
    y.step <- diff(seq(ylim[1], ylim[2], length=n.grid.y))[1]
    im <- matrix(table(factor(round((x - xlim[1])/x.step)+1, levels = 1:n.grid.x), 
        factor(round((y - ylim[1])/y.step)+1, levels = 1:n.grid.y)))
    if(gradual==FALSE){
        im[im > 0] <- 1 
    }
    fadecols <- alphaPalette(col, f.seq = seq(max.alpha, 0, length = max(im) + 1))
    im <- matrix(fadecols[im + 1], byrow = TRUE, ncol = n.grid.x)
    im <- im[n.grid.y:1,]
    rasterImage(as.raster(im), xleft = xlim[1], xright = xlim[2], ybottom = ylim[1], ytop = ylim[2], interpolate = FALSE)
}





#' Add rug to plot, based on model.
#'
#' @export
#' @import grDevices
#' @import graphics
#' @import stats
#' @description Add rug based on model data.
#' 
#' @param model gam or bam object.
#' @param view Text string containing the name of the smooth
#' to be displayed. Note that 
#' variables coerced to factors in the model formula won't work as view 
#' variables.
#' @param cond A named list of the values to use for the other predictor terms 
#' (not in view). Used for choosing between smooths that share the same view 
#' predictors.
#' @param data.rows Vector of numbers (indices of rows in data) or vector of 
#' logical vales (same length as rows in data) for selecting specific data 
#' points.
#' @param rm.ranef Logical: whether or not to remove random effects. 
#' Default is TRUE.
#' @param print.summary Logical: whether or not to print information messages.
#' Default set to the print info messages option 
#' (see \code{\link{infoMessages}}).
#' @param ... Optional graphical parameters (see \code{\link[graphics]{rug}}).
#' @author Jacolien van Rij
#' @examples
#' plot(cars$speed, cars$dist, pch=16, col=alpha(1))
#' lm1 <- lm(dist ~ speed, dat=cars)
#' abline(lm1, col='red', lwd=2)
#' rug_model(lm1, view="speed")
#' rug_model(lm1, view="dist", side=2)
#' 
#' \dontrun{
#' library(itsadug)
#' data(simdat)
#' m1 <- bam(Y ~ Group + te(Time, Trial, by=Group), data=simdat)
#' # plot:
#' fvisgam(m1, view=c("Time", "Trial"), cond=list(Group="Adults"))
#' rug_model(m1, view="Time", cond=list(Group="Adults"))
#' rug_model(m1, view="Trial", cond=list(Group="Adults"), side=2)
#' }
#' @family Functions for plotting
rug_model <- function (model, view, cond=NULL, data.rows=NULL, 
    rm.ranef=NULL, 
    print.summary=getOption('itsadug_print'),...) {
    dat <- NULL
    view <- view[1]
    if("lm" %in% class(model)){
        dat <- model$model
    }else if( "lmerMod" %in% class(model)){
        dat <- model@frame
    }
    if(!is.null(data.rows)){
        dat <- dat[data.rows,]
    }else if(!is.null(cond)){
        if(!is.null(rm.ranef)){
            if(rm.ranef==TRUE){
                for(i in 1:length(model$smooth)){
                    if("random" %in% names(model$smooth[[i]])){
                        terms <- model$smooth[[i]]$term
                        for(j in terms){
                            if ((j %in% names(cond)) & !(j %in% view)){
                                cond[[j]] <- NULL
                            }
                        }
                    }
                }
            }else if(inherits(rm.ranef, c('numeric', 'integer'))){
                for(i in rm.ranef){
                    if("random" %in% names(model$smooth[[i]])){
                        terms <- model$smooth[[i]]$term
                        for(j in terms){
                            if ( (j %in% names(cond)) & !(j %in% view)){
                                cond[[j]] <- NULL
                            }
                        }
                    }
                }
            }
        }
        for(i in names(cond)){
            if((!i %in% view) & (!inherits(dat[,i], c("numeric", 'integer')))){
                dat <- dat[dat[,i] %in% cond[[i]],]
            }
        }
    }
    if((nrow(dat)==0)){
        rug(model$model[, view], ...)
        if( print.summary ==TRUE){
            cat("Note: Selection of grouping predictors does not seem to appear in data. Rug of all data is being added.\n")
        }
    }else{
        rug(dat[,view], ...)
    }
}





