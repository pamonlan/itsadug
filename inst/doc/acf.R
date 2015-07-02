## ------------------------------------------------------------------------
suppressMessages(library(itsadug))
info('version')

## ---- results='hide'-----------------------------------------------------
data(simdat)

# For illustration purposes, we build a GAMM model
# with a nonlinear interaction, two groups, and
# random wiggly smooths for Subjects:
m1 <- bam(Y ~ Group + te(Time, Trial, by=Group)
  + s(Time, Subject, bs='fs', m=1),
  data=simdat)

## ---- fig.width=4, fig.height=4------------------------------------------
acf(resid(m1))

## ---- fig.width=4, fig.height=4------------------------------------------
acf_resid(m1)

## ---- eval=FALSE---------------------------------------------------------
#  # Option A: include named list
#  acf_resid(m1, split_pred=list(simdat$Subject,simdat$Trial))
#  
#  # Option B: include model predictors
#  # This method only works for predictors that are included in the model.
#  acf_resid(m1, split_pred=c("Subject","Trial"))

## ---- fig.width=4, fig.height=4, echo=FALSE------------------------------
acf_resid(m1, split_pred=c("Subject","Trial"))

## ---- fig.width=4, fig.height=4, results='hold'--------------------------
# Minimum ACF per lag:
acf_resid(m1, split_pred=c("Subject","Trial"), fun=min)
# Maximum ACF per lag:
acf_resid(m1, split_pred=c("Subject","Trial"), fun=max)

## ---- fig.width=4, fig.height=4------------------------------------------

# Median ACF per lag:
acf_resid(m1, split_pred="Subject", fun=median, lwd=3,
          main="Distribution of ACF")
# Calculate 25% and 75% quantiles:
acf1 <- acf_resid(m1, split_pred="Subject", 
    fun=function(x){quantile(x, .25)}, plot=FALSE)
acf2 <- acf_resid(m1, split_pred="Subject", 
    fun=function(x){quantile(x, .75)}, plot=FALSE)
# Plot these as error bars in different colors:
len <- length(acf1)-1
fill_area(x=0:len, y=acf2, from=acf1, col=alpha(1))
addInterval(pos=0:len, acf1, acf2, horiz=FALSE, col=alpha(1))
# add legend:
legend('topright',
    fill=alpha(1),
    border=alpha(1),
    legend='25-75%',
    bty='n')

## ---- fig.width=8, fig.height=6------------------------------------------
acf_resid(m1, split_pred=c("Subject","Trial"), n=6)

## ---- fig.width=8, fig.height=6------------------------------------------
out <- acf_resid(m1, split_pred=c("Subject","Trial"), n=6, plot=FALSE)
# print the head of the elements in the first quantile:
head(out[[1]][['elements']])
# print the quantile:
out[[1]][['quantile']]

## ---- fig.width=8, fig.height=6------------------------------------------
acf_resid(m1, split_pred=c("Subject","Trial"), n=6, random=TRUE)

## ---- fig.width=8, fig.height=6------------------------------------------
simdat$Event <- with(simdat, interaction(Subject, Trial))
acf_resid(m1, split_pred=list(Event=simdat$Event), n=6, 
    cond=list(Event=c('c05.-10', 'c11.-10', 'a05.-9', 'a09.-9', 'a13.-9', 'c02.-9')))

## ------------------------------------------------------------------------
# default output is the acf values:
(out <- acf_resid(m1, split_pred=c("Subject","Trial"), plot=FALSE))

# Alternatively, more information could be retrieved:
out <- acf_resid(m1, split_pred=c("Subject","Trial"), plot=FALSE, return_all=TRUE)
# out is a list of info:
names(out)

# 1. acf gives the acf values:
out[['acf']]

# 2. acftable provides the individual acf's in wide table format:
head(out[['acftable']], 3)
dim(out[['acftable']])

# 3. dataframe prvides a data frame with the acf, n, and ci information
# in long table format:
head(out[['dataframe']])

# 4. n provides the number of data points underlying each ACF:
head(out[['n']])
     
# 5. series and FUN provide info on input and function:
out[['series']]
out[['FUN']]

## ---- eval=FALSE---------------------------------------------------------
#  # Plot individual participants with the package lattice:
#  library(lattice)
#  out <- acf_resid(m1, split_pred=c("Subject"), plot=FALSE, return_all=TRUE)$dataframe
#  civec = out[out$lag==0,]$ci
#  xyplot(acf ~ lag | event, type = "h", data = out, col.line = "black",
#              panel = function(...) {
#                  panel.abline(h = civec[panel.number()], col.line = "grey")
#                  panel.abline(h = -civec[panel.number()], col.line = "grey")
#                  panel.abline(h = 0, col.line = "black")
#                  panel.xyplot(...)
#              },
#              strip = strip.custom(bg = "grey90"),
#              par.strip.text = list(cex = 0.8),
#              xlab="lag", ylab="autocorrelation")

## ---- fig.width=4, fig.height=4, results='hold'--------------------------
# genetare AR start column:
simdat <- start_event(simdat, column="Time", event="Event")
head(simdat)

# run GAMM with AR1 model:
m1 <- bam(Y ~ Group + te(Time, Trial, by=Group)
  + s(Time, Subject, bs='fs', m=1),
  data=simdat, rho=.65, AR.start=simdat$start.event)

# plot normal acf, without correction for rho:
acf(resid(m1))
# plot normal acf with acf_plot:
acf_resid(m1)
# plot normal acf with acf_plot:
acf_plot(resid(m1), split_by=list(simdat$Subject))
# plot corrected acf plot with acf_plot:
acf_plot(resid_gam(m1, incl_na=TRUE), split_by=list(simdat$Subject))

## ---- fig.width=4, fig.height=4, results='hold'--------------------------
acf_plot(resid_gam(m1))
acf_plot(resid_gam(m1, incl_na=TRUE), split_by=list(simdat$Subject))

## ---- fig.width=8, fig.height=6, results='hold'--------------------------
acf_n_plots(resid_gam(m1, incl_na=TRUE), split_by=list(simdat$Subject), n=6, random=TRUE)

