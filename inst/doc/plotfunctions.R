## ----, results='hide'----------------------------------------------------
library(itsadug)

data(simdat)

# For illustration purposes, we build a GAMM model
# with a nonlinear interaction, two groups, and
# random wiggly smooths for Subjects:
m1 <- bam(Y ~ Group + te(Time, Trial, by=Group)
  + s(Time, Subject, bs='fs', m=1),
  data=simdat)

## ----, eval=TRUE, results='asis'-----------------------------------------
gamtabs(m1, caption="Summaty of m1", comment=FALSE, type='html')

## ----,plotgam, echo=TRUE, include=TRUE, fig.width=8, fig.height=4, fig.cap="<b>Figure 1.</b> Partial effects surfaces <code>te(Time,Trial):GroupChildren</code> and <code>te(Time,Trial):GroupAdults</code> plotted with <code>plot.gam</code>."----
par(mfrow=c(1,2))
plot(m1, select=1, rug=FALSE,
     main='Group=Children', cex.axis=1.5, cex.lab=1.5)
plot(m1, select=2, rug=FALSE,
     main='Group=Adults', cex.axis=1.5, cex.lab=1.5)

## ----, fig.width=8, fig.height=4, echo=TRUE, include=TRUE,fig.cap="<b>Figure 2.</b> Partial effects surfaces <code>te(Time,Trial):GroupChildren</code> and <code>te(Time,Trial):GroupAdults</code> plotted with <code>pvisgam</code>."----
par(mfrow=c(1,2))
# Note: specify zlim when comparing two plots
pvisgam(m1, view=c("Time", "Trial"), select=1, 
     main='Group=Children', labcex=.8,
     zlim=c(-15,15), print.summary=FALSE)
pvisgam(m1, view=c("Time", "Trial"), select=2, 
     main='Group=Adults', labcex=.8,
     zlim=c(-15,15), print.summary=FALSE)

## ----, visgam, fig.width=8, fig.height=4, echo=TRUE, include=TRUE,fig.cap="<b>Figure 3.</b> Summed effects surfaces for <i>Time</i> and <i>Trial</i> plotted with <code>vis.gam</code>."----
par(mfrow=c(1,2))
# Note: specify zlim when comparing two plots
vis.gam(m1, view=c("Time", "Trial"), 
        cond=list(Group='Children', Subject='a01'),
        plot.type='contour', color='topo', main='Group=Children',
        zlim=c(-8,10))
vis.gam(m1, view=c("Time", "Trial"), 
        cond=list(Group='Adults', Subject='a01'),
        plot.type='contour', color='topo', main='Group=Adults',
        zlim=c(-8,10))

## ----, fvisgam,fig.width=8, fig.height=4, echo=FALSE, include=TRUE,fig.cap="<b>Figure 4.</b> Summed effects surfaces for <code>Time</code> and <code>Trial</code> plotted with <code>fvisgam</code>. Random effects are zeroed out."----
par(mfrow=c(1,2))
# Note: specify zlim when comparing two plots
fvisgam(m1, view=c("Time", "Trial"), 
        cond=list(Group='Children'),
        plot.type='contour', color='topo', main='Group=Children',
        zlim=c(-12,15), print.summary=FALSE)
fvisgam(m1, view=c("Time", "Trial"), 
        cond=list(Group='Adults'),
        plot.type='contour', color='topo', main='Group=Adults',
        zlim=c(-12,15))

## ----, plotSmooth1-code, echo=TRUE, include=FALSE, eval=FALSE------------
#  par(mfrow=c(1,2), cex=1.1)
#  # As there is no one-dimensional smooth in model m1,
#  # the interaction of m1 is decomposed in smooths and
#  # partial interactions:
#  m2 <- bam(Y ~ Group + s(Time, by=Group)
#    + s(Trial, by=Group) + ti(Time, Trial, by=Group)
#    + s(Time, Subject, bs='fs', m=1), data=simdat)
#  
#  # To plot the smooth over time for Children:
#  plot(m2, select=1, shade=TRUE, rug=FALSE, scale=0)
#  title(main="s(Time):GroupChildren")
#  abline(h=0) # add horizontal line
#  
#  # Now we add the intercept term:
#  plot(m2, select=1, shade=TRUE, rug=FALSE, scale=0,
#    shift=coef(m2)[1])
#  title(main="s(Time):GroupChildren")
#  mtext('incl. intercept', side=3, line=.5, cex=1.1)
#  abline(h=0) # add horizontal line

## ----,plotSmooth1, echo=FALSE, fig.width=8, fig.height=4, include=TRUE, fig.cap="<b>Figure 5.</b> Using <code>plot.gam</code> for plotting a one dimensional smooth."----
par(mfrow=c(1,2), cex=1.1)
# As there is no one-dimensional smooth in model m1,
# the interaction of m1 is decomposed in smooths and 
# partial interactions:
m2 <- bam(Y ~ Group + s(Time, by=Group)
  + s(Trial, by=Group) + ti(Time, Trial, by=Group)
  + s(Time, Subject, bs='fs', m=1), data=simdat)

# To plot the smooth over time for Children:
plot(m2, select=1, shade=TRUE, rug=FALSE, scale=0)
title(main="s(Time):GroupChildren")
abline(h=0) # add horizontal line

# Now we add the intercept term:
plot(m2, select=1, shade=TRUE, rug=FALSE, scale=0,
  shift=coef(m2)[1])
title(main="s(Time):GroupChildren")
mtext('incl. intercept', side=3, line=.5, cex=1.1)
abline(h=0) # add horizontal line

# indicate effect of intercept:
abline(h=coef(m2)[1], lty=2, col='red')
arrows(x0=500,x1=500,y0=0,y1=coef(m2)[1], 
  code=2, length=.1, col='red', lwd=2)
text(500,coef(m2)[1], labels='+intercept', font=3, col='red', pos=3)

## ----,plotSmooth2, echo=TRUE, fig.width=8, fig.height=4, fig.show='hold', include=TRUE, fig.cap="<b>Figure 6.</b> <i>Left:</i> Using plot_smooth for plotting a one dimensional smooth from model <code>m2</code>. <i>Right:</i> Using plot_smooth for extracting a one dimensional smooth from an interaction surface in model <code>m1</code>."----
par(mfrow=c(1,2), cex=1.1)

# First plot the smooth for Adult participants in gray...
plot_smooth(m2, view="Time", cond=list(Group="Adults"), rug=FALSE, 
  ylim=c(-10,15), print.summary=FALSE,
  main='m2: Time, Group=Adults')
# ... then add the smooth from which the random effects are excluded
plot_smooth(m2, view="Time", cond=list(Group="Adults"), 
  rug=FALSE, add=TRUE, col='red', rm.ranef=TRUE, 
  ylim=c(-10,15), print.summary=FALSE, xpd=TRUE)
# Add legend:
legend('bottomleft', 
  legend=c("incl. random","excl. random"),
  col=c("black", "red"), lwd=2,
  bty='n')

# Secondly, a smooth based on the tensor in m1:
plot_smooth(m1, view="Time", cond=list(Group="Adults"), 
  rug=FALSE, ylim=c(-10,15), print.summary=FALSE,
  main='m1: Time, Group=Adults')
plot_smooth(m1, view="Time", cond=list(Group="Adults"), 
  rug=FALSE, add=TRUE, col='red', rm.ranef=TRUE, 
  ylim=c(-10,15), print.summary=FALSE, xpd=TRUE)

## ----, plotDots, echo=TRUE, fig.width=4, fig.height=4,include=TRUE, fig.cap="<b>Figure 7.</b> Estimates for each group when all other predictors are set to zero."----
par(cex=1.1)

# First plot the Group estimates by setting the smooth terms on zero:
plot_parametric(m1, pred=list(Group=c("Adults", "Children")),  
  cond=list(Time=0, Trial=0), rm.ranef=TRUE, 
  print.summary=FALSE)

## ----,plotdiff, echo=TRUE, fig.width=8, fig.height=4, include=TRUE, fig.cap="<b>Figure 8.</b> Difference surface for <code>Group</code> plotted with <code>plot_diff2</code>.",fig.show='hold'----
par(mfrow=c(1,2))
plot_diff2(m1, view=c("Time","Trial"), 
        comp=list(Group=c("Adults", "Children")),
        zlim=c(-5,7.5), 
        main='Difference Adults-Children',
        print.summary=FALSE)
plot_diff(m1, view="Time",         
        comp=list(Group=c("Adults", "Children")),
        main='Time difference Adults-Children')

## ----, predict1, echo=TRUE, include=TRUE---------------------------------
# Extract prediction from model
# Note that the random effects are canceled out by setting rm.ranef to TRUE
xval <- seq(0,2000, length=100)
yval <- seq(-10,10, length=50)
g1 <- get_predictions(m1, cond=list(Time=xval, Trial=yval, Group='Adults'), 
                      rm.ranef=TRUE, print.summary=FALSE)
# Create plot matrix
g1 <- g1[order(g1$Time, g1$Trial),]
zval <- matrix(g1$fit, byrow=TRUE, nrow=100,ncol=50)

## ----, image1, echo=TRUE, include=TRUE,fig.width=4, fig.height=4,fig.cap="<b>Figure 9.</b> Using <code>image()</code> and <code>contour()</code> to plot the interaction surface."----
image(xval, yval, zval, col=topo.colors(100),
      main='Group=Adults', xlab='Time', ylab='Trial')
contour(xval, yval, zval, labcex=.8, add=TRUE, col='red')

## ----, predict2, echo=TRUE, include=TRUE---------------------------------
# Extract prediction from model
# Note that the random effects are canceled out by setting rm.ranef to TRUE
xval <- seq(0,2000, length=100)
g2 <- get_predictions(m1, cond=list(Time=xval, Trial=5, Group='Adults'), 
                      rm.ranef=TRUE, print.summary=FALSE)
g3 <- get_predictions(m1, cond=list(Time=xval, Trial=-5, Group='Adults'), 
                      rm.ranef=TRUE, print.summary=FALSE)
head(g2)

## ----, image2, echo=TRUE, fig.width=8, fig.height=4, include=TRUE,fig.cap="<b>Figure 10.</b> <i>Left:</i> Interaction between Time and Trial for adult participants. <i>Right:</i> The effect of Time for Trial 5 (A) and Trial -5 (B) with 95% CI."----
par(mfrow=c(1,2))

image(xval, yval, zval, col=topo.colors(100),
      main='Group=Adults', xlab='Time', ylab='Trial')
contour(xval, yval, zval, labcex=.8, add=TRUE, col='red')
# Add arrows for comparing two conditions
arrows(x0=0, x1=2200, y0=5, y1=5, code=2, 
  length=.1, angle=30, lwd=2, xpd=TRUE)
arrows(x0=0, x1=2200, y0=-5, y1=-5, code=2, 
  length=.1, angle=30, lwd=2, col='magenta', xpd=TRUE)
text(2100, c(5,-5), labels=c('A', 'B'), 
  col=c('black', 'magenta'), pos=3, cex=1.1, xpd=TRUE)

# Setup the plot region for the two smooths:
emptyPlot(range(g2$Time), 
  range(c(g2$fit+g2$CI, g2$fit-g2$CI,g3$fit+g3$CI, g3$fit-g3$CI)),
  main='Trials 5 and -5', xlab='Time', ylab='Est. value of Y',
  h0=0)

# add two smooths:
# Note: f is set to 1, because the SE are already multiplied by 1.96 to get 95%CI
plot_error(g2$Time, g2$fit, g2$CI, 
  shade=TRUE, f=1, xpd=TRUE)
plot_error(g3$Time, g3$fit, g3$CI, 
  col='magenta', shade=TRUE, f=1, xpd=TRUE)

# add text as legend:
text(1500,c(15,10), labels=c("A", "B"), 
  col=c('black', 'magenta'), font=2, adj=0)


## ----,predictRandom, echo=TRUE, include=TRUE-----------------------------
# Extract the mean of the random smooths from model m1
g4 <- get_random(m1, fun='mean')
# Extract the median of the random smooths from model m1
g5 <- get_random(m1, fun='median')

## ----, random, echo=TRUE, fig.width=8, fig.height=4, include=TRUE, fig.cap="<b>Figure 11.</b> <i>Left:</i> Random effects smooths. <i>Right:</i> The mean (black) and median (blue) of the random smooths."----
par(mfrow=c(1,2))

plot(m1, select=3, ylim=c(-20,20))
title(main="Random smooths")
abline(h=0)

# Plot the mean random smooth:
itsadug::emptyPlot(range(g4[[1]]$Time), c(-10,10),
    main='Mean and median of random smooths', 
    xlab='Time', ylab='Est. value of Y',
    h0=0)
lines(g4[[1]]$Time, g4[[1]]$x, lwd=2, xpd=TRUE)
lines(g5[[1]]$Time, g5[[1]]$x, lwd=2, col='blue', xpd=TRUE)

## ----, resid, echo=TRUE, fig.width=8, fig.height=8, include=TRUE, warning=FALSE, fig.cap="<b>Figure 12.</b> <i>Top row:</i> Test the distribution of the model residuals for normality. The residuals seem to follow a t-distribution rather than normal distribution. <i>Bottom row:</i> The autocorrelation in the residuals. The left panel shows the acf of all the residuals, treating them as a single time series. The right panel is based on the function <code>acf_plot</code>, which averages over the time series."----
# Note: as no AR1 model was included, resid() is used instead of resid_gam()

check_resid(m1, split_by=list(Subject=simdat$Subject, Trial=simdat$Trial))

## ----, acfplots, echo=TRUE, fig.width=12, fig.height=8, include=TRUE, fig.cap="<i>Figure 13.</i> 6 ACF plots, averaged over time series."----
acf_n_plots(resid(m1), 
  split_by=list(Subject=simdat$Subject, Trial=simdat$Trial), 
  n=6,cex.lab=1.5, cex.axis=1.5, cex.main=2)

## ----, acfresid, echo=TRUE, fig.width=4, fig.height=4, include=TRUE, fig.cap="<i>Figure 14.</i> ACF plot of model residuals."----
acf_resid(m1, split_pred=c("Subject", "Trial"))

## ----,packageVersions, echo=TRUE, include=TRUE---------------------------
packageVersion("mgcv")
packageVersion("itsadug")

## ------------------------------------------------------------------------
citation("itsadug")

