## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("thohag/pupilParse")

## ------------------------------------------------------------------------
library(pupilParse)
data(pupilsamples)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(pupilsamples, 10))

## ------------------------------------------------------------------------
pupilsamples = pupilPrepare(pupilsamples,
                            subjectsColumn = "Subject",
                            trialsColumn = "Trial",
                            pupilSizeLeftColumn = "L.Mapped.Diameter..mm.",
                            pupilSizeRightColumn = "R.Mapped.Diameter..mm.",
                            samplingFrequency = 60)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(pupilsamples, 10))

## ---- fig.width=7, fig.height=4, fig.cap = "Raw data"--------------------
avgs = pupilsamples[,list(Left=mean(PupilSizeL,na.rm=T), Right=mean(PupilSizeR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="Pupil size")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4, fig.cap = "Cleaned data"----------------
pupilCleaner(pupilsamples)
avgs = pupilsamples[,list(Left=mean(PupilSizeL,na.rm=T), Right=mean(PupilSizeR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="Pupil size")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4, fig.cap = "Distribution of blinks over time"----
avgs = pupilsamples[,list(Left=mean(BlinkL,na.rm=T), Right=mean(BlinkR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="Blink Rate")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4,message=FALSE, fig.cap = "After blink interpolation"----
pupilBlinkInterpolator(pupilsamples)
avgs = pupilsamples[,list(Left=mean(PupilSizeL,na.rm=T), Right=mean(PupilSizeR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="Pupil size")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4,message=FALSE, fig.cap = "After applying some smoothing"----
pupilSmoother(pupilsamples, Lowess_f=0.05, Lowess_iter=3L, Lowess_delta=0.05)
avgs = pupilsamples[,list(Left=mean(PupilSizeL,na.rm=T), Right=mean(PupilSizeR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="Pupil size")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4,message=FALSE, fig.cap = "Normalized data"----
pupilNormalizer(pupilsamples, c(0, 1000))
avgs = pupilsamples[,list(Left=mean(PupilSizeL,na.rm=T), Right=mean(PupilSizeR,na.rm=T)),by=TrialTime]
ylim = range(avgs$Left,avgs$Right)
plot(Left~TrialTime, data=avgs, ylim=ylim, type="l", col="green", ylab="% Change from baseline")
lines(Right~TrialTime, data=avgs, ylim=ylim, type="l", col="red")
legend("topleft", legend = c("Left","Right"), fill=c("green","red"))

## ---- fig.width=7, fig.height=4,message=FALSE, fig.cap = "Left and right eye combined"----
avgs = pupilsamples[,list(Size=mean(((PupilSizeL+PupilSizeR)/2),na.rm=T)),by=TrialTime]
plot(Size~TrialTime, data=avgs, type="l", col="black", ylab="% Change from baseline")

