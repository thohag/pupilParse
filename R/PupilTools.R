computeTrialTimes = function(datas, SamplingRate) {
  datas[!duplicated(Trial),"TrialTime":= 0L,by=Trial]
  datas[duplicated(Trial),"TrialTime":= 1:.N,by=Trial]
  datas[,"TrialTime":= as.double(TrialTime),]
  datas[,TrialTime:=TrialTime*(1000/SamplingRate)]
}

plotAll = function(datas, title, xlimit) {

  par(mfrow=c(1,1))


  datas1 = aggregate( PupilSize~TrialTime, datas, mean )

  datas1 = datas1[1:xlimit,]

  minimum = min(min(datas1$PupilSize))
  maximum = max(max(datas1$PupilSize))

  plot(datas1,type="n",xlab="Samples",ylab="Pupil Size",ylim=c(minimum,maximum),main=title)


  line_red(datas1$TrialTime,datas1$PupilSize,datas1$PupilSize)

}


cleanup = function(alldatas) {
  alldatas = alldatas[!alldatas$size %in% boxplot.stats(alldatas$size)$out,]
}


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x$size, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.0 * IQR(x$size, na.rm = na.rm) #Original: 1.5
  y <- x
  y[x$size < (qnt[1] - H),c("size")] <- NA
  y[x$size > (qnt[2] + H),c("size")] <- NA
  y
}

tagBaselineOutliers = function(datas, type, range, left, right) {
  if (left) {
    x = aggregate(BaselineL~Trial,datas,max)
    names(x)[2] = "size"
    x = tag_outliers(x, "BaselineOutlierL",type, range)
    x = x[,c("Trial","BaselineOutlierL")]
    datas = merge(datas,x,by="Trial")
  }
  if (right) {
    x = aggregate(BaselineR~Trial,datas,max)
    names(x)[2] = "size"
    x = tag_outliers(x, "BaselineOutlierR",type, range)
    x = x[,c("Trial","BaselineOutlierR")]
    datas = merge(datas,x,by="Trial")
  }
  datas
}

tag_outliers <- function(x, variable, type, range) {
  x[,variable] = FALSE

  qnt <- quantile(x$size, probs=c(.25, .75), na.rm = TRUE, type = type)
  H <- range * IQR(x$size, na.rm = TRUE) #Original: 1.5
  y <- x
  y[x$size < (qnt[1] - H),variable] <- TRUE
  y[x$size > (qnt[2] + H),variable] <- TRUE
  y
}

exportPupilMeansOnSubjects = function(vars, alldatas) {



  conds = data.frame();
  for (sub in 1:length(subjects)) {

    subject = subjects[sub]

    datas1 = aggregate( size~TrialTime, alldatas[alldatas$Cue == "farge" & alldatas$Switch == "ikke" & alldatas$Congruency == "I" & alldatas$SubjectNr == subject & alldatas$ErrorType == 0,], mean )
    datas2 = aggregate( size~TrialTime, alldatas[alldatas$Cue == "farge" & alldatas$Switch == "ikke" & alldatas$Congruency == "C" & alldatas$SubjectNr == subject & alldatas$ErrorType == 0,], mean )

    cond1 = mean(datas1[datas1$TrialTime > 4000 & datas1$TrialTime < 6000,]$size)
    cond2 = mean(datas2[datas2$TrialTime > 4000 & datas2$TrialTime < 6000,]$size)

    conds[sub, "I"] = cond1*100;
    conds[sub, "C"] = cond2*100;

  }
  conds = signif(conds, digits=3);
  row.names(conds)<-NULL
  write.table(conds,"Ord_IvsC.txt",sep="\t", dec=",",row.names=F)
  t.test(conds$I,conds$C,paired=T)
}


vars = list(
  "Conditions" = list(
    "Incongruent" = list (list("Variable" = "Cue", "Value" = "farge"),list("Variable" = "Cue", "Value" = "farge"))
  ),
  "TrialTime" = list("Lower" = 4000, "Upper" = 6000),
  "FileName" = "testing.txt"
)

#utils::View(alldatas)

#winDialog("yesno", "Is it OK to delete file blah")

# SciViewsPackages <- c("SciViews", "svMisc", "svSocket", "svGUI", "svIDE",
#                       "svKomodo", "svDialogs", "svSweave", "svTools", "svUnit", "svWidgets", "tcltk2")
# install.packages(SciViewsPackages)

# form <- list(
#   "Name:TXT" = "John Smith",
#   "Age:NUM" = 25,
#   "Sex:CB" = c("male", "female"),
#   "Married:CHK"=FALSE
# )
# #dlgForm(form, "My form")$res
# dlgForm(form, title = "Fill the form", gui = .GUI)
# test = dlgInput(message = "Enter a value", default = "", gui = .GUI)
# test$res
#
# res <- dlgList(month.name, multiple = TRUE)$res
# if (!length(res)) {
#   cat("You cancelled the choice\n")
# } else {
#   cat("You selected:\n")
#   print(res)
# }

#install.packages("gWidgetsRGtk2", dep = TRUE)

#install.packages("devtools")
#devtools::install_github("shiny-incubator", "rstudio")
