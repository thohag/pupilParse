#' pupilCleaner
#'
#' Attempts to clean the pupil data by removing and interpolating over extreme sample values
#'
#' May help for datasets with extreme deviations
#'
#' @import zoo pracma
#'
#' @param data data.table from pupilPrepare
#' @param MinimumPupilSize remove values less than this value
#' @param MaximumPupilSize remove values greater than this value
#' @param SDLimit outlier limits in standard deviations
#'
#' @return NULL (data.table processed in place)
#'
#' @examples
#' pupilCleaner(pupilsamples)
#'
#' @export
pupilCleaner = function(data, MinimumPupilSize=2, MaximumPupilSize=8, SDLimit=3) {
  if (sum(names(data) == "PupilSizeL") == 1) {
    cleanPupils(data, "PupilSizeL", MinimumPupilSize, MaximumPupilSize, SDLimit)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    cleanPupils(data, "PupilSizeR", MinimumPupilSize, MaximumPupilSize, SDLimit)
  }
}

cleanPupils = function(datas, pupilcolumn, MinimumPupilSize, MaximumPupilSize, SDLimit){

  #Set all values outsize physiological limits to NA
#   sum(is.na(datas$PupilSizeL))
#   settings$PupilCleaner$Limits$MinimumPupilSize = 3
  datas[get(eval(pupilcolumn)) < MinimumPupilSize,eval(pupilcolumn):=NA]
  datas[get(eval(pupilcolumn)) > MaximumPupilSize,eval(pupilcolumn):=NA]
  #sum(is.na(datas$PupilSizeCombined_Normalized))


  datas[,Mean:=mean(get(eval(pupilcolumn)),na.rm=T),by=list(Subject,Trial)]

  datas[,UpperLimit:=Mean+(SDLimit*sd(get(eval(pupilcolumn)),na.rm=T)),by=list(Subject,Trial)]
  datas[,LowerLimit:=Mean-(SDLimit*sd(get(eval(pupilcolumn)),na.rm=T)),by=list(Subject,Trial)]

  datas[get(eval(pupilcolumn)) > UpperLimit, eval(pupilcolumn):=NA]
  datas[get(eval(pupilcolumn)) < LowerLimit, eval(pupilcolumn):=NA]

  datas[,Mean:=mean(get(eval(pupilcolumn)),na.rm=T),by=list(Subject,Trial)]


  #Fill inn NAs around NAs
  datas[,NABack:=shift(get(eval(pupilcolumn)), 10, type="lead", fill=0),by=list(Subject,Trial)]
  datas[,NAFront:=shift(get(eval(pupilcolumn)), 10, type="lag", fill=0),by=list(Subject,Trial)]
  datas[is.na(NABack) | is.na(NAFront),eval(pupilcolumn):=NA,]
  datas[,NABack:=NULL]
  datas[,NAFront:=NULL]



  #Perform interpolation
  datas[TrialTime == 0 & is.na(get(eval(pupilcolumn))), eval(pupilcolumn):=Mean]
  datas[!is.na(Mean),eval(pupilcolumn):=na.approx(get(eval(pupilcolumn)),na.rm=F),by=list(Subject,Trial)]



  #Fill inn values with the mean for points that are still NA (mostly end of trial)
  datas[is.na(get(eval(pupilcolumn))),eval(pupilcolumn):=Mean,by=list(Subject,Trial)]

  #If still NA, the entire trial is useless, set to grand mean
  grand = datas[,list(GrandMean=mean(get(eval(pupilcolumn)),na.rm=T))][[1]]
  datas[,BAD:=FALSE]
  datas[is.na(Mean),BAD:=TRUE]
  datas[is.na(get(eval(pupilcolumn))), eval(pupilcolumn):=grand]


  #Clean up variables
  datas[,Mean:=NULL]
  datas[,UpperLimit:=NULL]
  datas[,LowerLimit:=NULL]

}
# pupilcolumn = "PupilSizeCombined_Normalized"
# datas = alldatas
# pupilCleaner(alldatas, "PupilSizeCombined_Normalized")
