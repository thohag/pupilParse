#' pupilBlinkInterpolator
#'
#' see also ?zoo::na.approx
#'
#' @import zoo
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
pupilBlinkInterpolator = function(data, buffer=5) {

  if (sum(names(data) == "PupilSizeL") == 1) {
    applyBlinkInterpolator(data, "PupilSizeL", buffer = buffer)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    applyBlinkInterpolator(data, "PupilSizeR", buffer = buffer)
  }

}




applyBlinkInterpolator = function(data, pupilcolumn, buffer=5) {

  blinkcolumn = paste("Blink", substr(pupilcolumn,nchar(pupilcolumn),nchar(pupilcolumn)), sep="")

  #Store the grandaverage trial sizes
  data[,MeanBackup:=mean(get(eval(pupilcolumn)),na.rm=T),by=Trial]

  #Set all ranges with blinks to NA
  data[get(eval(blinkcolumn)) == 1, eval(pupilcolumn):=NA,]

  #Fill inn NAs around NAs
  data[,NABack:=shift(get(eval(pupilcolumn)), buffer, type="lead", fill=0),by=Trial]
  data[,NAFront:=shift(get(eval(pupilcolumn)), buffer, type="lag", fill=0),by=Trial]
  data[is.na(NABack) | is.na(NAFront),eval(pupilcolumn):=NA,]
  data[,NABack:=NULL]
  data[,NAFront:=NULL]

  #If any trial is only NA we need to mark them as BAD
  if (sum(names(data) == "BAD") == 0) {data[,BAD:=FALSE]}
  data[,MeanSize:=mean(get(eval(pupilcolumn)),na.rm=T),by=list(Subject,Trial)]
  data[is.na(MeanSize),BAD:=TRUE]


  library(zoo)
  data[,eval(pupilcolumn):=na.approx(get(eval(pupilcolumn)))]

  data[is.na(get(eval(pupilcolumn))), eval(pupilcolumn):=MeanBackup]

  #Clean up
  data[,MeanBackup:=NULL]
}

