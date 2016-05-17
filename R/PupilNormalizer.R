#' pupilNormalizer
#'
#' normalizes pupilsizes according to a specified baseline
#' ((pupilsize - baselinesize)/baselinesize)*100
#'
#' @param data a data.table
#' @param baseline a TrialTime range to use for baseline
#'
#' @return processes the data.table in place
#'
#' @examples
#' pupilNormalizer(data, c(100,1000))
#'
#' @export
pupilNormalizer = function(data, baseline) {
  tag = NULL
  range = NULL
  if (length(baseline) == 1) {
    tag = baseline
  } else if (length(baseline) == 2) {
    range = baseline
  } else {
    stop("Baseline wrongly specified, use either TrialTime ranges or TrialPosition tags")
  }

  if (sum(names(data) == "PupilSizeL") == 1) {
    computeBaselines(data, "PupilSizeL", tag, range)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    computeBaselines(data, "PupilSizeR", tag, range)
  }

}

computeBaselines = function(datas, pupilcolumn, tag, range) {


  baselinecolumn = paste("Baseline", substr(pupilcolumn,nchar(pupilcolumn),nchar(pupilcolumn)), sep="")

  if (!is.null(tag)) {
    datas[TrialPosition == tag,eval(baselinecolumn):=mean(get(eval(pupilcolumn)), na.rm=T),by = Trial]
  } else {
    datas[TrialTime >= range[1] & TrialTime <= range[2], eval(baselinecolumn):=mean(get(eval(pupilcolumn)), na.rm=T),by = Trial]
  }

  datas[,eval(baselinecolumn):=mean(get(eval(baselinecolumn)),na.rm=T),by = Trial]
  datas[,eval(pupilcolumn):=((get(eval(pupilcolumn)) - get(eval(baselinecolumn)))/(get(eval(baselinecolumn))))*100,]

  #Clean up
  datas[,eval(baselinecolumn):=NULL]

}
