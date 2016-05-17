#'  pupilPrepare
#'
#'  prepare a data.frame for processing with the functions in this package.
#'
#'@import data.table
#'
#' @param data a data.frame or data.table with pupil samples in rows
#' @param subjectsColumn name of column with subject identifiers
#' @param trialsColumn name of column with trial identifiers
#' @param pupilSizeLeftColumn name of column with left pupil size
#' @param pupilSizeRightColumn name of column with right pupil size
#' @param samplingFrequency sampling frequency of the eye-tracker (samples per second)
#'
#' @return a data table with specific columns
#'
#' @examples
#' data(pupilsamples)
#' pupilPrepare(pupilsamples, subjectsColumn = "Subject", trialsColumn = "Trial", pupilSizeLeftColumn = "L.Mapped.Diameter..mm.", pupilSizeRightColumn = "R.Mapped.Diameter..mm.", samplingFrequency = 60)
#'
#'
#' @export
pupilPrepare = function(data,
                        subjectsColumn = "Subject",
                        trialsColumn = "Trial",
                        pupilSizeLeftColumn = "L.Mapped.Diameter..mm.",
                        pupilSizeRightColumn = "R.Mapped.Diameter..mm.",
                        samplingFrequency = 60,
                        normalizeTrialDurations = TRUE
                        )
{
  if (is.null(data)) {stop("No data")}
  if (is.null(trialsColumn)) {stop("No trialsColumn")}
  if (is.null(pupilSizeLeftColumn)) {stop("No pupilSizeLeftColumn")}
  if (is.null(pupilSizeRightColumn)) {stop("No pupilSizeRightColumn")}

  data = as.data.table(data)

  setnames(data, subjectsColumn, "Subject")
  setnames(data, trialsColumn, "Trial")
  setnames(data, pupilSizeLeftColumn, "PupilSizeL")
  setnames(data, pupilSizeRightColumn, "PupilSizeR")

  computeTrialTimes(data,samplingFrequency)

  setattr(data, "SamplingFrequency",samplingFrequency)


  #Normalize trial lengths
  if (normalizeTrialDurations) {
    lengthts = data[,list(Max=max(TrialTime)),by=Trial]
    duration = median(lengthts$Max)
    data = data[TrialTime <= duration,]
  }

  return(data)
}
