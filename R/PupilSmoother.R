#' pupilSmoother
#'
#' @param data a prepared data.table
#' @param ... params for the smoothing method
#'
#' @return processes the data.table in place
#'
#' @examples
#' pupilSmoother(data)
#'
#' @export
pupilSmoother = function(...) {
  args <- list(...)
  do.call(pupilSmoother_Lowess,args)
}


#' pupilSmoother_Hampel
#'
#' Hampel Filter
#'
#' see ?pracma::hampel
#'
#' @import pracma
#'
#' @param data a prepared data.table
#' @param Hampel_k window length
#' @param Hampel_t0 threshold
#'
#' @return processes the data.table in place
#'
#' @examples
#' pupilSmoother_Hampel(data)
#'
#' @export
pupilSmoother_Hampel = function(data, Hampel_k=15, Hampel_t0=1) {

  if (sum(names(data) == "PupilSizeL") == 1) {
    applyHampelFilter(data, "PupilSizeL", Hampel_k, Hampel_t0)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    applyHampelFilter(data, "PupilSizeR", Hampel_k, Hampel_t0)
  }

}

#' pupilSmoother_Loess
#'
#' Local Polynomial Regression Fitting
#'
#' see ??stats::loess
#'
#' @import stats
#'
#' @param data prepared data.table
#' @param Loess_degree the degree of the polynomials to be used
#' @param Loess_span degree of smoothing
#'
#' @return processes the data.table in place
#'
#' @examples
#' pupilSmoother_Loess(data)
#'
#' @export
pupilSmoother_Loess = function(data, Loess_degree=2, Loess_span=0.17)  {

  if (sum(names(data) == "PupilSizeL") == 1) {
    applyLoessFilter(data, "PupilSizeL", Loess_degree, Loess_span)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    applyLoessFilter(data, "PupilSizeR", Loess_degree, Loess_span)
  }

}

#' pupilSmoother_Lowess
#'
#' see ?stats::lowess
#'
#' @import stats
#'
#' @param data a data.table
#' @param Lowess_f the smoother span
#' @param Lowess_iter the number of robustifying iterations
#' @param Lowess_delta defaults to 1/100th of the range of x
#'
#' @return processes the data.table in place
#'
#' @examples
#' pupilSmoother_Lowess(data)
#'
#' @export
pupilSmoother_Lowess = function(data, Lowess_f=0.1, Lowess_iter=3L, Lowess_delta=0.01)  {

  if (sum(names(data) == "PupilSizeL") == 1) {
    applyLowessFilter(data, "PupilSizeL", Lowess_f, Lowess_iter, Lowess_delta)
  }
  if (sum(names(data) == "PupilSizeR") == 1) {
    applyLowessFilter(data, "PupilSizeR", Lowess_f, Lowess_iter, Lowess_delta)
  }

}



hampelFilter = function(size, Hampel_k=15, Hampel_t0=1) {
  hampel(size, Hampel_k, t0 = Hampel_t0)$y
}

applyHampelFilter = function(data, pupilcolumn, Hampel_k=15, Hampel_t0=1) {
  data[,eval(pupilcolumn):=hampelFilter(get(eval(pupilcolumn)), Hampel_k, Hampel_t0),by=list(Subject,Trial)]
}


loessFilter = function(size, time, Loess_degree=2, Loess_span=0.17) {
  loess(size ~ time,degree = Loess_degree, span=Loess_span)$fitted
}

applyLoessFilter = function(data, pupilcolumn, Loess_degree=2, Loess_span=0.17) {
  data[,eval(pupilcolumn):=loessFilter(get(eval(pupilcolumn)), TrialTime, Loess_degree, Loess_span),by=list(Subject,Trial)]
}


lowessFilter = function(size, Lowess_f=0.1, Lowess_iter=3L, Lowess_delta=0.01) {
  lowess(size,f = Lowess_f, iter = Lowess_iter, delta = Lowess_delta * diff(range(size)))$y
}

applyLowessFilter = function(data, pupilcolumn, Lowess_f=0.1, Lowess_iter=3L, Lowess_delta=0.01) {
  data[,eval(pupilcolumn):=lowessFilter(get(eval(pupilcolumn)), Lowess_f, Lowess_iter, Lowess_delta),by=list(Subject,Trial)]
}








