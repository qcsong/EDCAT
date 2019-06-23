##################################################################################
## A facde for computeQuestion that implements the PiL<R Calculated Content API
## for requests submitted by openCPU.
##
##################################################################################

library('mirtCAT')
data(CATDesign)

compute.fn <- function(design.elements, questions, answers) {
  if (is.null(questions)) {
    return(1)
  }
  updateDesign(design.elements, items=questions, responses=answers)
  tryCatch({
    findNextItem(design.elements)
  },
  error = function(error_condition) {
    1
  })
}


#' @export
#' @import mirtCAT
pilrContentApi <- function(participantCode, resultsSoFar, sourceCard,
                             # following parameters are test hooks.
                             computeFn = compute.fn,
                             mirtCatDataFrame = df) {
  design.elements <- mirtCAT(df, mod, criteria = 'KL', start_item = 'Trule',
                             design_elements = TRUE,
                             design = list(min_SEM = rep(0.4, 3),
                                           max_items = ncol(data_epsi1a),
                                           delta_thetas = rep(0.03, 3)))
  questions <- c()
  answers <- c()
  for (section in resultsSoFar) {
    filter <- grepl('mc:.*', section$data$question_code)
    qs = substring(section$data$question_code[filter], 4)
    as = section$data$response_value[filter]
    questions <- c(questions, as.numeric(qs))
    answers <- c(answers, as.numeric(as))
  }
  sourceCard$section <- sourceCard$section + 1
  nextQuestionIx <- computeFn(design.elements, questions, answers)

  option.names = names(mirtCatDataFrame)[grepl('Option.*', names(mirtCatDataFrame))]
  options <- lapply(mirtCatDataFrame[nextQuestionIx, option.names], function(optStr) {
    parts <- strsplit(optStr, '-')[[1]]
    list(value = parts[[1]], name = parts[[2]])
  })
  calculatedCard <- list(section = sourceCard$section,
                         data = list(title = mirtCatDataFrame$Question[[nextQuestionIx]],
                                     text = '',
                                     code = paste0('mc:', nextQuestionIx),
                                     options = options))
  list(result=list(calculatedCard))
}

# dump inputs passed by openCPU for use in sample-parameters.R for testing
dumper <- function(participantCode, resultsSoFar, sourceCard) {
  fn <- 'dumped-stuff.R'
  dump(c('participantCode', 'resultsSoFar', 'sourceCard'), file=fn)
  readChar(fn, file.info(fn)$size)
}
