##################################################################################
## A facde for computeQuestion that implements the PiL<R Calculated Content API
## for requests submitted by openCPU.
##
##################################################################################
library('mirtCAT')

pilrContentApi <- function(participantCode, resultsSoFar, sourceCard,
                             # following parameters are test hooks.
                             computeFn = computeQuestion,
                             mirtCatDataFrame = df) {
  questions <- c()
  answers <- c()
  for (section in resultsSoFar) {
    filter <- grepl('mc:.*', section$data$question_code)
    qs = substring(section$data$question_code[filter], 4)
    as = section$data$response_value[filter]
    questions <- c(questions, as.numeric(qs))
    answers <- c(answers, as.numeric(as))
  }
  # print(list(qs=questions, as=answers))
  sourceCard$section <- sourceCard$section + 1
  nextQuestionIx <- computeFn(designElements(), questions, answers)

  option.names = names(mirtCatDataFrame)[grepl('Option.*', names(mirtCatDataFrame))]
  options <- lapply(mirtCatDataFrame[nextQuestionIx, option.names], function(optStr) {
    parts <- strsplit(optStr, '-')[[1]]
    list(value = parts[[1]], name = parts[[2]])
  })
  result <- list(section = 1,
                 data = list(title = mirtCatDataFrame$Question[[nextQuestionIx]],
                             text = '',
                             code = paste0('mc:', nextQuestionIx),
                             options = options))
  result
}

# dump inputs passed by openCPU for use in sample-parameters.R for testing
dumper <- function(participantCode, resultsSoFar, sourceCard) {
  fn <- 'dumped-stuff.R'
  dump(c('participantCode', 'resultsSoFar', 'sourceCard'), file=fn)
  readChar(fn, file.info(fn)$size)
}

library('mirtCAT')
data(CATDesign)

designElements.cached <- NULL

designElements <- function() {
  if (is.null(designElements.cached)){
    designElements.cached <<-
      mirtCAT(df, criteria = 'KL', start_item = 'Trule',
              design_elements = TRUE,
              design = list(min_SEM = rep(0.4, 3),
                            max_items = ncol(data_epsi1a),
                            delta_thetas = rep(0.03, 3)))
  }
  designElements.cached 
}  