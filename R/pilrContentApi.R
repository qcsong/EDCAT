##################################################################################
## A facade for computeQuestion that implements the PiL<R Calculated Content API
## for requests submitted by openCPU.
##
##################################################################################
library('mirtCAT')

#' Set global constant df, the mirtCAT data-frame that defines the survey.
data(CATDesign)

#' Wrap PiLR Content API around a mirtCAT survey
#' 
#' See "232 - KU Cat Design - Remote Calculation Card" (https://docs.google.com/document/d/1fC8kag54Ttm9Yy0vm3oayHKyk5jLnvHw9e5MOqrkZJo)
#'
#' @param participantCode not used here but part of API
#' 
#' @param resultsSoFar the 'result' variable from EMA survey director.  
#'   It defines the questions asked so far and the answers given
#'   
#' @param sourceCard the calulated content card object that triggered this request. 
#' 
#' @return a list of cards objects with which EMA replaces the sourceCard in the survey
#' 
#' @export
#' @import mirtCAT
pilrContentApi <- function(participantCode, resultsSoFar, sourceCard,
                             # following parameters are test hooks.
                             computeFn = findNextQuestionIx,
                             mirtCatDataFrame = df) {
  tryCatch({
    # need to build fresh every time because update.disgn modifies it
    design.elements <- mirtCAT(df, mod, criteria = 'KL', start_item = 'Trule',
                               design_elements = TRUE,
                               design = list(min_SEM = rep(0.4, 3),
                                             max_items = ncol(data_epsi1a),
                                             delta_thetas = rep(0.03, 3)))
    history <- buildHistory(resultsSoFar)

    nextQuestionIx <- computeFn(design.elements, history$questions, history$answers)
    
    if (is.na(nextQuestionIx)) {
      return(list(result=list(buildInstructionCard('Done', 'Tap submit to send results', sourceCard$section))))
    }
    
    options <- optionsForQuestion(nextQuestionIx, mirtCatDataFrame)
    
    calculatedCard <- list(card_type = 'q_select',
                           section = sourceCard$section,
                           order = 1,
                           data = list(title = mirtCatDataFrame$Question[[nextQuestionIx]],
                                       text = '',
                                       code = paste0('mc:', nextQuestionIx),
                                       options = options))
    
    nextCalcCard <- sourceCard
    nextCalcCard$section <- nextCalcCard$section + 1
    
    list(result=list(calculatedCard, nextCalcCard))
  },
  error = function(error_condition) {
    list(error=error_condition)
  })
}

#' Extracts question and answer indices from resultsSoFar
buildHistory = function(resultsSoFar) {
  questions <- c()
  answers <- c()
  for (section in resultsSoFar) {
    filter <- grepl('mc:.*', section$data$question_code)
    qs = substring(section$data$question_code[filter], 4) # question ix is code with 'mc:' prefix removed
    as = section$data$response_value[filter]
    questions <- c(questions, as.numeric(qs))
    answers <- c(answers, as.numeric(as))
  }
  data.frame(questions, answers)
}

#' Construct the EMA single-select card option list for the given question
optionsForQuestion <- function(questionIx, mirtCatDataFrame) {
  # Convert mirtCAT options [dataframe columns named 'Option-0', etc] to card options
  option.names = names(mirtCatDataFrame)[grepl('Option.*', names(mirtCatDataFrame))]
  options <- lapply(mirtCatDataFrame[questionIx, option.names], function(optStr) {
    parts <- strsplit(optStr, '-')[[1]]
    list(value = parts[[1]], 
         text = parts[[2]], 
         order=1+as.numeric(parts[[1]]))
  })
  names(options) <- NULL
  options
}

buildInstructionCard <- function(title, text, section) {
  list(card_type = 'instruction',
       section = section,
       order = 1,
       data = list(title = title,
                   text = text,
                   code = paste0('mc:done')))  
}

#' pilrContentApi clone that terminates after 2 cards
#' @export
testPilrContentApi <- function(participantCode, resultsSoFar, sourceCard) {
  if (length(resultsSoFar) > 2) {
    return(c()) 
  }
  pilrContentApi(participantCode, resultsSoFar, sourceCard)
}


# dump inputs passed by openCPU for use in sample-parameters.R for testing
dumper <- function(participantCode, resultsSoFar, sourceCard) {
  fn <- 'dumped-stuff.R'
  dump(c('participantCode', 'resultsSoFar', 'sourceCard'), file=fn)
  readChar(fn, file.info(fn)$size)
}
