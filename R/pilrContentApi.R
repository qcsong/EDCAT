##################################################################################
## A facade for computeQuestion that implements the PiL<R Calculated Content API
## for requests submitted by openCPU.
##
##################################################################################

#' Wrap PiLR Content API around a mirtCAT survey
#'
#' See "232 - KU Cat Design - Remote Calculation Card" (https://docs.google.com/document/d/1fC8kag54Ttm9Yy0vm3oayHKyk5jLnvHw9e5MOqrkZJo)
#'
#' @param participantCode not used here but part of API
#'
#' @param resultsSoFar the 'result' variable from EMA survey director.
#'   It defines the questions asked so far and the answers given
#'
#' @param sourceCard the calulated content card object that triggered this request. Only the properties listed below are
#'   used.
#'
#' @param sourceCard$section is used so set the section of the expansion cards
#'
#' @param sourceCard$args$maxQuestions is option for testing.  If set, pilrContentApi() will return the Done result
#' 
#' @param findNextFn is a hook for testing. Its default value invokes a facade for mirtCAT::findNext.
#' 
#' @mirtCatDataFrame is the 'df' parameter passed to mirtCAT::mirtCAT. It defines the questions and response options. 
#'
#' @return a list of cards objects with which EMA replaces the sourceCard in the survey
#'
#' @export
pilrContentApi <- function(participantCode, resultsSoFar, sourceCard,
                             # following parameters are test hooks.
                             findNextFn = findNextQuestionIx) {
  param <- buildParamFn(sourceCard$data$args)

  tryCatch({
    history <- buildHistory(resultsSoFar)

    # For testing
    if(nrow(history) >= param('maxQuestions', 1e6)) {
      return(buildDoneResult(sourceCard$section))
    }
    
    nextQuestionIx <- findNextFn(history$questions, history$answers)

    if (!is.numeric(nextQuestionIx)) {
      return(buildDoneResult(section))
    }

    text <- if (as.logical(param('debug', FALSE))) {
      paste0('(question #', nextQuestionIx, ')') 
    } else {
      ''
    }

    calculatedCard <- buildSelectCard(nextQuestionIx, sourceCard$section) 
    nextCalcCard <- sourceCard
    nextCalcCard$section <- nextCalcCard$section + 1

    list(result=list(calculatedCard, nextCalcCard))
  },
  error = function(error_condition) {
    list(error=error_condition)
  })
}

buildParamFn <- function(argString) {
  noParams <- function(paramName, defaultValue) defaultValue
  tryCatch({
    paramMap = jsonlite::parse_json(argString)
    if (is.list(paramMap)) {
       function(paramName, defaultValue) {
        if (exists(paramName, paramMap)) paramMap[paramName] else  defaultValue
      }
    } 
    else {
      noParams
    }
  }, error = function(error) noParams)
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

buildSelectCard <- function(questionIx, section) {
  list(card_type = 'q_select',
       section = section,
       order = 1,
       data = list(title = titleForQuestion(questionIx),
                   text = text,
                   code = paste0('mc:', questionIx),
                   required = TRUE,
                   options = optionListForQuestion(questionIx)))
}

optionListForQuestion <- function(questionIx) {
  # Convert mirtCAT options [dataframe columns named 'Option-0', etc] to card options
  text <- optionTextsForQuestion(questionIx)
  value <- c(1:length(text))
  data.frame(text=text, value=value, order=value)
}

buildDoneResult <- function(section) {
  list(result=list(buildDoneCard(section)))
}

buildDoneCard <- function(section, title= 'Finished', text='Thank you! Please press submit to send results.') {
  list(card_type = 'instruction',
       section = section,
       order = 1,
       data = list(title = title,
                   text = text,
                   code = paste0('mc:done')))
}

# dump inputs passed by openCPU for use in sample-parameters.R for testing
dumper <- function(participantCode, resultsSoFar, sourceCard) {
  fn <- 'dumped-stuff.R'
  dump(c('participantCode', 'resultsSoFar', 'sourceCard'), file=fn)
  readChar(fn, file.info(fn)$size)
}
