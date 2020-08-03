#' Simulate tests

findNextQuestionDF <- function(qs, as, survey) {
  lst <- findNextQuestionIx(qs, as, survey=survey)
  df <- lst$questionInfo
  df$ix <- lst$questionIx
  row.names(df) = NULL
  df[c('ix', 'Question')]
}

#' Simulate tests
runTest <- function (stepCount=1, answers =  c(rep(1,stepCount)), survey = 'epsi') {
  result = findNextQuestionDF(c(), c(), survey=survey)
  for (i in seq(stepCount)) {
    result[i+1,] = findNextQuestionDF(result$ix, answers[seq(i)], survey=survey)
    if (is.na(result[i+1,]$ix)) {
      break
    }
  }
  result
}

