#' A facade for mirtCAT used by pilrContentApi()
#' 
#' # Data Files
#' 
#' The data/ files were extracted from MultiCATinput_EPSI_09-21-18.RData from the demo
#' Shiny app as follows:
#' 
#'   load("MultiCATinput_EPSI_09-21-18.RData")
#;   saveRDS(mmod3, 'data/mmod3.rds')
#;   saveRDS(questions, 'data/questions')
#;   saveRDS(options.rds, 'data/options.rds')
#'
#' This changes makes it more obvious what variables are being set.

# mirtCAT() inputs that specify the survey
mirtCAT.mo <- readRDS('data/mmod3.rds')
mirtCAT.options <- readRDS('data/options.rds')
mirtCAT.df <- data.frame(Question = as.vector(readRDS('data/questions.rds')), 
                 Option = mirtCAT.options, 
                 Type = "radio", 
                 stringsAsFactors = F)

mirtCAT.preCAT = list(min_items = 15, 
                      max_items = length(mirtCAT.df$Question),
                      criteria = 'Trule',
                      method = 'MAP',
                      response_variance = F)


#' Calculate next question to ask
#' 
#' @param questions is a vector of the indicies of the questions answered so far
#' 
#' @param answers is a vector of the responses to corresponding questions
#' 
#' @return index of the next question to ask
#' 
#' @import mirtCAT
#' @export
findNextQuestionIx <- function(questions, answers, design=list(min_SEM=0.5)) {
  if (is.null(questions)) {
    return(1)
  }
  
  # need to build fresh every time because update.disgn modifies it
  CATdesign <- mirtCAT(mirtCAT.df, mirtCAT.mo, 
                             preCAT = mirtCAT.preCAT,
                             design_elements = TRUE)
  updateDesign(CATdesign, items=questions, responses=answers)
  CATdesign$design@Update.thetas(CATdesign$design, CATdesign$person, CATdesign$test)
  tryCatch({
    findNextItem(CATdesign)
  }, 
  # will get error if there are no more questions. Treat as if it terminated cleanly
  error = function(err) { NA })
}

titleForQuestion <- function(questionIx) {
  mirtCAT.df$Question[[questionIx]]
}

optionTextsForQuestion <- function(questionIx) {
  mirtCAT.options[questionIx,]
}

questionIxMatching <- function(text) {
  match(text, mirtCAT.df$Question)
}