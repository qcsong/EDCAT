#' A facade for mirtCAT used by pilrContentApi()
#' 
#' findNextQuestionIx() is the function used by pilrContentApi().
#' 
#' buildMirtCatStateObject() is an internal function that is useful for tuning the survey. It returns the CATDesign parmeter required by 
#' mirtCAT::findNextItem().   
#' 
#' TUNING EXAMPLE
#'
#' Simulate asking questions 1, 40 and 3 with responses always option 2.  Examine resulting state's theta history
#'  
#'   > library(mirtCAT)
#'   > source('~/Projects/EDCAT/R/findNextQuestionIx.R')
#'   > x <- buildMirtCatStateObject(questions=c(1, 40, 3), answers=c(2, 2, 2))
#'   > x$person$thetas_history
#'   F1            F2        F3
#'   [1,] 0.0000000  0.000000e+00 0.0000000
#'   [2,] 0.1611487 -3.615848e-07 0.9306835
#'   
#' Look at ?mirtCAT::findNext for examples of how to modify buildMirtCatStateObject()
#'
#' DATA FILES
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

# mirtCAT() input parameters that specify the survey
.design.mo <- readRDS('data/mmod3.rds')
.design.options <- readRDS('data/options.rds')
.design.design = list(min_SEM=0.5)
.design.start_item = 'Trule'
.design.df <- data.frame(Question = as.vector(readRDS('data/questions.rds')), 
                 Option = .design.options, 
                 Type = "radio", 
                 stringsAsFactors = F)

.design.preCAT = list(min_items = 15, 
                      max_items = length(.design.df$Question),
                      criteria = 'Trule',
                      method = 'MAP',
                      response_variance = T)


#' Calculate next question to ask
#' 
#' 
#' @param questions is a vector of the indicies of the questions answered so far
#' 
#' @param answers is a vector of the responses to corresponding questions
#' 
#' @return index of the next question to ask
#' 
#' @import mirtCAT
#' @export
findNextQuestionIx <- function(questions, answers) {
  tryCatch({
    mcState <- buildMirtCatStateObject(questions, answers)
    if (mcState$design@stop_now)
      NA
    else
      findNextItem(mcState)
  }, 
  error = function(err) { 
    # will get error if there are no more questions. Treat as if it terminated cleanly
    NA 
  })
}

#' Construct the object that enapsulate the state of mirtCAT survey from responses so far
#' 
#' @param questions is a vector of the indicies of the questions that have been answered
#' 
#' @paaram answers is a vector of the indicies of the responses
#' 
#' @return the state object x such that mirtCAT::findNextItem(x) returns the next question to be asked.
#' 
buildMirtCatStateObject <- function(questions, answers) {
  CATdesign <- mirtCAT(.design.df, .design.mo,
                       preCAT = .design.preCAT,
                       design = .design.design,
                       start_item = .design.start_item,
                       design_elements = TRUE)
  if (is.null(questions)) {
    return(CATdesign)
  }
  for (i in c(1:length(questions))) {
    CATdesign <- updateDesign(CATdesign, items=questions[i], responses=answers[i])
    
    # from Server.R#166...
    
    CATdesign$design@Update.thetas(design=CATdesign$design, person=CATdesign$person, test=CATdesign$test)
    CATdesign$person$Update.info_mats(design=CATdesign$design, test=CATdesign$test)
    CATdesign$design <- mirtCAT:::Update.stop_now(CATdesign$design, person=CATdesign$person)
    CATdesign$design <- mirtCAT:::Next.stage(CATdesign$design, person=CATdesign$person, test=CATdesign$test, item=i)
  }
  CATdesign
}

# old.buildMirtCatStateObject <- function(questions, answers) {
#   CATdesign <- mirtCAT(.design.df, .design.mo, 
#                        preCAT = .design.preCAT,
#                        design = list(min_SEM=0.5),
#                        start_item = 'Trule',
#                        design_elements = TRUE)
#   if (is.null(questions)) {
#     return(CATdesign)
#   }
#   CATdesign <- updateDesign(CATdesign, items=questions, responses=answers)
#   CATdesign$design@Update.thetas(design=CATdesign$design, person=CATdesign$person, test=CATdesign$test)
#   CATdesign$person$Update.info_mats(design=CATdesign$design, test=CATdesign$test)
#   CATdesign
# }
titleForQuestion <- function(questionIx) {
  .design.df$Question[[questionIx]]
}

optionTextsForQuestion <- function(questionIx) {
  .design.options[questionIx,]
}

questionIxMatching <- function(text) {
  match(text, .design.df$Question)
}

# Run the orign, shiny version of same test
shiningPath <- function() {
  GUI = list(title = "Shiny Survey",
             authors = "",
             stem_default_format = shiny::h5,
             lastpage = function(person){return(list(h5("Thanks for taking the survey.
                                                      To exit, please click the action button")))},
             time_before_answer = 3 # minimum amount of time for answering
  )
  mirtCAT(.design.df, .design.mo,
          preCAT = .design.preCAT,
          design = .design.design,
          start_item = .design.start_item,
          shinyGUI = GUI)
}
