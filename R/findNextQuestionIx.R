#' A facade for mirtCAT used by pilrContentApi()

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
findNextQuestionIx <- function(questions, answers, design=list(min_SEM_0.5)) {
  if (is.null(questions)) {
    return(1)
  }
  
  # need to build fresh every time because update.disgn modifies it
  design.elements <- mirtCAT(mirtCAT.df, mirtCAT.mo, 
                             preCAT = mirtCAT.preCAT,
                             design_elements = TRUE)
  updateDesign(design.elements, items=questions, responses=answers)
  findNextItem(design.elements)
}

titleForQuestion <- function(questionIx) {
  mirtCAT.df$Question[[questionIx]]
}

optionTextsForQuestion <- function(questionIx) {
  mirtCAT.options[questionIx,]
}
