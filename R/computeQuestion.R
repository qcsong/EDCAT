##################################################################################
## Pseudocode
# mirtCAT()
# {
#   i <- 0
#   answers = c()
#   done <- false
#   while not done:
#     nextQuestion, done <- computeQuestion(answers)
#   If done exit
#   nextAnswer <- askQuestion(QuestionDataframe[ i ]
#                             answers <- c(answers, (nextQuestion, nextAnswer))
#                             end
# }
#
# The function, "computeQuestion()" uses the user's responses to
# each question (input) and outputs a) the index of the next question or
# b) tell the app to stop showing questions when certain thresholds were met.
# "QuestionDataframe" is the list of questions and corresponding options.
##################################################################################

# #' example_EDCAT
# #' Input data for computeQuestion() example
# #'  Note. Variables:
# #'  mod, df, options, questions, data_epsi1a, data_epsi2a, data_epsi3a, data_epsi4a, data_items,
# #'  lab_bdis, lab_binge, lab_cogres, lab_exerc, lab_mbuild, lab_negatt, lab_purge, lab_rest
# "example_EDCAT"

#' computeQuestion
#'
#' Find the question ID for the next question to be administered in CAT
#' @param CATdesign mirtCAT design object, obtained using mirtCAT(), see example
#' @param askedQuestions The ID's of the questions administered so far
#' @param responses The responses to each questions administered so far
#' @return nextQuestion: the ID of the next question to administer or "done", indicating the termination of CAT
#' @examples
#' # Specify CATdesign object
#' data(CATdesign)
#' CATdesign <- mirtCAT(df, mod, criteria = 'KL', start_item = 'Trule',
#'                      design_elements = TRUE,
#'                      design = list(min_SEM = rep(0.4, 3),
#'                                    max_items = ncol(data_epsi1a),
#'                                    delta_thetas = rep(0.03, 3)))
#' # Questions #1 and #5 were previously administered
#' askedQuestions = c(1,5)
#' # The respondent's response to the previously administered questions (i.e., Questions #1 and #5) were Options 1 and 2
#' responses = c(1,2)
#' nextQuestion <- computeQuestion(CATdesign = CATdesign, askedQuestions = askedQuestions, responses = responses)
#' # The ID of the next question to administer
#' nextQuestion
#'
#' @export
computeQuestion <- function(CATdesign, askedQuestions, responses){

  updatedCATdesign <- updateDesign(CATdesign, items = askedQuestions, responses = responses)

  nextQuestion <- findNextItem(x = updatedCATdesign, criteria = 'KL')
  if(is.na(nextQuestion)){nextQuestion = "done"}

  return(nextQuestion)
}
