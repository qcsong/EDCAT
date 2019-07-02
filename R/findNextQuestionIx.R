#' A wrapper around mirtCAT#updateDesign() 
findNextQuestionIx <- function(design.elements, questions, answers, design=list(min_SEM_0.5)) {
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
