#' A wrapper around mirtCAT#updateDesign() 
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
