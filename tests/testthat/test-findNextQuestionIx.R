context('findNextQuestionIx')

test_that("yields same sample question sequence as shiningPath()", {
  result <- findNextQuestionIx(questions = c(), answers = c())
  str('wtf')
  str(result)
  expect_equal(1, result$questionIx )
  
  expect_equal(findNextQuestionIx(questions = c(1), answers = c(4))$questionIx, 
               questionIxMatching(' People told me that I do not eat very much'))
  
  shinySession <- data.frame(
    question=c('I did not like how clothes fit the shape of my body', 
               'People told me that I do not eat very much', 
               'I engaged in strenuous exercise at least five days per week',
               'I used muscle building supplements'),
    answer=c(4,4,4,4)
  )
  
  qs <- c()
  for (i in 1:nrow(shinySession)) {
    retVal <- if (i == 1) 
      findNextQuestionIx(c(), c())
    else {
      findNextQuestionIx(qs, shinySession$answer[c(1:(i-1))])
    }
    nextQ <- retVal$questionIx
    expect_equal(titleForQuestion(nextQ),
                 paste0(' ', shinySession$question[[i]]))
    #expect_equal(retVal$extraValues, list()) 
    
    q <- c(qs, nextQ)
   }
})

exRetVal <- list( questionIx=6,  extraValues=list( list(question_type='q_yesno', question_code='terminated_successfully', response='yes', response_value='yes'), list(question_type='q_select_multiple', question_code='thetas', responses=c("1.400576", "-4.580430e-07", "-4.580430e-07"), response_values=c(1.400576, -4.580430e-07, -4.580430e-07)) )  )