context('findNextQuestionIx')

test_that("yields same sample question sequence as shiningPath()", {
  result <- findNextQuestionIx(questions = c(), answers = c())
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
    expect_equal(retVal$extraValues, list()) 
    
    qs <- c(qs, nextQ)
   }
})

test_that('terminating example works correctly', {
  # Sample, terminating sequence for given current design data
  questions<-c(1, 40, 30, 22, 3, 6, 26, 2, 5, 7, 29, 31, 28, 44, 24, 16, 17, 18, 19, 20, 21, 23, 25, 27, 32, 33, 34, 35, 36, 37, 38)
  answers  <-c(4,  3,  4,  2, 1, 0,  2, 3, 4, 1,  2,  3,  4,  0,  3,  4,  1,  2,  3,  4,  0,  2,  4,  3,  2,  1,  0,  1,  2,  3,  4)

  retVal <- findNextQuestionIx(questions, answers)
  
  expect_equal(retVal$questionIx, NA)
  expect_length(retVal$extraValues, 3)
  for (value in retVal$extraValues) {
    if (value$question_code == 'terminated_successfully') {
      expect_equal(value$response, 'yes')
    }
    else if (value$question_code == 'thetas') {
      expect_equal(value$response_values, c(1.151441, 1.333754, 1.354088),
                   tolerance = .0001, scale = 1)
    }
    else if (value$question_code == 'SE_thetas') {
      expect_equal(value$response_values, c(0.3036354, 0.2384548, 0.4011720),
                   tolerance = .0001, scale = 1)
    }
    else {
      expect_equal(value, 'unknown')
    }
  }
})

exRetVal <- list( questionIx=6,  extraValues=list( list(question_type='q_yesno', question_code='terminated_successfully', response='yes', response_value='yes'), list(question_type='q_select_multiple', question_code='thetas', responses=c("1.400576", "-4.580430e-07", "-4.580430e-07"), response_values=c(1.400576, -4.580430e-07, -4.580430e-07)) )  )