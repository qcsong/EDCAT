context('pilrContentAPI')

source('sample-parameters.R')

library(mirtCAT)
data(CATDesign)

dummyMirtCatDf <-
  data.frame(Question = c('question 1', 'question 2', 'question 3'),
           Option.1 = c('0-q1.opt1', '0-q2.opt1', '0-q3.opt1'),
           Option.2 = c('1-q1.opt2', '1-q2.opt2', '1-q3.opt2'),
           Option.3 = c('2-q1.opt3', '2-q2.opt3', '2-q3.opt3'),
           Option.4 = c('3-q1.opt4', '3-q2.opt4', '3-q3.opt4'),
           stringsAsFactors = FALSE)
dummyMirtCatDf <- df

test_that("extracts questions and options correctly", {
  
  answers <- NULL
  questions <- NULL
  dummyCompute <- function(de, qs, as) {
    questions <<- qs
    answers <<- as
    1
  }

  result <- pilrContentApi('myPt', resultsSoFar, sourceCard,
                             computeFn = dummyCompute,
                             mirtCatDataFrame = dummyMirtCatDf)
  expect_equal(questions, c(2, 3, 4, 6))
  expect_equal(answers, c(20, 30, 40, 60))
})

test_that("returns correct question as a card", {
  opt.filter = grepl('Option.*', names(dummyMirtCatDf))
  for (i in 1:3) {
    result <- pilrContentApi('myPt', resultsSoFar, sourceCard,
                               computeFn = function(x,y,z) { i },
                               mirtCatDataFrame = dummyMirtCatDf)
    expected.opts = lapply(list(1,2,3,4,5),
                         function (optix) {
                            list(value = paste(optix), name = dummyMirtCatDf[[i, paste0('Option.', optix)]])
                         })
    expect_equal(result$section, 1)
    expect_equal(result$data$title, dummyMirtCatDf$Question[[i]])
    expect_equal(result$data$text, '')
    expect_equal(length(result$data$options), length(names(dummyMirtCatDf)[opt.filter]))
    for (optix in 1:length(result$data$options)) {
      expect_equal(result$data$options[[optix]]$value, paste(optix-1))
      expect_equal(result$data$options[[optix]]$name, substring(dummyMirtCatDf[[i, paste0('Option.', optix)]], 3))
    }
  }
})
