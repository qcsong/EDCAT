test_that("can call mirtCAT()", {
  devtools::install_github(repo='philchalmers/mirt', auth_token = '8c55601cbf6cac95d9c6a71aa4fec0f10a81aa55')
  devtools::install_github(repo='philchalmers/mirtcat', auth_token = '8c55601cbf6cac95d9c6a71aa4fec0f10a81aa55')
  library(mirtCAT)
  data(CATDesign)
  designElements <- mirtCAT(df, mod, criteria = 'KL', start_item = 'Trule',
                       design_elements = TRUE,
                       design = list(min_SEM = rep(0.4, 3),
                                     max_items = ncol(data_epsi1a),
                                     delta_thetas = rep(0.03, 3)))
  expect_named(designElements)
})
 