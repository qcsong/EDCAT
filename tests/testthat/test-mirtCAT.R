# This installs correct version of mirt and mirtCAT. One in CRAN may be different
if (FALSE) {
  # a github personal access token. See https://github.com/settings/tokens
  git.token <- readLines('../../GIT-TOKEN')
  devtools::install_github(repo='philchalmers/mirt', auth_token = git.token)
  devtools::install_github(repo='philchalmers/mirtcat', auth_token = git.token)
}

test_that("can call mirtCAT()", {
  data(CATDesign)
  designElements <- mirtCAT(df, mod, criteria = 'KL', start_item = 'Trule',
                       design_elements = TRUE,
                       design = list(min_SEM = rep(0.4, 3),
                                     max_items = ncol(data_epsi1a),
                                     delta_thetas = rep(0.03, 3)))
  expect_named(designElements)
  # Need some more tests that we are getting expected result
  #str(designElements)
})
 