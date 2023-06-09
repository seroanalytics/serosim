test_that("Check that observation_model_continuous function works", {
  
  ## Load in example data and necessary arguments
  obs_tmp<- observation_model_continuous(example_biomarker_states, NULL)

  ## Expect that observation_model_continuous function output equals 5.163667
  expect_equal(obs_tmp$observed[642], 5.163667, tolerance=0.000001)
})


test_that("Check that observation_model_continuous_bounded function works", {
  
  ## Load in example data and necessary arguments
  bounds <- tibble(biomarker_id=1,name=c("lower_bound","upper_bound"),value=c(2,8))
  obs_tmp<-observation_model_continuous_bounded(example_biomarker_states, NULL,bounds)

  ## Expect that observation_model_continuous_bounded function output equals 5.576374
  expect_equal(obs_tmp$observed[870], 5.576374, tolerance=0.000001)
})


test_that("Check that observation_model_discrete function works", {

  ## Load in example data and necessary arguments
  breaks <- seq(0,8,by=1)
  cutoffs <- matrix(breaks,nrow=1,ncol=length(breaks))
  obs_tmp<-observation_model_discrete(example_biomarker_states, NULL,cutoffs)

   ## Expect that observation_model_discrete function output equals 3
  expect_equal(obs_tmp$observed[12000],3)
 })


test_that("Check that observation_model_continuous_bounded_noise function works", {
  
  ## Load in example data and necessary arguments
  bounds <- tibble(biomarker_id=1,name=c("lower_bound","upper_bound"),value=c(2,8))
  obs_tmp<-observation_model_continuous_bounded_noise(example_biomarker_states %>% drop_na(), example_model_pars_numeric, bounds,0.95,1)
  
  ## Expect that observation_model_continuous_bounded_noise function output equals 7.8
  expect_equal(obs_tmp$observed[89],5.3, tolerance=2)
})


test_that("Check that observation_model_continuous_noise function works", {
  
  ## Load in example data and necessary arguments
  
  ## Expect that observation_model_continuous_noise function works properly with no errors
  expect_message(observation_model_continuous_noise(example_biomarker_states%>% drop_na(), example_model_pars_numeric, 0.95,0.99), regexp = NA)
})


test_that("Check that observation_model_discrete_noise function works", {
  
  ## Load in example data and necessary arguments
  breaks <- seq(0,8,by=1)
  cutoffs <- matrix(breaks,nrow=1,ncol=length(breaks))
  tmp_pars <- example_model_pars_numeric %>% mutate(sd=ifelse(name=="obs_sd",2,sd))
  
  ## Expect that observation_model_discrete_noise function works properly with no errors
  expect_message(observation_model_discrete_noise(example_biomarker_states %>% drop_na(), tmp_pars, cutoffs, 0.95,0.99), regexp = NA)
})


