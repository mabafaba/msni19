context("test-match_guidance_examples")


all_input_combinations<-
  expand.grid(list(
    # msni = 1:4,
    fsl = 1:4,
    wash = 1:4,
    capacity_gaps = 1:4,
    protection = 1:4,
    shelter = 1:4,
    health = 1:4,
    impact = 1:4,
    education = 1:4))



all_input_combinations$calculated_msni<-with(all_input_combinations,{
  msni(education_lsg = education,
         fsl_lsg = fsl,
         health_lsg = health,
         protection_lsg = protection,
         shelter_lsg = shelter,
         wash_lsg = wash,
         capacity_gaps = capacity_gaps,
         impact = impact)
})


test_that("examples match", {
  # expect_true(TRUE)
})
