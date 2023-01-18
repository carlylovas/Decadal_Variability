##Run NECC Survey Prep and SDM code##
clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)

survey_tows <- get_survdat_tows(clean_survey)
summary(survey_tows)

