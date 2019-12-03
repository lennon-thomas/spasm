library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
source('../R/sim_fishery_az.R',local=FALSE,echo=FALSE)
source('../R/plot-spasm.R',local=FALSE,echo=FALSE)
boxdir<-"/Users/lennonthomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"test"

juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
shore_dist<-read.csv(paste0(boxdir,runname,"/distance_to_shore.csv"))

hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))
num_patches<-length(unique(adult_distance$from))
# Create dataframe that has cell_no, patch_no, whether juve or adult, and habitat quality
cell_lookup<-data.frame(matrix(ncol=4,nrow=num_patches))
cell_lookup[,1]<-c(1:20)
cell_lookup[,2]<-unique(adult_distance$from)
cell_lookup[,3]<-c(rep(0,10),rep(1,10))
cell_lookup[,4]<- hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))
colnames(cell_lookup)<-c("patch","cell_no","juve_ad_hab","hab_qual")
 # Adult habitat quality

fish <-
  create_fish(
    scientific_name = "Pagellus bogaraveo",
    query_fishlife = T,
    mat_mode = "length",
    time_step = 1,
    cv_len = 0,
    sigma_r = 0.00,
    steepness = 0.8,
    r0 = 20000,#10972.933*1000, #trying to get to K from best Jabba run
    rec_ac = 0,
    adult_movement = 200,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  0.2,
    price = 940,
    price_cv = 0,
    price_ac = 0,
    price_slope =  0.0001

  )


fleet <- create_fleet(
  fish = fish,
  q = 1e-3,
  cost = 0.1,
  cost_cv =  0,
  cost_ac = 0,
  cost_slope = 0.0001, #This has to be >0 in order for distance from shore to be considered cost but increases costs significantly
  q_cv = 0.01,
  q_ac = 0,
  q_slope = 0,
  #eq_f = .1,
  b_ref_oa = 0.25,#0.25,
  max_cr_ratio = 0.001, ## this is cost revene ratio- the higher the number the higher the costs.  this is how you change economics.
  fleet_model = "open-access",
  sigma_effort = 0.1,
  length_50_sel = 0.1 * fish$linf,
  initial_effort = 0,#.002,
  beta = 1,
#  theta = 1e-1,
  max_perc_change_f = 2,
  effort_allocation = "profit-gravity", #"gravity", #'simple',
  mpa_reaction = "leave",#"leave", #"leave"
  profit_lags = 10, # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.
  cost_function = "distance to port")




system.time(simple <- sim_fishery_az_test(
  fish = fish,
  fleet = fleet,
  manager = create_manager(mpa_size = 0, year_mpa = 100),
  num_patches = 20,
  sim_years = 20,
  burn_years = 100,
  time_step = fish$time_step,
  #est_msy = FALSE,
  random_mpas =TRUE,
  min_size = 0.05,
  mpa_habfactor = 1,
  sprinkler = TRUE,
  keep_burn = TRUE,
  adult_distance = adult_distance,
  juve_adult_distance = juve_adult_distance,
  adult_juve_distance = adult_juve_distance,
  juve_distance = juve_distance,
  shore_dist = shore_dist,
  hab_qual = hab_qual,
  rec_driver = "stochastic",
  tune_costs = FALSE
))

#During sim- pop in all adult and juve patches are the same?

simple %>%
  filter(year == max(year)) %>%
  # mpa == TRUE) %>%
  dplyr::group_by(patch)

plot_spasm(simple, type = "patch", font_size = 12)

plot_spasm(simple, type = "totals", font_size = 12)
