# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# This script does WHAAAAAAAAAAAAT

#**Inputs**



#**Outputs**



# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# HOUSEKEEPING ----

# **************************************************************

## Center yourself ----

### Where are we???????????

here::i_am("02_train_test_models.R")

# **************************************************************

## Packages ----

### General 

require(here)

### Data mgmt & processing

require(tidyverse)
require(cluster)

### File mgmt

require(usethis)
require(fs)

### Random forest
require(xgboost)
require(lightgbm)
require(fastshap)
require(rsample)
require(tidymodels)
require(bonsai)

### Data viz
require(ggthemes)
require(shapviz)
require(wacolors)
require(ggfortify)

# **************************************************************

## Load custom functions ----

source(here("00_functions.R"))

# **************************************************************

## And set script-wide vars ----

### Map projection

map_crs <- 26913 # NAD83 / UTM zone 13N

map_proj <- "EPSG:26913"

### Email for LF download

em <- "john.kemper@usu.edu"

# **************************************************************

# //////////////////////////////////////////////////////////////////////////////

# **************************************************************

# Read in fire data ----

## Read data ----

fire_files <- dir_ls(here("output-data", "fire-features"), glob = "*.csv")

dnbr_and_drivers <- map_dfr(fire_files, read_fire_files)

all_fire_metadata <- read_csv(here("output-data", "all_fires_metadata.csv"))

# **************************************************************


## Modify data ----

### Add fire_index so we can loop over each fire
### Remove pixels that have CC less than 10%
### And make sure variables are of the correct type
### And then cut out likely invalid/erroneous values using a threshold of -600
### Here is the reference for that number: https://www.fs.usda.gov/rm/pubs_series/rmrs/gtr/rmrs_gtr164/rmrs_gtr164_13_land_assess.pdf

dnbr_and_drivers <- dnbr_and_drivers %>%
  filter(cc > 10) %>%
  filter(dnbr > -600) %>%
  dplyr::group_by(fire_id) %>%
  dplyr::ungroup() %>%
  mutate(across(any_of(c("evh", "evt", "evc", "fbfm40", "fccs")), 
                ~as.factor(.))) %>%
  mutate(across(any_of(c("cbd", "cbh", "cc", "ch")), 
                ~as.integer(.)))

all_fire_metadata <- all_fire_metadata %>%
  filter(fire_id %in% unique(dnbr_and_drivers$fire_id))


## Split data ----

#### Make training/validation and testing splits
#### We'll use the training/validation to tune
#### He're we're dividing our data up into 65% training/validation
#### and 35% testing

set.seed(666)

g_split <- rsample::initial_split(all_fire_metadata,
                       prop = 0.65,
                       strata = acres_within_fire_perimeter)

train_groups <- rsample::training(g_split)$fire_id

test_groups <- rsample::testing(g_split)$fire_id


### Now add the train & test groups
### to the fire metadata so we can know what fire is in which group
### and develop an index number based on these splits
### It makes it easier to do the LOOCV looping later
### If the testing fires are indexed 1-x
### and the training data are index x-157

all_fire_metadata <- all_fire_metadata %>%
  mutate(train_test = ifelse(fire_id %in% train_groups, "train_valid", "test")) %>%
  arrange((train_test)) %>%
  mutate(usu_fire_index = row_number()) %>%
  dplyr::ungroup()

dnbr_and_drivers <- dnbr_and_drivers %>%
  inner_join(., all_fire_metadata %>%
               dplyr::select(fire_id, usu_fire_index),
             by = "fire_id")

train_valid <- dnbr_and_drivers %>%
  filter(fire_id %in% train_groups)

held_out_test <- dnbr_and_drivers %>%
  filter(fire_id %in% test_groups)

nrow(train_valid)/(nrow(train_valid) + nrow(held_out_test))

rm(dnbr_and_drivers)

# **************************************************************


# held_out_test_lookup <- held_out_test %>%
#   dplyr::group_by(fire_id) %>%
#   dplyr::summarise(fire_id = fire_id[1],
#                    usu_fire_index = usu_fire_index[1])
# 
# all_fire_lookup <- all_fire_metadata %>%
#   dplyr::group_by(fire_id) %>%
#   dplyr::summarise(fire_id = fire_id[1],
#                    usu_fire_index = usu_fire_index[1])

#write_csv(all_fire_lookup, here("output-data", "lookup-tables, "all_fire_lookup.csv"))


# **************************************************************

# //////////////////////////////////////////////////////////////////////////////

# **************************************************************

# Data exploration ----

## Basic distributions ----

train_valid %>%
  ggplot() +
    geom_histogram(aes(x = npp), fill = "darkgreen", color = "black",
                   alpha = 0.8,
                   bins = 100
                   ) +
    theme_bw()


## Basic scatterplots ----

dnbr_lat_p <- train_valid %>%
  ggplot() +
  geom_bin2d(aes(x = dnbr , y = y),
             color = NA,
             bins = 50,
             ) +
  scale_fill_viridis_c(option = "turbo",
                       #breaks = trans_breaks("log10", function(x) 10^x),
                       #labels = trans_format("log10", math_format(10^.x)),  
                       guide = guide_colorbar(barheight = unit(0.6, "cm"),
                                              barwidth  = unit(10, "cm")),
                       name = "Count") + 
  scale_x_continuous(limits = c(-500, 1300)) +
  #scale_y_continuous(limits = c(-500, 1300)) + 
  #coord_equal(ratio = 1) + 
  labs(x = "Observed dNBR",
       y = "Latitude (UTM)") + 
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "gray90")) + 
  pres_theme

dnbr_long_p

ggsave("plots/dnbr_by_long.png",
       dnbr_long_p, 
       height = 6, width = 6,
       dpi = 300,
)


dnbr_lat_p

ggsave("plots/dnbr_by_lat.png",
       dnbr_lat_p, 
       height = 6, width = 6,
       dpi = 300,
)


ggpubr::ggarrange(dnbr_lat_p, dnbr_long_p, nrow = 2)



train_valid %>%
  ggplot() +
  geom_bin2d(aes(x = npp , y = dnbr),
             color = NA,
             bins = 100,
  ) +
  geom_smooth(aes(x = npp, y = dnbr),
              formula = y~x,
              method = "lm",
              color = "gray50",
              linewidth = 0.5) + 
  scale_fill_viridis_c(option = "turbo",
                       guide = guide_colorbar(barheight = unit(0.6, "cm"),
                                              barwidth  = unit(10, "cm")),
                       name = "Count") + 
  scale_y_continuous(limits = c(-500, 1300)) +
  labs(x = "Net Primary Prod.",
       y = "Observed dNBR") + 
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "gray90")) + 
  pres_theme


## PCA ----

pca_dnbr <- train_valid %>%
  dplyr::select(!usu_fire_index) %>%
  dplyr::select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

## K-medoids ----

clust_cv <- train_valid %>%
  dplyr::slice(1:50000) %>%
  dplyr::select(!c(usu_fire_index, fire_id, fire_name, group_index))   

g_dist_cv <- cluster::daisy(clust_cv, metric = "gower")

sil_width <- list()

for(i in 1:12){
  
  cat(crayon::yellow("\n Calculating PAM with ", i, "clusters \n"))
  
  pam_fit <- cluster::pam(clust_cv, diss = TRUE, k = i)
  
  sil_width[[i]] <- tibble(avg_sil_width = pam_fit$silinfo$avg.width,
                         k = i)
  
}

sil_width_all <- bind_rows(sil_width)

sil_width_all %>%
  ggplot() +
    geom_point(aes(x = k, y = avg_sil_width),
               color = "black",
               size = 3, shape = 16) + 
    geom_line(aes(x = k, y = avg_sil_width),
               color = "black",
               linewidth = 1) +
    scale_x_continuous(breaks = seq(0,12,2)) + 
    theme_bw()


pam_fit <- cluster::pam(clust_cv, diss = TRUE, k = 7)



pam_results <- clust_cv %>%
  mutate(cluster = pam_fit$clustering) %>%
  bind_cols(., train_valid %>%
              dplyr::slice(1:10000) %>%
              dplyr::select(dnbr))



pam_results %>%
  dplyr::select(!dnbr) %>%
  mutate(cluster = as.factor(cluster)) %>%
  dplyr::select(cluster, where(is.numeric)) %>%
  pivot_longer(cols = -cluster, values_to = "value",
               names_to = "feature") %>%
  ggplot() +
    geom_boxplot(aes(x = cluster, y = value, fill = cluster)) +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~feature, scales = "free")


cluster::c


# //////////////////////////////////////////////////////////////////////////////

# **************************************************************

# **************************************************************

# Train & Tune Models ----

# **************************************************************

## Bespoke ----

### Train & Validate ----

#### First we want to make a five-fold CV split
#### For our training and validation data
#### So that we can use 5-fold CV to train the hypers

#### Make splits for tuning 
#### By splitting into 5-fold cross validation
#### Where each testing dataset has twenty fires 
### and each training has eighty fires

cv <- 5

train_valid <- train_valid %>%
  mutate(group_index = ceiling(dense_rank(usu_fire_index)/
                                 (length(unique(.$usu_fire_index))/cv)))



#### Then, we want to tune 

#### First, get ranges to examine the parameters we wish to tune and
#### construct the grid over which we are going to do the search 

model_grid <- grid_space_filling(
                   type = "max_entropy",
                   num_leaves(c(32,512)),
                   min_n(c(50,500)),
                   #trees(c(500,2000)),
                   #tree_depth(c(2,12L)),
                   learn_rate(c(0.1, 0.01), trans = NULL),
                   sample_size = sample_prop(),
                   finalize(mtry_prop(), train_valid),
                   size = 100
                   ) %>%
  mutate(learn_rate = round(learn_rate, 2)) %>%
  mutate(trees = as.integer(1/learn_rate*20))

# Now, do the tuning

i <- 0

#nrow(model_grid)
for(i in 0:nrow(model_grid)) {
  
  ### First, pick out the hypers 
  
  if(i == 0){
    
    hypers <- NULL
    
  } else (hypers <- model_grid[i, ])
  

  
  ### Now, loop over our 5-fold CV

  hypers_plus <- list()
  
  for(j in 1:length(unique(train_valid$group_index))) {

    cat(crayon::green("\nTesting on hyper index #", i, "\n"))
    
    cat(crayon::yellow("\nAnd on CV #", j, "\n"))
    
    ### Carve out the training and testing data
    
    train <- train_valid %>%
      filter(group_index != j) %>%
      dplyr::select(!c(usu_fire_index,
                       group_index))
    
    valid <- train_valid %>%
      filter(group_index == j) %>%
      dplyr::select(!c(usu_fire_index,
                       group_index))
    
    
    ### Run the LightGBM with them
    
    lgbm_out <- run_lgbm(train_df = train, 
                         test_df = valid, 
                         tgt_var = "dnbr", 
                         objective = "quantile", 
                         application = "tuning",
                         hyperparams = hypers, 
                         alpha = 0.5,
                         explain = TRUE,
                         plot_shaps = FALSE)
    
    ### Caluclate and extract the performance stats 
    
    perf_stats <- evaluate_preds(lgbm_out[[1]],
                                 tgt_var = "dnbr")
    
    
    hypers_plus[[j]] <- bind_cols(hypers, 
                                  perf_stats[3]) %>%
      mutate(hyper_index = i,
             group_index = j)
    
    ### If we're running only the default hypers, also save predictions
    ### And shap values and stuff
    
    if(is.null(hypers)){
      
      
      shv_b <- lgbm_out[[3]] %>%
        mutate(valid_split = j)
      
      shv_shap <- lgbm_out[[4]] %>%
        bind_cols(valid %>% dplyr::select(fire_id) %>%
                    rename(test_fire_id = fire_id))
      
      shv_feats <- lgbm_out[[5]] %>%
        bind_cols(valid %>% dplyr::select(fire_id) %>%
                    rename(test_fire_id = fire_id))
      
    
    write_csv(perf_stats[[1]],
              here("output-data",
                   "train-valid",
                   paste0("dnbr_preds_tv_split",
                         j,
                         ".csv")))
      
      write_csv(shv_b, here("output-data",
                            "train-valid",
                            paste0("shap_baseline_tv_split",
                                   j,
                                   ".csv")))
      
      write_csv(shv_shap, here("output-data",
                               "train-valid",
                               paste0("shap_values_tv_split",
                                     j,
                                      ".csv")))
      
      write_csv(shv_feats, here("output-data",
                                "train-valid",
                                paste0("shap_feature_mags_tv_split",
                                       j,
                                       ".csv")))
      
    }
    
    cat(crayon::yellow("\n Takin out deh traaaaaash\n"))
    
    gc()
    
  }
  
  if(is.null(hypers)) {
    
    next
  }
  
  all_hypers <- bind_rows(hypers_plus)
  
  ### And write to file 
  
  if(i == 1) {
    
    write_csv(hypers_plus, 
              here("output-data", "hyper-tuning",
                   "hypers_and_perf_cv.csv"),
              append = FALSE)
    
  } else if (i > 1) {
    
    
    write_csv(hypers_plus, 
              here("output-data", "hyper-tuning",
                   "hypers_and_perf_cv.csv"),
              append = TRUE)
    
    
    
  }

}

# **************************************************************

### Plot the hypers ----

### Bring in the gridded search results

potential_hypers <- read_csv(here("output-data", "hyper-tuning",
                         "hypers_and_perf_cv.csv")) %>%
  dplyr::select(!group_index) %>%
  dplyr::select(!mape)

### And the default numbers

default_hypers <- read_csv(here("output-data", "hyper-tuning",
              "hypers_and_perf_cv_DEFAULT_HYPERS.csv")) %>%
  dplyr::select(!group_index) %>%
  dplyr::select(!mape) %>%
  filter(!is.na(hyper_index)) 

### Let's pivot things longer

hyper_cols  <- c("num_leaves", "min_n", "learn_rate", "sample_size", "mtry_prop", "trees")

metric_cols <- c("mae", "rmse", "pbias", "nse", "mean_predicted_dnbr")

### First pivot the metrics and take some summary stats

metric_long <- potential_hypers %>%
  pivot_longer(cols = all_of(metric_cols),
               names_to = "metric",
               values_to = "metric_value") %>%
  dplyr::select(!all_of(hyper_cols)) %>%
  dplyr::group_by(hyper_index, metric) %>%
  summarise(median = median(metric_value),
         iqr  = IQR(metric_value)) %>%
  dplyr::ungroup()

### Then, pivot the hypers

hyper_long <- potential_hypers %>%
  dplyr::group_by(hyper_index) %>%
  dplyr::slice(1) %>% # Get only the first row, cause hypers are repeated
  pivot_longer(cols = all_of(hyper_cols),
               names_to = "hyperparameter",
               values_to = "hyper_value") %>%
  dplyr::select(!all_of(metric_cols))

### Now, join together

potential_hypers_long <- inner_join(hyper_long, metric_long,
                                    by = "hyper_index")

best_hypers_long1 <- potential_hypers_long %>%
  filter(!metric %in% c("mean_predicted_dnbr", "nse")) %>%
  dplyr::group_by(hyperparameter, metric) %>%
  dplyr::slice_min(abs(median))

best_hypers_long2 <- potential_hypers_long %>%
  filter(metric %in% c("nse")) %>%
  dplyr::group_by(hyperparameter, metric) %>%
  dplyr::slice_max((median))

best_hypers_long <- bind_rows(best_hypers_long1, best_hypers_long2)

### And pivot the default long too
default_hypers_long <- default_hypers %>%
  dplyr::select(hyper_index, all_of(metric_cols)) %>%
  pivot_longer(cols = all_of(metric_cols),
               names_to = "metric",
               values_to = "metric_value") %>%
  dplyr::group_by(metric) %>%
  summarise(median = median(metric_value),
            iqr  = IQR(metric_value)) %>%
  dplyr::ungroup() %>%
  filter(metric != "mean_predicted_dnbr")


### And plot

potential_hypers_long %>%
  filter(metric != "mean_predicted_dnbr") %>%
  ggplot() +
    geom_hline(data = default_hypers_long, 
               aes(yintercept = median),
               color = "gray50") +
    geom_hline(data = default_hypers_long, 
               aes(yintercept = median + iqr),
               color = "gray50",
               linetype = "dashed") +
    geom_hline(data = default_hypers_long, 
               aes(yintercept = median - iqr),
               color = "gray50",
               linetype = "dashed") +
    geom_line(aes(x = hyper_value,
                  y = median,
                  color = hyperparameter),
              linewidth = 0.5) +
    geom_ribbon(aes(x = hyper_value, 
                      ymin = median - iqr, 
                      ymax = median + iqr,
                      fill = hyperparameter),
                  alpha = 0.1) +
    geom_point(aes(x = hyper_value, y = median, 
                   color = hyperparameter),
               shape = 19, size = 1) +
    geom_vline(data = best_hypers_long,
               aes(xintercept = hyper_value),
               color = "black",
               linetype = "dotted") + 
    scale_color_brewer(palette = "Dark2", guide = "none") +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    scale_x_log10() + 
    #scale_y_log10() + 
    labs(x = "Hyperparamter Value",
         y = element_blank()) + 
    theme_bw() +
    facet_grid(metric~hyperparameter, 
               scales = "free") +
    theme(strip.background = element_rect(fill = NA,
                                          color = NA),
          strip.placement = "outside",
          plot.title = element_text(hjust = 0.5))

### And rank

#### By metric & hyperparameter

potential_hypers_ranks <- potential_hypers_long %>%
  filter(metric != "mean_predicted_dnbr") %>%
  dplyr::group_by(hyperparameter, metric) %>%
  mutate(median_rank = ifelse(metric == "nse",
                              dense_rank(desc(median)),
                              dense_rank(median))) %>%
  mutate(iqr_rank = dense_rank(iqr)) %>%
  mutate(overall_score  = 0.7*median_rank + 0.3*iqr_rank) %>%
  mutate(overall_rank = dense_rank(overall_score)) %>%
  dplyr::ungroup()


#### And get the ranges of hyper values
#### that span the top ranks for each metric-hyperparameter combo

top_rank_hyper_ranges <- potential_hypers_ranks %>%
  dplyr::group_by(hyperparameter, metric) %>%
  filter(overall_rank == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(hyperparameter) %>%
  summarise(min_hyper_value = min(hyper_value),
            max_hyper_value = max(hyper_value))


# **************************************************************
# **************************************************************

## Tidymodels ----

####### Note that this is super slow and it is probably best to use the bespoke model training

### First, split the data

splits <- split_data(train_valid)

### Now, set the model recipe

lgbm_rec <- recipe(dnbr ~ ., data = train_valid) %>%
  update_role(fire_id, fire_name, usu_fire_index, 
              group_index,
              new_role = "ID") %>%
  #step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors())

### Then, the metrics used

mets <- metric_set(mae, rmse, rsq, mape, mpe, rsq_trad)

mets_trim <- metric_set(mae, rmse, rsq, rsq_trad)
  
### And model specs

lgbm_mod <- parsnip::boost_tree(mode = "regression",
                       min_n = tune(),
                       trees = tune(),
                       mtry = tune(),
                       sample_size = tune(),
                       learn_rate = tune()) %>%
  set_mode("regression") %>%
  set_engine("lightgbm", 
             num_leaves = tune(),
             bagging_freq = 1,
             counts = TRUE,
             metric = "mae",
             num_threads = 0,
             verbose = 1L)

### Now, build the workflow 
lgbm_wf <- workflow() %>%
  add_model(lgbm_mod) %>%
  add_recipe(lgbm_rec)

### And set a refined grid search for tuning 

model_grid_refined <- grid_space_filling(
  type = "max_entropy",
  num_leaves(c(top_rank_hyper_ranges$min_hyper_value[top_rank_hyper_ranges$hyperparameter == "num_leaves"], 
               top_rank_hyper_ranges$max_hyper_value[top_rank_hyper_ranges$hyperparameter == "num_leaves"])),
  min_n(c(top_rank_hyper_ranges$min_hyper_value[top_rank_hyper_ranges$hyperparameter == "min_n"], 
          top_rank_hyper_ranges$max_hyper_value[top_rank_hyper_ranges$hyperparameter == "min_n"])),
  learn_rate(c(top_rank_hyper_ranges$min_hyper_value[top_rank_hyper_ranges$hyperparameter == "learn_rate"], 
               top_rank_hyper_ranges$max_hyper_value[top_rank_hyper_ranges$hyperparameter == "learn_rate"]), 
             trans = NULL),
  sample_size = sample_prop(c(top_rank_hyper_ranges$min_hyper_value[top_rank_hyper_ranges$hyperparameter == "sample_size"], 
                              top_rank_hyper_ranges$max_hyper_value[top_rank_hyper_ranges$hyperparameter == "sample_size"])),
  finalize(mtry_prop(c(top_rank_hyper_ranges$min_hyper_value[top_rank_hyper_ranges$hyperparameter == "mtry_prop"], 
                       top_rank_hyper_ranges$max_hyper_value[top_rank_hyper_ranges$hyperparameter == "mtry_prop"])), 
           dnbr_and_drivers),
  size = 20) %>%
  mutate(trees = as.integer(1/learn_rate*20))

### First, modify the model grid for tidy models
model_grid_refined_tidy <- model_grid_refined %>%
  mutate(mtry = mtry_prop*
           lgbm_rec$var_info %>%
           filter(role == "predictor") %>%
           nrow()) %>% ### Bayes needs a number not a proportion, so multiply by predictor feats
  mutate(mtry = round(mtry, 0)) %>%
  dplyr::select(!mtry_prop)

#### First, let's do a grid search

lgbm_grid_res <- tune::tune_grid(object = lgbm_wf,
                              resamples = splits,
                              grid = model_grid_refined_tidy,
                              metrics = mets_trim,
                              control = tune::control_grid(verbose = TRUE))

#### Collect our metrics


collected_mets <- collect_metrics(lgbm_grid_res, summarize = TRUE)

write_csv(collected_mets,
          here("output-data", "hyper-tuning", "refined_grid_search_tuning_hypers.csv"))

#### And update our parameter space

params <- extract_parameter_set_dials(lgbm_wf)

refined_hyper_ranges <- model_grid_refined_tidy %>%
  summarise(across(everything(), list(min = min,
                                      max = max)))

refined_params <- params %>%
  update(
    num_leaves = num_leaves(c(refined_hyper_ranges$num_leaves_min, 
                              refined_hyper_ranges$num_leaves_max)),
    min_n = min_n(c(refined_hyper_ranges$min_n_min, 
                    refined_hyper_ranges$min_n_max)),
    learn_rate = learn_rate(c(refined_hyper_ranges$learn_rate_min,
                              refined_hyper_ranges$learn_rate_max), 
                            trans = NULL),
    sample_size = sample_prop(c(refined_hyper_ranges$sample_size_min, 
                                refined_hyper_ranges$sample_size_max)),
    mtry = mtry(c(refined_hyper_ranges$mtry_min, 
                  refined_hyper_ranges$mtry_max)),
    trees = trees(c(refined_hyper_ranges$trees_min, 
                    refined_hyper_ranges$trees_max))
  )


#### Then a bayesian optimization 


## DO it

lgbm_bayes_res <- lgbm_wf %>%
  tune_bayes( resamples = splits,
              initial = lgbm_grid_res,
              param_info = refined_params,
              iter = 20,
              metrics = mets,
              control = control_bayes(verbose = TRUE,
                                      verbose_iter = TRUE,
                                      seed = 666,
                                      no_improve = 10,
                                      time_limit = 60,
                                      save_gp_scoring = FALSE))

### #### Extract metrics and write to file  

collected_bayes_mets <- collect_metrics(lgbm_bayes_res, summarize = TRUE)

# write_csv(collected_bayes_mets,
#           here("output-data", "hyper-tuning", "bayes_tuning_hypers_summary.csv"))


### And finally rank things

final_ranks <- collected_bayes_mets %>%
  dplyr::group_by(.metric) %>%
  filter(.metric %in% c("mae", "rmse", "rsq", "rsq_trad")) %>%
  mutate(mean_rank = ifelse(.metric %in% c("rsq", "rsq_trad"),
                              dense_rank(desc(mean)),
                              dense_rank(mean))) %>%
  mutate(se_rank = dense_rank(std_err)) %>%
  mutate(overall_score  = 0.7*mean_rank + 0.3*se_rank) %>%
  mutate(overall_rank = dense_rank(overall_score)) %>%
  dplyr::ungroup()

### And extract the final hyperparameters
### And convert mtry to a proportion, which is needed

final_hypers <- final_ranks %>%
  dplyr::filter(overall_rank == 1) %>%
  dplyr::slice(1)

final_hypers <- final_hypers %>%
  mutate(mtry_prop = 3/
           lgbm_rec$var_info %>%
           filter(role == "predictor") %>%
           nrow()) %>%
  dplyr::select(!mtry) %>%
  relocate(mtry_prop, .before = "trees")

#write_csv(final_hypers, here("output-data", "final_hypers.csv"))


# **************************************************************

# //////////////////////////////////////////////////////////////////////////////


# **************************************************************

# Test models ----

# We now want to add back in our test and then use LOOCV 
# To fully test the model skill 

## Add 'em back in

full_dnbr <- bind_rows(train_valid, held_out_test)

rm(dnbr_and_drivers)

## Then run the models

### Double check that our testing fires 
### are dtill numbered 1-56 when added back into the big pot

test_fire_check <- sort(unique(full_dnbr %>%
  filter(fire_id %in% unique(held_out_test$fire_id)) %>%
    .$usu_fire_index)) == sort(unique(held_out_test$usu_fire_index))

length(test_fire_check[test_fire_check == FALSE]) == 0

#length(unique(test$usu_fire_index))

#sort(unique(test$usu_fire_index))) 

for(i in sort(unique(held_out_test$usu_fire_index))) {
  

  train <- full_dnbr %>%
    filter(usu_fire_index != i) %>%
    dplyr::select(!any_of(c("usu_fire_index",
                     "group_index")))
  
  test <- full_dnbr %>%
    filter(usu_fire_index == i) %>%
    dplyr::select(!any_of(c("usu_fire_index",
                     "group_index")))

  cat(crayon::red("\nTesting on fire #",i, test$fire_id[1], "\n"))
  
  
  lgbm_out_test <- run_lgbm(train_df = train, 
                       test_df = test, 
                       tgt_var = "dnbr", 
                       objective = "regression", 
                       application = "testing",
                       hyperparams = final_hypers, 
                       explain = TRUE,
                       plot_shaps = TRUE)
  
  ## Now extract some things of interest
  
  ## First, get predictions to calculate performance stats
  
  
  predicted_df <-  lgbm_out_test[[1]]
  
  perf_stats <- evaluate_preds(predicted_df,
                               tgt_var = "dnbr")
  
  ## Then, extract performance stats 
  
  ### For each feature instance
  preds_and_perf <- perf_stats[[1]]
  
  ### Overall by testing fire
  overall_stats <- perf_stats[[3]]
  
  ## Then get variable importance 
  
  var_imp <- lgbm_out_test[[2]] %>%
    as_tibble() %>%
    mutate(test_fire = test$fire_id[1])
  
  ## And various SHAP things
  
  shv_b <- lgbm_out_test[[3]] %>%
    mutate(test_fire = test$fire_id[1])
  
  shv_shap <- lgbm_out_test[[4]] %>%
    mutate(test_fire = test$fire_id[1])
  
  shv_feats <- lgbm_out_test[[5]] %>%
    mutate(test_fire = test$fire_id[1])
  
  
  ### And write them all to file
  
  write_csv(preds_and_perf, here("output-data",
                        "dnbr-predictions",
                        paste0("predicted_dnbr_",
                               test$fire_id[1],
                               ".csv")))
  
  write_csv(overall_stats, here("output-data",
                        "performance-stats",
                        paste0("perf_stats_",
                               test$fire_id[1],
                               ".csv")))
  
  write_csv(var_imp, here("output-data",
                                "feat-importance",
                                paste0("feat_imp_",
                                       test$fire_id[1],
                                       ".csv")))
  
  
  write_csv(shv_b, here("output-data",
                        "shap-stuff",
                        "fire-by-fire",
                        paste0("shap_baseline_",
                               test$fire_id[1],
                               ".csv")))
  
  write_csv(shv_shap, here("output-data",
                           "shap-stuff",
                           "fire-by-fire",
                           paste0("shap_values_",
                                  test$fire_id[1],
                                  ".csv")))
  
  write_csv(shv_feats, here("output-data",
                            "shap-stuff",
                            "fire-by-fire",
                            paste0("shap_feature_mags_",
                                   test$fire_id[1],
                                   ".csv")))
  
  
  ### Clean things up
  
  cat(crayon::yellow("\n Takin out deh traaaaaash\n"))
  
  gc()
  
  
}

# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# Some plotting stuff ----

## SHAPs----


#### Read .csvs from file
#### Bind together and convert 

new_sv <- shapviz::shapviz(shv_shap %>% as.matrix(), X = shv_feats)

#### 

shap_obj <- map(xgb_output, ~.[[4]])

all_shaps_m <- map_df(shap_obj, 
                      function(obj) shapviz::get_shap_values(obj) %>% 
                        as.data.frame) %>%
  as.matrix()

all_feats <- map_df(shap_obj, 
                    shapviz::get_feature_values)

new_sv <- shapviz::shapviz(all_shaps_m, X = all_feats)


p <- sv_importance(object = new_sv, kind = "beeswarm", 
                   viridis_args = list(option = "turbo",
                                       begin = 0, end = 1,
                                       direction = -1), 
                   max_display = 8,
                   number_size = 6,
                   show_numbers = TRUE) +
  scale_color_wa_c(palette = "puget") +
  theme_bw()

ggsave(here("plots/test_bee.png"), last_plot()) 

# **************************************************************

## Check train-test distribution

### Check

all_fire_metadata %>%
  ggplot() +
  geom_histogram(aes(x = acres_within_fire_perimeter, 
                     fill = fct_reorder(train_test, desc(train_test))),
                 color = "black",
                 bins = 10,
                 alpha = 0.3, position = "identity") +
  scale_fill_manual(values = c("black", "blue")) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  theme_bw()


# YOU CAN USE BINARY LOGISTIC FOR XGBOOST
# objective='binary:logistic
