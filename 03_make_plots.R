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

here::i_am("03_make_plots.R")

# **************************************************************

## Packages ----

### General 

require(here)

### Data mgmt & processing

require(tidyverse)

### File mgmt

require(usethis)
require(fs)

### Data viz
require(scales)
require(ggthemes)
require(fastshap)
require(shapviz)
require(wacolors)

### Maps
require(geodata)
require(terra)
require(rnaturalearthdata)
require(sf)

# **************************************************************

## Load custom functions ----

source(here("00_functions.R"))

# **************************************************************

## Set script-wide vars ----

### Map projection

map_crs <- 26913 # NAD83 / UTM zone 13N

map_proj <- "EPSG:26913"

### Some presentation formating for figure

pres_theme <- theme(plot.background = element_rect(fill = "transparent", color = NA),
                    legend.background = element_rect(fill = "transparent", color = NA),
                    legend.box.background = element_rect(fill = "transparent", color = NA),
                    strip.background = element_rect(fill = "transparent"),
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 22),
                    legend.text = element_text(size = 20),
                    legend.title = element_text(size = 20),
                    strip.text = element_text(size = 18))

# **************************************************************

# GET DATA ----

## Train-Valid output data ----

dnbr_pred_obs_tv <- map_dfr(dir_ls(here("output-data", "train-valid"), 
                                   regexp = "dnbr_preds.*\\.csv$"),
                           read_csv)


shap_vals_tv <- map_dfr(dir_ls(here("output-data", "train-valid"), 
                               regexp = "shap_values.*\\.csv$")
                        , function(f) {
                          
                          split_name <- str_extract(f, "(?<=tv_)split\\d+(?=\\.csv$)")
                          
                          read_csv(f) %>%
                            mutate(split = split_name)
                          
                          })




shap_baseline_tv <- map_dfr(dir_ls(here("output-data", "train-valid"), 
                                   regexp = "shap_baseline.*\\.csv$")
                            , function(f) {
                              
                              split_name <- str_extract(f, "(?<=tv_)split\\d+(?=\\.csv$)")
                              
                              read_csv(f) %>%
                                mutate(split = split_name)
                              
                            })



shap_feats_tv <- map_dfr(dir_ls(here("output-data", "train-valid"), 
                                regexp = "shap_feature_mags.*\\.csv$")
                         , function(f) {
                           
                           split_name <- str_extract(f, "(?<=tv_)split\\d+(?=\\.csv$)")
                           
                           read_csv(f) %>%
                             mutate(split = split_name)
                           
                         })



sv_tv <- shapviz::shapviz(shap_vals_tv %>%
                             dplyr::select(!c(test_fire_id,
                                              split)) %>% 
                             as.matrix(), 
                           X = shap_feats_tv %>%
                             dplyr::select(!c(test_fire_id,
                                              split)))

# **********************************

## Testing output data ----

### Predictions and Observations data

pred_files <- dir_ls(here("output-data", "dnbr-predictions"), glob = "*.csv")

dnbr_pred_obs <- map_dfr(pred_files, read_fire_files)

### Performance stats by fire

performance_files <- dir_ls(here("output-data", "performance-stats"), 
                            glob = "*.csv")


performance <- map_dfr(performance_files, function(f) {
  fire_id <- str_extract(f, "(?<=_)[^_]+(?=\\.csv$)")
  
  read_csv(f) %>%
    mutate(fire_id = fire_id)
})

### Shap values

shap_vals_files <- dir_ls(here("output-data", 
                               "shap-stuff", 
                               "fire-by-fire"), 
                          regexp = "values.*\\.csv$")

shap_vals_all <- map_dfr(shap_vals_files, read_csv)

shap_baseline <- dir_ls(here("output-data", 
                             "shap-stuff", 
                             "fire-by-fire"), 
                        regexp = "baseline.*\\.csv$")


shap_baseline_all <- map_dfr(shap_baseline, read_csv)

shap_feats_files <- dir_ls(here("output-data", 
                                "shap-stuff", 
                                "fire-by-fire"), 
                           regexp = "feature_mags.*\\.csv$")

shap_feats_all <- map_dfr(shap_feats_files, read_csv)



# **********************************


# **************************************************************

# CALCULATE SUMMARIES ----

# **********************************

## Train-Valid ----

fire_stats <- dnbr_pred_obs_tv %>%
  dplyr::group_by(fire_id) %>%
  summarise(mean_observed_dnbr = mean(observed_dnbr),
            median_observed_dnbr = median(observed_dnbr),
            max_observed_dnbr = max(observed_dnbr),
            sd_observed_dnbr = sd(observed_dnbr)) %>%
  dplyr::ungroup()

fire_feature_stats <- dnbr_pred_obs_tv %>%
  dplyr::group_by(fire_id) %>%
  dplyr::select(!contains(c("err", "error"))) %>%
  dplyr::select(!c("observed_dnbr", "predicted_dnbr")) %>%
  summarise(across(where(is.numeric), ~mean(abs(.x)))) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = -fire_id,
               values_to = "mean_feature_value",
               names_to = "feature")

### Plot some feature distributions 
fire_feature_stats %>%
  filter(feature == "erc") %>%
  ggplot() +
    geom_histogram(aes(x = mean_feature_value),
                   color = "black",
                   fill = "darkred",
                   alpha = 0.7,
                   binwidth = 1,
                   #bins = 3
                   ) +
    theme_bw()
  

# **********************************

## Testing ----


### Calculate some summary stats


fire_stats <- dnbr_pred_obs %>%
  dplyr::group_by(fire_id) %>%
  summarise(mean_observed_dnbr = mean(observed_dnbr),
            median_observed_dnbr = median(observed_dnbr),
            max_observed_dnbr = max(observed_dnbr),
            sd_observed_dnbr = sd(observed_dnbr)) %>%
  dplyr::ungroup()

fire_feature_stats <- dnbr_pred_obs %>%
  dplyr::group_by(fire_id) %>%
  dplyr::select(!contains(c("err", "error"))) %>%
  dplyr::select(!c("observed_dnbr", "predicted_dnbr")) %>%
  summarise(across(where(is.numeric), ~mean(abs(.x)))) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = -fire_id,
               values_to = "mean_feature_value",
               names_to = "feature")


  
  

### And make a model with these stats?

#### Fire-by-fire

error_mod_df <- inner_join(fire_feature_stats %>%
  pivot_wider(names_from = feature,
              values_from = mean_feature_value),
  perf_by_fire,
  join_by(fire_id == test_fire_id)) %>%
  inner_join(., cor_coeff_by_fire) %>%
  dplyr::select(any_of(fire_feature_stats$feature), r) %>%
  mutate(across(any_of(c("evh", "evt", "evc", "fbfm40", "fccs")), 
                ~round(., 0))) %>%
  mutate(across(any_of(c("evh", "evt", "evc", "fbfm40", "fccs")), 
                ~as.factor(.))) %>%
  mutate(across(any_of(c("cbd", "cbh", "cc", "ch")), 
                ~as.integer(.))) 

err_mod <- lm(r ~ ., data = error_mod_df,
              na.action = na.fail)

err_mod_me <- lmer(r ~ ., data = error_mod_df)

summary(err_mod)

MuMIn::dredge(err_mod)

err_mod2 <- lm(r ~ cbh + fccs + hli, data = error_mod_df,
               na.action = na.fail)

summary(err_mod2)


error_mod_df %>%
  ggplot() +
    geom_point(aes(x = hli, y = r)) +
    geom_smooth(aes(x = hli, y = r),
                method = "lm",
                formula = y~x,
                color = "darkred",
                fill = "gray80",
                alpha = 0.3) + 
    #scale_x_log10() + 
    theme_bw()

##

dnbr_pred_obs_trim <- dnbr_pred_obs_tv %>%
  dplyr::ungroup() %>%
  mutate(r = cor(predicted_dnbr, observed_dnbr)) %>%
  dplyr::select((where(is.numeric))) %>%
  dplyr::select(!c(predicted_dnbr, observed_dnbr)) %>%
  dplyr::select(!c(
    #raw_err,
    sqrerr,
    abs_error,
    abs_pct_error,
    r
  ))

big_mod <- lm(raw_err ~ ., dnbr_pred_obs_trim)

summary(big_mod)

big_mod2 <- lm(r ~ hsp+evt+evc+cc+cbh, dnbr_pred_obs_trim)

summary(big_mod2)

big_mod3 <- lm(r ~ hsp+evc+cbh, dnbr_pred_obs_trim)

summary(big_mod3)

dnbr_pred_obs_trim %>%
  ggplot() +
    geom_histogram(aes(x = cbh),
                   fill = "lightgoldenrod3", 
                   color = "black",
                   bins = 50) +
    theme_bw()



# **********************************


# MAKE PLOTS ----

## Pred vs Obs ----

#### Binned average predictions vs observations

binned_calib <- dnbr_pred_obs_tv %>%
  mutate(bin = ntile(predicted_dnbr, 100)) %>%
  dplyr::group_by(bin) %>%
  summarise(mean_pred = mean(predicted_dnbr),
            mean_obs = mean(observed_dnbr)) %>%
  ggplot() +
  geom_point(aes(x =mean_pred , y = mean_obs),
             color = "tomato4",
             fill = "tomato3",
             size = 2, shape = 21,
             alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  scale_x_continuous(limits = c(0,750)) +
  scale_y_continuous(limits = c(0,750)) + 
  coord_equal(ratio = 1) + 
  labs(x = "Predicted dNBR",
       y = "Observed dNBR") + 
  theme_bw()

ggsave("plots/train-valid-explore/binned_calib_quant_regr.png", binned_calib,
       height = 6, width = 6, dpi = 300)

#### Histogram observed vs predicted

pred_obs_hist <- dnbr_pred_obs_tv %>%
  dplyr::select(fire_id, x, y, observed_dnbr, predicted_dnbr) %>%
  pivot_longer(cols = -c(fire_id, x, y), 
               names_to = "type", values_to = "dnbr") %>%
  mutate(type = case_when(type == "observed_dnbr" ~ "Observed",
                          type == "predicted_dnbr" ~ "Predicted")) %>%
  ggplot() +
    geom_histogram(aes(x = dnbr,
                       y = after_stat(count / sum(count)),
                       fill = type,
                       alpha = type),
                   bins = 100,
                   position = "identity",
                   color = "black",
                   #alpha = 0.5
                   ) +
    scale_fill_manual(values = c("tomato4", "tomato2"),
                       name = "") +
    scale_alpha_manual(values = c(0.9, 0.7),
                    name = "") +
    labs(x = "dNBR", y = "Frac.") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 22),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20))

pred_obs_hist

ggsave("plots/train-valid-explore/pred_vs_obs_hist_quant_regr.png",
       last_plot(),
       height = 3.5, width = 6.5, dpi = 300)



dnbr_pred_obs %>%
dplyr::select(fire_id, x, y, observed_dnbr, predicted_dnbr) %>%
  ggplot() +
  geom_histogram(aes(x = observed_dnbr,
                     y = after_stat(count / sum(count))),
                 fill = "tomato4",
                 color = "black",
                 bins = 100,
                 position = "identity",
                 alpha = 0.9
                 )+ 
  geom_histogram(aes(x = predicted_dnbr,
                     y = after_stat(count / sum(count))),
                 fill = "tomato2",
                 color = "black",
                 bins = 100,
                 position = "identity",
                 alpha = 0.7
  )+ 
  theme_bw()

#### Pred vs Obs Scatterplot

pred_mod <- lm(observed_dnbr ~ predicted_dnbr, data = dnbr_pred_obs)

r2 <- summary(pred_mod)$r.squared

slope <- summary(pred_mod)$coefficients[[2]]

samp_size <- format(nrow(dnbr_pred_obs), big.mark = ",")

pred_p <- dnbr_pred_obs %>%
  #dplyr::slice(1:1000) %>%
  ggplot() +
  geom_point(aes(x =predicted_dnbr , y =observed_dnbr ),
             color = "tomato3", shape = 1) +
  geom_smooth(aes(x = predicted_dnbr, y = observed_dnbr),
              formula = y~x,
              method = "lm",
              color = "tomato4") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "black") +
  annotate(geom = "text", 
           x = 1200, y = -400, 
           label = paste0("R^2 == ", round(r2,2)),
           color = "tomato3",
           parse = TRUE) + 
  annotate(geom = "text", 
           x = 1200, y = -500, 
           label = paste0("m = ", round(slope,1)),
           color = "tomato3",
           parse = FALSE) + 
  annotate(geom = "text", 
           x = -300, y = 1300, 
           label = paste0("n = ", samp_size),
           color = "tomato3",
           fontface = "italic",
           parse = FALSE) + 
  scale_x_continuous(limits = c(-500, 1300)) +
  scale_y_continuous(limits = c(-500, 1300)) + 
  coord_equal(ratio = 1) + 
  labs(x = "Predicted dNBR",
       y = "Observed dNBR") + 
  theme_bw()

pred_p

ggsave("plots/preds_vs_obs.png", pred_p,
       height = 6, width = 6, dpi = 300)

#### Pred vs Obs Heatmap

pred_p2 <- dnbr_pred_obs %>%
  #dplyr::slice(1:1000) %>%
  ggplot() +
  geom_bin2d(aes(x =predicted_dnbr , y = observed_dnbr),
            color = NA,
             binwidth = 10) +
  geom_smooth(aes(x = predicted_dnbr, y = observed_dnbr),
              formula = y~x,
              method = "lm",
              color = "gray50",
              linewidth = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray50") +
  annotate(geom = "text", 
           x = 1100, y = -250, 
           label = paste0("R^2 == ", round(r2,2)),
           color = "gray50",
           size = 6,
           parse = TRUE) + 
  annotate(geom = "text", 
           x = 1100, y = -400, 
           label = paste0("m = ", round(slope,1)),
           color = "gray50",
           size = 6,
           parse = FALSE) + 
  annotate(geom = "text", 
           x = -300, y = 1300, 
           label = paste0("n = ", samp_size),
           color = "gray50",
           fontface = "italic",
           size = 4,
           parse = FALSE) + 
  scale_fill_viridis_c(option = "turbo",
                       #breaks = trans_breaks("log10", function(x) 10^x),
                       #labels = trans_format("log10", math_format(10^.x)),  
                       guide = guide_colorbar(barheight = unit(0.6, "cm"),
                                              barwidth  = unit(10, "cm")),
                       name = "Count") + 
  scale_x_continuous(limits = c(-500, 1300)) +
  scale_y_continuous(limits = c(-500, 1300)) + 
  coord_equal(ratio = 1) + 
  labs(x = "Predicted dNBR",
       y = "Observed dNBR") + 
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "gray90")) + 
  pres_theme

pred_p2


ggsave("plots/preds_vs_obs_heatmap_prez.png", pred_p2,
       height = 6, width = 6, dpi = 300)

### Resids heatmap

pred_p3 <- dnbr_pred_obs %>%
  #dplyr::slice(1:10000) %>%
  ggplot() +
  geom_bin2d(aes(x = predicted_dnbr, y = raw_err),
             color = NA,
             binwidth = 10) +
  geom_smooth(aes(x = predicted_dnbr, y =  raw_err),
              formula = y~x,
              method = "lm",
              color = "gray70",
              linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
              color = "gray70") +
  # annotate(geom = "text", 
  #          x = 1200, y = -400, 
  #          label = paste0("R^2 == ", round(r2,2)),
  #          color = "gray70",
  #          parse = TRUE) + 
  # annotate(geom = "text", 
  #          x = 1200, y = -500, 
  #          label = paste0("m = ", round(slope,1)),
  #          color = "gray70",
  #          parse = FALSE) + 
  scale_fill_viridis_c(option = "turbo",
                       guide = guide_colorbar(barheight = unit(0.6, "cm"),
                                              barwidth  = unit(10, "cm")),
                       name = "density") + 
  scale_x_continuous(limits = c(-500, 1300)) +
  #scale_y_continuous(limits = c(-500, 1300)) + 
  coord_equal(ratio = 1) + 
  labs(x = "Predicted dNBR",
       y = "Resids (Pred – Obs)") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  pres_theme

pred_p3

ggsave("plots/resids_heatmap.png", pred_p3,
       height = 6, width = 7.5, dpi = 300)

### Some other stuff

dnbr_pred_obs %>%
  ggplot() +
  geom_bin2d(aes(x = erc, y = raw_err),
             color = NA,
             bins = 20) +
  geom_smooth(aes(x = erc, y =  raw_err),
              formula = y~x,
              method = "lm",
              color = "gray70",
              linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray70") +
  scale_fill_viridis_c(option = "turbo",
                       guide = guide_colorbar(barheight = unit(0.6, "cm"),
                                              barwidth  = unit(10, "cm")),
                       name = "density") + 
  #scale_x_continuous(limits = c(-500, 1300)) +
  #scale_y_continuous(limits = c(-500, 1300)) + 
  #coord_equal(ratio = 1) + 
  labs(x = "Predicted dNBR",
       y = "Resids (Pred – Obs)") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  pres_theme




# **************************************************************

## Check error drivers ----

n_vars <- dnbr_pred_obs %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!contains("err")) %>%
  names()

lm_list <- list()

for(i in 1:length(n_vars)){
  
  cat(crayon::cyan("\n Calculating relationship with var #", i,
                   n_vars[i], "\n"))
  
  
  mod <- lm(reformulate(n_vars[i], response = "abs_error"),
            data = dnbr_pred_obs)
  
  s_m <- summary(mod)
  
  rr <- s_m$r.squared
  
  lm_list[[i]] <- tibble(var = n_vars[i],
                         r2 = rr)
  
  
}

all_var_rels <- bind_rows(lm_list)






# **************************************************************

## Performance Stats ----



stats_tv <- evaluate_preds(preds_df = dnbr_pred_obs_tv)

perf_by_fire <- stats_tv[[2]] %>%
  dplyr::select(!mean_predicted_dnbr)

stats_tv[[3]]

stats_tv[[2]] %>%
  summarise(across(where(is.numeric), median))
  
cor(dnbr_pred_obs_tv$predicted_dnbr, dnbr_pred_obs_tv$observed_dnbr)

cor_coeff_by_fire <- dnbr_pred_obs_tv %>%
  dplyr::group_by(fire_id) %>%
  summarise(mean_predicted_dnbr = mean(predicted_dnbr),
            mean_observed_dnbr = mean(observed_dnbr),
            r = cor(predicted_dnbr, observed_dnbr))

cor_coeff_by_fire %>%
  summarise(median_r = median(r),
            mean_r = mean(r))

lmd <- lm(mean_obs ~ mean_pred, data = cor_coeff_by_fire)

lmd2 <- lm(observed_dnbr ~ predicted_dnbr, data = dnbr_pred_obs_tv)

summary(lmd)

summary(lmd2)
### Summary table


### Add in mean feature values
### And plot by feature values

inner_join(fire_feature_stats, perf_by_fire,
           join_by(fire_id == test_fire_id)) %>%
  inner_join(., cor_coeff_by_fire,
             join_by(fire_id == fire_id)) #%>%
  filter(feature == "y") %>%
  ggplot() +
  geom_point(aes(x = mean_feature_value , y = r , 
                 fill = mean_predicted_dnbr,
                 size = mae),
             #color = "#d73027",
             shape = 21) +
  geom_smooth(aes(x = mean_feature_value , y = r),
              color = "black",
              method = "lm",
              formula = y~x,
              fill = "gray70",
              alpha = 0.3) +
  # scale_fill_distiller(palette = "PuRd", 
  #                      direction = 1,
  #                      transform = "log10",
  #                      labels = scales::label_number(big.mark = ","),
  #                      name = "Fire Size (acres)") +
  cols4all::scale_fill_continuous_c4a_seq(palette = "-kovesi.bk_rd_yl",
                                          name = "Predicted dNBR") + 
  # scale_size_continuous(transform = "log10",
  #                       labels = scales::label_number(big.mark = ","),
  #                       name = "Fire Size (acres)") + 
  #scale_x_log10() + 
  labs(x = "Lat.",
       y = "r") + 
  theme_bw() +
  theme(legend.position = "right") +
  pres_theme

ggsave("plots/perf_by_npp.png",
       last_plot(), height = 6, width = 6, dpi= 300
)


### Plots

perf_boxes <- performance %>%
  dplyr::select(!mean_predicted_dnbr) %>%
  pivot_longer(everything(),
               names_to = "metric",
               values_to = "score") %>%
  filter(!(metric == "nse" & score <= -1)) %>%
  mutate(metric = case_when(metric == "mae" ~ "Mean Abs. Error (dNBR)",
                            metric == "mape" ~ "Mean Abs. Pct. Error (%)",
                            metric == "nse" ~ "Nash-Sutcliffe Eff.",
                            metric == "pbias" ~ "% Bias",
                            metric == "rmse" ~ "Root Mean Sq. Error")) %>%
  #filter(metric == "nse") %>%
  ggplot() +
  geom_violin(aes(x = metric, y = score, fill = metric)) +
  geom_boxplot(aes(x = metric, y = score), 
               color = "gray90", alpha = 0, width = 0.3,
               outlier.shape = 1, outlier.size = 1) +
  stat_summary(aes(x = metric, y = score),
               fun = mean,
               geom = "point",
               shape = 21,
               fill = "gray90",
               size = 2) + 
  scale_fill_brewer(palette = "Dark2",
                    guide = "none") +
  #scale_y_continuous(limits = c(-1,1)) + 
  labs(x = element_blank(),
       y = element_blank()) + 
  theme_bw() +
  facet_wrap(~metric, scales = "free",
             strip.position = "left",
             nrow = 2) +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA,
                                        color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_blank()) +
  pres_theme

perf_boxes

ggsave("plots/performance_stats_boxplots.png",
       perf_boxes, height = 6, width = 9
       )

### Overall

overall_stats <- dnbr_pred_obs %>%
  summarise(mae = mean(abs_error),
            mape = mean(is.finite(abs_pct_error)),
            rmse = sqrt(mean(sqrerr)),
            nse = hydroGOF::NSE(predicted_dnbr, 
                                observed_dnbr),
            pbias = hydroGOF::pbias(predicted_dnbr,
                                    observed_dnbr)) %>%
  pivot_longer(everything(), 
               values_to = "value",
               names_to = "metric") %>%
  mutate(metric = case_when(metric == "mae" ~ "Mean Abs. Error (dNBR)",
                            metric == "mape" ~ "Mean Abs. Pct. Error (%)",
                            metric == "nse" ~ "Nash-Sutcliffe Eff.",
                            metric == "pbias" ~ "% Bias",
                            metric == "rmse" ~ "Root Mean Sq. Error")) %>%
  pivot_wider(names_from = metric, values_from = value)
  

  overall_stats
  
  write_csv(overall_stats, "plots/overall_perf_stats.csv")
  
  clr <- hcl.colors(palette = "PuRd", n = 9)
  
  fire_bounds_exp %>%
    as_tibble() %>%
    ggplot() +
      geom_point(aes(x = mean_observed_dnbr, y = mae, 
                     fill = acres_within_fire_perimeter,
                     size = acres_within_fire_perimeter),
                 #color = "#d73027",
                 shape = 21) +
      geom_smooth(aes(x = mean_observed_dnbr, y = mae),
                  color = "black",
                  method = "lm",
                  formula = y~x,
                  fill = "gray70",
                  alpha = 0.3) +
      scale_fill_distiller(palette = "PuRd", 
                           direction = 1,
                           transform = "log10",
                           labels = scales::label_number(big.mark = ","),
                           name = "Fire Size (acres)") +
      scale_size_continuous(transform = "log10",
                            labels = scales::label_number(big.mark = ","),
                            name = "Fire Size (acres)") + 
      labs(x = "Mean Obs. dNBR",
           y = "Mean Abs. Error (dNBR)") + 
      theme_bw() +
      theme(legend.position = "right") +
      pres_theme
  
mm <- lm(pbias ~ acres_within_fire_perimeter, data = fire_bounds_exp %>% as_tibble())

summary(mm)

ggsave("plots/mae_by_dnbr2.png",
       last_plot(), height = 6, width = 9,
       dpi = 300,
)


# **************************************************************

## Variable Importance ----

var_imp_files <- dir_ls(here("output-data", "feat-importance"), glob = "*.csv")

var_imp_all <- map_dfr(var_imp_files, read_csv)




var_imp_all <- var_imp_all %>%
  dplyr::group_by(Feature) %>%
  mutate(mean_Gain = mean(Gain)) %>%
  dplyr::ungroup() %>%
  mutate(Feature = fct_reorder(Feature, mean_Gain))

var_imp_p <- var_imp_all %>%
  ggplot() +
  # geom_violin(aes(x = Gain, y = Feature, fill = Feature),
  #             width = 3, linewidth = 0.2,
  #             color = "gray50") +
  geom_boxplot(aes(x = Gain, y = Feature,
                   fill = Feature),
               color = "black",
               alpha = 0.9, width = 0.8,
               linewidth = 0.2,
               outlier.shape = 1, outlier.size = 1) +
  geom_violin(aes(x = Gain, y = Feature),
              width = 3, linewidth = 0.3,
              alpha = 0,
              color = "gray60",
              fill = "gray80") +
  scale_color_manual(values = as.vector(pals::kelly()),
                     name = element_blank(),
                     guide = "none") +
  scale_fill_manual(values = as.vector(pals::kelly()),
                    name = element_blank(),
                    guide = "none") +
  #scale_x_log10() + 
  theme_bw() +
  pres_theme

var_imp_p

ggsave("plots/var_imp_violin_plots_prez.png",
       var_imp_p, height = 8, width = 8,
       dpi = 300,
)

# **************************************************************

## SHAP values ----

### Calculate a median across all LOOCV runs
shap_vals_sum <- shap_vals_tv %>%
  dplyr::select(!test_fire_id) %>%
  group_by(split) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean_raw_shap = ~mean(.x, na.rm = TRUE),
        mean_abs_shap = ~mean(abs(.x), na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(
    -split,
    names_to = c("feature", ".value"),
    names_sep = "__"
  ) %>%
  group_by(feature) %>%
  mutate(
    median_mean_raw = median(mean_raw_shap, na.rm = TRUE),
    median_mean_abs = median(mean_abs_shap, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  mutate(feature = fct_reorder(feature, median_mean_abs))

shap_feats_sum <- shap_feats_all %>%
  #dplyr::group_by(test_fire) %>%
  summarise(across(where(is.numeric), ~mean(abs(.x)))) %>%
  dplyr::ungroup()


### Plot

shap_imp_p <- shap_vals_sum %>%
  ggplot() +
  geom_boxplot(aes(x = mean_abs_shap, y = feature,
                   fill = feature),
               color = "black",
               alpha = 0.9, width = 0.8,
               linewidth = 0.2,
               outlier.shape = 1, outlier.size = 1) +
  geom_violin(aes(x = mean_abs_shap, y = feature),
              width = 1.4, linewidth = 0.3,
              alpha = 0,
              color = "gray60",
              fill = "gray80") +
  scale_color_manual(values = as.vector(pals::kelly()),
                     name = element_blank(),
                     guide = "none") +
  scale_fill_manual(values = as.vector(pals::kelly()),
                    name = element_blank(),
                    guide = "none") +
  labs(x = "Mean Abs. SHAP",
       y = "Feature") + 
  theme_bw() +
  pres_theme

shap_imp_p

ggsave("plots/train-valid-explore/shap_imp_prez.png",
       shap_imp_p, height = 8, width = 8,
       dpi = 300,
)


### Make a new beeswarm plot


shap_bee_new <- sv_importance(new_sv, 
                          kind = "beeswarm",
                          show_numbers = TRUE,
                          max_display = 10,
                          number_size = 6) +
  theme_bw() +
  cols4all::scale_colour_continuous_c4a_seq(palette = "-kovesi.bk_rd_yl") + 
  guides(color = guide_colorbar(direction = "horizontal",
                                barwidth = 12,
                                barheight = 0.7,
                                title.position = "left",
                                title.theme = element_text(margin = margin(r = 24)))) + 
  theme(
    legend.position = "bottom",
    plot.background = element_blank(),
    legend.background = element_blank(), #transparent legend bg
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 22),
  ) 

ggsave(here("plots", 
            "shap-plots",
            "combined-beeswarm.png"),
       shap_bee_new,
       width = 8.5,
       height = 7.5, 
       dpi = 300)

### SHAP rank by test fire 

feature_ranks_by_fire <- shap_vals_sum %>%
  dplyr::select(!c(median_mean_raw, median_mean_abs)) %>%
  dplyr::group_by(test_fire) %>%
  mutate(feature_rank = dense_rank(desc(mean_abs_shap)))

### Ranks and stats by fire

feat_ranks_and_vals_by_fire <- inner_join(feature_ranks_by_fire %>%
             rename(fire_id = test_fire), 
           all_fire_metadata,
           by = "fire_id") %>%
  inner_join(., fire_stats) %>%
  filter(train_test == "test") %>%
  mutate(start_j_date = yday(ignition_date)) %>%
  inner_join(., fire_feature_stats,
             by = c("feature", "fire_id"))
  


feat_ranks_and_vals_by_fire %>%
  #filter(feature %in% c("erc", "npp", "x", "y", "hsp", "evc")) %>%
  filter(feature %in% c("erc","npp")) %>%
  ggplot() +
    geom_point(aes(x = mean_feature_value, y = mean_raw_shap,
                   color = feature
                   )
               ) +
    geom_smooth(aes(x = mean_feature_value, y = mean_raw_shap,
                 color = feature
                 ),
                alpha = 0.2,
                fill = "gray75",
                method = "lm",
                formula = y~x) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2",
                       guide = "none") +
    # scale_color_manual(values = as.vector(pals::kelly()),
    #                  name = element_blank(),
    #                  guide = "none") +
    facet_wrap(~feature, scales = "free") +
    labs(y = "Mean SHAP Value", x = "Mean Feature Value") + 
    #scale_y_log10() + 
    pres_theme

feat_ranks_and_vals_by_fire %>%
  filter(feature %in% c("x","y" , "npp")) %>%
  dplyr::select(fire_id, mean_observed_dnbr, 
                feature, feature_rank, 
                mean_abs_shap, mean_raw_shap,
                mean_feature_value) %>%
  pivot_wider(names_from = feature,
              values_from = feature_rank:mean_feature_value) %>%
  ggplot() +
    geom_point(aes(x =mean_feature_value_x, y = feature_rank_npp ,
                   fill = mean_raw_shap_npp),
               shape = 21, size = 3) + 
    cols4all::scale_fill_continuous_c4a_seq(palette = "-kovesi.bk_rd_yl",
                                              name = "Mean Abs.\nShap (Long.)") +
    guides(fill = guide_colorbar(direction = "horizontal",
                                  barwidth = 16,
                                  barheight = 0.7,
                                  title.position = "left",
                                  title.theme = element_text(margin = margin(r = 24)))) + 
    labs(x = "Feature Rank Long.", y = "Latitude (UTM)") + 
    theme_bw() +
    theme(legend.position = "bottom") +
    pres_theme
    

ggsave("plots/feat_rank_long_by_lat_value.png",
       last_plot(),
       height = 6, width = 6, dpi = 300)

# **************************************************************


## Fire outlines ----

### Get fire boundaries & metadata

forested_fires_files <- read_csv(here("output-data", 
                                      "forested_fires_files.csv"))

forested_fires_files <- as_fs_path(forested_fires_files$file_path)

trimmed_forested_fires <- unique(dnbr_pred_obs$fire_id)

trimmed_forested_fires_files <- forested_fires_files[str_detect(forested_fires_files, 
                                                                str_c(trimmed_forested_fires, 
                                                                      collapse = "|"))]


fire_bounds <- map(trimmed_forested_fires_files,
              get_fire_boundry,
              map_crs)

fire_bounds <- list_rbind(fire_bounds) %>%
  mutate(Event_ID = tolower(Event_ID)) %>%
  rename_all(tolower)

fire_meta <- read_csv(here("output-data", "all_fires_metadata.csv")) %>%
  mutate(log_acres = log10(acres_within_fire_perimeter))

fire_bounds_exp <- inner_join(fire_bounds, performance,
                              join_by(event_id == fire_id)) %>%
  inner_join(., fire_meta,
             join_by(event_id == fire_id)) %>%
  inner_join(., fire_stats,
             join_by(event_id == fire_id)) %>%
  st_as_sf()

fire_bounds_exp <- vect(fire_bounds_exp)

# **************************************************************

### Get state outline & Hillshade


co <- rnaturalearthdata::states50 %>%
  filter(iso_a2 == "US") %>%
  filter(name %in% c("Colorado")) 


### Get hillshade

# co_dem <- elevation_30s(country = "USA", path = tempdir())
# 
# co_dem <- project(co_dem, map_pr)

co_dem <- elevatr::get_elev_raster(co, 
                                   z = 8)

co_dem <- crop(co_dem, co, mask = TRUE, snap = "near", touches = TRUE)

co_dem <- rast(co_dem)

co_slope <- terra::terrain(co_dem, "slope", unit = "radians")
co_aspect <- terra::terrain(co_dem, "aspect", unit = "radians")

co_hs <- terra::shade(co_slope, co_aspect, 45, 315, normalize = TRUE)

co_hs <- project(co_hs, map_proj)

plot(co_hs)

co <- vect(st_transform(co, map_crs))

plot(co)
# **************************************************************

### And pop centers

den <- usmap::citypop %>%
  dplyr::filter(state %in% c("Colorado")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(map_crs)

# **************************************************************

### Now plot

#### Set bbox


bb <- st_bbox(st_union(st_as_sfc(st_bbox(fire_bounds_exp)),
      st_as_sfc(st_bbox(co)),
      st_as_sfc(st_bbox(co_hs)),
      st_as_sfc(st_bbox(den))))

#### And map palette 

pal <- hcl.colors(palette = "RdYlBu", n = 11, rev = TRUE)

wacolors::scale_colour_wa_c(palette = "puget")

#gray <- grey(seq(1,100,10)/100)

gray <- gray.colors(100, start = 1, end = 0)

tmap::tm_shape(co_hs,
               bbox = bb,
               crs = map_crs) +
  tmap::tm_raster("hillshade",
                  tm_scale_continuous(values = "brewer.greys"),
                  col.legend = tm_legend_hide()) +
  tm_grid(crs = map_crs,
          labels.format = tm_label_format(scientific = TRUE),
          #labels.inside_frame = TRUE,
          col = "black") 
  # tm_graticules(labels.pos = c("right", "top"),
  #               labels.format = list(big.mark = ""),
  #               col = "black",
  #               crs = map_crs)

t <- st_bbox(co_hs)$xmin


as.character(round(t, 0))
#### Now map

site_map <- tmap::tm_shape(co_hs,
                           bbox = bb,
                           crs = map_crs) +
      tmap::tm_raster("hillshade",
                      tm_scale_continuous(values = "brewer.greys"),
                      col.legend = tm_legend_hide()) +
      tm_grid(labels.pos = c("left", "top"),
                    col = "black",
              crs = map_crs) +
  tmap::tm_shape(co,
                 crs = map_crs) +
      tm_polygons(col = "black",
                  fill = "gray80",
                  fill_alpha = 0.5,
                  lwd = 3) +
      tm_text("name_tr", 
              size = 2,
              col = "gray40",
              fontface = "italic") +
  tm_shape(den,
           crs = map_crs) +
      tm_symbols(size = 0.4,
                 fill = "black",
                 shape = 22) +
      tm_text("most_populous_city", size = 0.8,
              xmod = 1.8,
              col = "black",
              fontface = "italic",
              ymod = 0.1) +
  tmap::tm_shape(fire_bounds_exp,
                 crs = map_crs) +
      # tm_polygons(fill = "mae",
      #            col = "gray20",
      #            lwd = 0.1,
      #            fill.scale = tm_scale_continuous(values = pal)) +
      tm_symbols(fill = "mean_observed_dnbr",
                 #fill.scale = tm_scale_continuous(values = pal),
                 fill.scale = tm_scale_continuous(values = "-kovesi.bk_rd_yl",
                                                  ticks = seq(0,725, 150)),
                 fill.legend = tm_legend(title = "Mean Obs. dNBR",
                                         frame = FALSE,
                                         orientation = "landscape",
                                         group_id = "B",
                                         #position = tm_pos_out(),
                                         text.size = 1.4,
                                         title.size = 1.6,
                                         title.align = c("center", "bottom"),
                                         title.padding = c(2,0,2,0),
                                         width = 14,
                                         height = 2.5,
                                         item_text.margin = 2.5
                 ),
                 # fill.chart = tm_chart_histogram(
                 #   plot.axis.x = TRUE,
                 #   plot.axis.y = TRUE,
                 #   group_id = "B",
                 #   frame = FALSE,
                 #   extra.ggplot2 = list(labs(title = "Mean Abs. Error",
                 #                             y = "Count"))
                 #                                 ),
                 size = "acres_within_fire_perimeter",
                 size.scale = tm_scale_continuous_pseudo_log(base = exp(10),
                                                             limits = c(500, 210000),
                                                             values.scale = 1),
                 size.legend = tm_legend(title = "Fire Size\n(Acres)",
                                         frame = FALSE,
                                         orientation = "portrait",
                                         group_id = "A",
                                         #position = tm_pos_out(),
                                         text.size = 1.4,
                                         title.size = 1.6,
                                         #title.padding = c(1,0,2,0),
                                         item.space = 2,
                                         height = 10
                                         ),
                 # size = "mean_observed_dnbr",
                 # size.scale = tm_scale_continuous(values = seq(1.5,7.5,2),
                 #                                  values.scale = 1.5),
                 # size.legend = tm_legend(title = "Mean Obs. dNBR",
                 #                         frame = FALSE,
                 #                         orientation = "portrait",
                 #                         group_id = "A",
                 #                         #position = tm_pos_out()
                 #                         ),
                 shape = 21,
                 col = "gray20",) +
  tm_layout(bg.color = "white",
            bg = TRUE,
            outer.bg.color = "transparent",  
            #legend.outside = TRUE,
            #legend.outside.position = c("center", "bottom"),
            #legend.bg = FALSE,
            #legend.bg.alpha = 0,
            #legend.text.size = 0.5,
            #legend.title.size = 1,
            #legend.outside.size = 0.01,
            inner.margins = c(0.03, 0.03, 0.03, 0.03),
            #bg.color = "transparent",
            frame = TRUE,
            legend.bg.color = "transparent",
            legend.bg.alpha = 0,
            # legend.text.size = 1.8,
            # legend.title.size = 1.8,
            #attr.text.size = 1.4,
            #attr.title.size = 1.4,
            
            ) +
  tm_components(group_id = "A", 
                stack = "vertical",
                position = tm_pos_out("right", "center")) + 
  tm_components(group_id = "B", 
                position = tm_pos_out("center", "bottom", 
                                      pos.h = "right", 
                                      pos.v = "bottom",
                                      ),
                offset = 2,
                frame = FALSE,
                frame_combine = FALSE) +
  tm_credits("n = 56",
             # position = tm_pos_out("right", "bottom", 
             #                       pos.h = "center", 
             #                       pos.v = "top"),
             position = c("right", "top",
                          pos.h = "right", 
                          pos.v = "top"),
             size = 0.9,
             padding = c(2,1,1.5,2),
             col = "gray75",
             fontface = "italic") +
  tm_options(crs = map_crs)

site_map

tmap::tmap_save(site_map, 
                here("plots", "site_v2.png"),
                width = 7,
                height = 6,
                bg = "transparent")



# *******************************************************************

## Observed ranges ----

all_fire_lookup <- read_csv(here("output-data", "lookup-tables", "all_fire_lookup.csv"))

test_fire_lookup <- read_csv(here("output-data", "lookup-tables", "held_out_test_lookup.csv"))

### Fire size ----

fire_meta %>%
  ggplot() +
    geom_histogram(aes(x = acres_within_fire_perimeter),
                   fill = "tomato1",
                   color = "tomato4",
                   bins = 30) +
    scale_x_log10() + 
    theme_bw()


fire_stats %>%
  ggplot() +
  geom_histogram(aes(x = mean_observed_dnbr),
                 fill = "tomato3",
                 color = "tomato4",
                 binwidth = 25) +
  #scale_x_log10() + 
  theme_bw()

### Observed dnbr ----

full_dnbr %>%
  mutate(train_test = ifelse(fire_id %in% test_fire_lookup$fire_id[1], "xtest", "train")) %>%
  ggplot() +
    geom_histogram(aes(x = dnbr, 
                       y = after_stat(count / sum(count)),
                       fill = train_test,
                       alpha = train_test),
                   color = "black",
                   bins = 100,
                   position = "identity",
                   alpha = 0.5) +
    scale_fill_manual(values = c("tan1", "red1")) +
    theme_bw()



# ******************************************

co_utm <- st_transform(co, crs = map_crs)

st_bbox(co_utm)

ggplot(co_utm) +
  geom_sf() + 
  coord_sf(crs = st_crs(26913), default_crs = st_crs(26913)) 

st_crs(co_utm)


tmap::tm_shape(co_utm) +
  tm_polygons(col = "black",
              fill = "gray80",
              fill_alpha = 0.5,
              lwd = 3) +
  tm_text("name_tr", 
          size = 2,
          col = "gray40",
          fontface = "italic") +
  tm_graticules(labels.pos = c("right", "top"),
                col = "black")

