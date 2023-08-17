speed_profile_index_stats = function(data, grp_c = c('index', 'flag_peak_time', 'direction_id'), quantiles = seq(0, 1, .05)){
  data %>%
    group_by(across({{grp_c}})) %>%
    summarize(
      rec_count = n()
      ,speed_avg_mean = mean(speed_avg, na.rm = T)
      ,speed_avg_var = var(speed_avg, na.rm = T)
      ,speed_avg_sd = sd(speed_avg, na.rm = T)
      ,speed_avg_stderr = speed_avg_sd/sqrt(rec_count)
      ,speed_avg_pre = 1/speed_avg_var
      ,across(speed_avg, purrr::map(quantiles, ~purrr::partial(DescTools::Quantile, probs = .x))
              ,.names = "{.col}_qt_{quantiles}")
    ) %>%
    ungroup()
}


