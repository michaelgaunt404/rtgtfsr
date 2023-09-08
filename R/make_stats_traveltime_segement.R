make_stats_traveltime_segement = function (data, grp_c = c("segement_name", "flag_peak", "direction_id"),
                              quantiles = seq(0, 1, 0.05))
{
  data %>%
    group_by(across({{grp_c}})) %>%
    summarize(rec_count = n()
              ,time_diff_avg_mean = mean(time_diff, na.rm = T)
              ,time_diff_avg_var = var(time_diff, na.rm = T)
              ,time_diff_avg_sd = sd(time_diff , na.rm = T)
              ,time_diff_avg_stderr = time_diff_avg_sd/sqrt(rec_count)
              ,time_diff_avg_pre = 1/time_diff_avg_var
              ,across(time_diff, purrr::map(quantiles,
                                            ~purrr::partial(DescTools::Quantile, probs = .x)),
                      .names = "{.col}_qt_{quantiles}")) %>%
    ungroup()
}
