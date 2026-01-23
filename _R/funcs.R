
read_data = function(pattern){
  list.files("_data", pattern, full.names = T) |> 
    map_dfr(read_tsv) |> 
    distinct()|>  
    filter(!is.na(model)) |> 
    distinct(id, model, .keep_all = T)  
}

prep_for_analysis = function(df, full_df){
  df |> 
    group_by(id) |> 
    slice_sample(n=1) |> 
    ungroup() |> 
    left_join(full_df)
}

get_codebook = function(filename){
  yaml::read_yaml(here::here(glue::glue("_codebooks/{filename}.yml")))$task
}

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finalize_plot <- function(p, is_binary = FALSE) {
  require(scales)
  vals <- c(p$data$conf.low, p$data$conf.high)

  if (is_binary) {
    step <- 0.05
    lims <- c(floor(min(vals) / step) * step, ceiling(max(vals) / step) * step)
    lims[1] <- max(lims[1], -0.05)
    lims[2] <- min(lims[2], 1.05)
    brk <- seq(floor(lims[1] / step) * step, ceiling(lims[2] / step) * step, by = step)
    brk <- brk[brk >= lims[1] & brk <= lims[2]]
    if (length(brk) > 7) {
      brk <- pretty(lims, n = 7)
    }
    p <- p + scale_y_continuous(limits = lims, breaks = brk, labels = scales::percent)
  } else {
    step <- 5
    lims <- c(floor(min(vals) / step) * step, ceiling(max(vals) / step) * step)
    brk <- seq(floor(lims[1] / step) * step, ceiling(lims[2] / step) * step, by = step)
    brk <- brk[brk >= lims[1] & brk <= lims[2]]
    if (length(brk) > 7) {
      brk <- pretty(lims, n = 7)
    }
    p <- p + scale_y_continuous(limits = lims, breaks = brk)
  }
  p
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(knitr.duplicate.label = "allow")


std_analysis = function(df, outcome, ...){
  df = df |> 
    count(brand, country) |> 
    mutate(outlet_n = paste(brand, "ZZZ", n)) |> 
    right_join(df, by = c("brand", "country")) |> 
    mutate(outlet = outlet_n) |> 
    filter(if_all(c(platform, country, outlet, outlet_type), ~ !is.na(.))) 
  
  regions = distinct(df, country, region)

  is_binary = min(df[,outcome], na.rm = T) >= 0 & max(df[,outcome], na.rm = T) <= 1

  frm_outlets = paste(outcome, "~ 1 + (1|outlet)")
  m_outlets = glmer(frm_outlets, data = df, ...) 
  
  frm_countries = paste(outcome, "~ 1 + (1|country)")
  m_countries = glmer(frm_countries, data = df, ...) 
  
  frm_platforms = paste(outcome, "~ platform + outlet_type + (1|country)")
  m_platforms = glmer(frm_platforms, data = df, ...) 
  
  avg = avg_predictions(m_platforms, re.form = NULL)
  
  avg_contrasts = avg_comparisons(m_platforms, re.form = NULL)
  
  nobs = nrow(df)

  outlet_preds = m_outlets |> 
    avg_predictions(by = "outlet", re.form = NULL)  |> 
    as_tibble() |> 
    left_join(distinct(df, outlet, country)) |> 
    mutate(brand = str_remove_all(outlet, " ZZZ.*")) |> 
    left_join(regions)
  

  country_preds = m_countries |> 
    avg_predictions(by = "country", re.form = NULL) |> 
    as_tibble() |> 
    left_join(regions)
  
  country_plot = #outlet_preds |> 
    #left_join(country_preds |> select(country, region, avg = estimate)) |> 
    #filter(!estimate %in% boxplot(estimate, plot = FALSE)$out) |> 
    country_preds |> 
    ggplot(aes(x = reorder(country, estimate),  y = estimate, color = region,
               ymin = conf.low, ymax = conf.high))+
    geom_hline(yintercept = avg$estimate, linetype = "dashed")+
    geom_point(data = outlet_preds, aes(x = country, y = estimate, color = region), alpha = .33, size = 1)+
    geom_pointrange()+
    #geom_pointrange(data = country_preds, aes(x = country, y = estimate, ymin = conf.low, ymax = conf.high, color = region))+
    labs(y = outcome, color = "Region")+
    coord_flip()
  
  platform_preds = m_platforms |> 
    avg_predictions(by = "platform", re.form = NULL) |> 
    as_tibble()
  
  platform_plot =
    platform_preds |> 
    ggplot(aes(x = fct_rev(platform), y = estimate, ymin = conf.low, ymax = conf.high))+
    geom_hline(yintercept = avg$estimate, linetype = "dashed")+
    geom_pointrange()+
    labs(y = outcome)+
    coord_flip()

  
  type_preds = m_platforms |> 
    avg_predictions(by = "outlet_type", re.form = NULL) |> 
    as_tibble()
  
  type_plot =
    type_preds |> 
    ggplot(aes(x = fct_rev(outlet_type), y = estimate, ymin = conf.low, ymax = conf.high))+
    geom_hline(yintercept = avg$estimate, linetype = "dashed")+
    geom_pointrange()+
    labs(y = outcome)+
    coord_flip()
  
  platform_plot = finalize_plot(platform_plot, is_binary)
  type_plot = finalize_plot(type_plot, is_binary)
  country_plot = finalize_plot(country_plot, is_binary)
  
  #if(is_binary){
  #  country_plot = country_plot+scale_y_continuous(labels = scales::percent)
  #  platform_plot = platform_plot+scale_y_continuous(labels = scales::percent)
  #  type_plot = type_plot+scale_y_continuous(labels = scales::percent)
  #}
  
  
  results = lst(avg, avg_contrasts, nobs, outlet_preds, country_preds, platform_preds, type_preds, platform_plot, country_plot, platform_plot, type_plot)
  saveRDS(results, paste0(here::here("_data/results/"), outcome, ".rds"))
  results
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
std_plots = function(results, title = "A standard title", short = NULL, 
                     ylab = "Standard outcome", text = NULL, unit = "posts"){
  if(is.null(short)) {short = title}
  tpl = read_file(here::here("_includes/_stdplots.qmd"))
  glue::glue(tpl, .open = "{{", .close = "}}")
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
std_text = function(results, title = "Title", ylab = "Outcome"){
  results$avg_contrasts = results$avg_contrasts  |> filter(p.value <= .05)
  
  md = results_to_md(results)
   
  prompt = glue::glue("Below, you will find an analysis of short videos posts. Write a 1-paragraph non-technical summary of the following results for a science magazine.  Start with the overall average, and then cover possible differences by (1) platform, (2) outlet type and maybe (3) by country. For platform and outlet type, be careful to highlight only statistically significant and substantially meaningful contrasts which are found in the respective contrast tables or model summaries, otherwise state that the differences are small and non-significant. Double check statistical significance, either by looking a p-values or confidence intervals, but do not report p-values or CI in text. Use bold for specific highlighted numbers, if you choose to highlight them. Round numbers to integers where appropriate (e.g. percentages), otherwise round to one decimal at most. Prefer reporting predicted values rather than contrasts, unless the contrast is important. Do not provide additional interpretations without specific proof based on the analyses.  Return *only* the interpretation without additional text. Do not capitalize the category names unless it's appropriate. Use past tense when writing about findings, use active 'we' voice. Text:\n\n ## Results for {title} \n\n Outcome variable: {ylab}\n\n {md} ")

ellmer::chat_google_gemini(system_prompt = "You are a prolific science writer. You produce high-quality text for an interested audience of journalists and researchers from different fields.")$chat(prompt)
  
}

results_to_md = function(results){
  require(glue)
  rm_cols = c("df", "s.value", "std.error", "statistic")
  out =  map(results[c("avg", "avg_contrasts", "platform_preds", "type_preds", "country_preds")], 
            ~ select(.x, -any_of(rm_cols))) |> 
    map( ~ knitr::kable(.x, format="pipe", digits = 2) |>
              paste(collapse = "\n")) 
  tpl = "\n\n### Overall average \n\n {out$avg} \n\n### Contrasts \n\n {out$avg_contrasts} \n\n### Predictions by platform \n\n{out$platform_preds} \n\n### Predictions by outlet type \n\n{out$type_preds} \n\n### Predictions by country \n\n{out$country_preds} \n\n"
  glue::glue(tpl)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
var_name_to_text <- function(var_name) {
  if (str_detect(var_name, "and")) {
    str_replace_all(var_name, "_", " ") %>% 
      str_to_sentence()
  } else {
    str_replace_all(var_name, "_", " ") %>% 
      str_to_sentence()
  }
}

country_tab = function(results, d){
  df = read_rds(here::here(glue::glue("_data/results/{results}.rds")))
  avg = filter(df$country_preds, country == d$country[1])$estimate[1]
  
  df$outlet_preds |> 
    inner_join(count(d, brand, country, outlet_type)) |> 
    with_fa() |> 
    select(brand, m = estimate) |>
    bind_rows(tibble(brand = paste(fa("globe"), "Overall"), m = avg)) |> 
    mutate_if(is.numeric, round, 2) |> 
    set_names(c("Outlet", results))
}

to_kable = function(tab){
  tab |> 
    mutate_if(is.numeric, ~na_if(., 0)) |> 
    mutate_if(is.numeric, ~na_if(., 0)) |> 
    kbl(escape = FALSE) |> 
    column_spec(1, extra_css = "white-space: nowrap;") |> 
    row_spec(nrow(tab), bold = T)
}

with_fa = function(df){
  df |> mutate(
    brand = case_when(outlet_type == "Print" ~ paste(fa("newspaper"),brand),
                       outlet_type == "Digital" ~ paste(fa("laptop"), brand),
                      outlet_type == "Private broadcaster" ~ paste(fa("satellite-dish"), brand),
                      outlet_type == "Public broadcaster" ~ paste(fa("building-columns"), brand),
            T ~ brand)) |> 
    mutate(brand = str_remove_all(brand, "\\(public broadcaster\\)"))
}

outlet_desc = function(df){
  df |> 
    distinct(brand, country, outlet_type) |> 
    count(outlet_type) |> 
    mutate(outlet_type = case_when(outlet_type == "Print" ~ paste(outlet_type, fa("newspaper")),
                      outlet_type == "Digital" ~ paste(outlet_type, fa("laptop")),
                      outlet_type == "Private broadcaster" ~ paste(outlet_type, fa("satellite-dish")),
                      outlet_type == "Public broadcaster" ~ paste(outlet_type, fa("building-columns")),
                      T ~ outlet_type)) |> 
    arrange(-n) |> 
    deframe() |> 
    as.list() |> 
    imap(~ glue::glue("{.x} {.y}")) |> paste(collapse = ", ")
}
