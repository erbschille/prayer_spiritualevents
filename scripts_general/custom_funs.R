# REGRESSION -----

# function for modifying stargazer tables (modifying interaction symbol)
sub_interact_fun <- function(str) {
  str <- gsub("\\`", "", str) # for `Porosity Vignettes`, etc.
  str <- gsub("\\.\\:", "\\. × ", str) # for country
  str <- gsub("\\:Cou", " × Cou", str)
  str <- gsub("\\:Sit", " × Sit", str)
  str <- gsub("\\:Rel", " × Rel", str)
  str <- gsub("\\:Pop", " × Pop", str)
  str <- gsub("\\:Por", " × Por", str)
  str <- gsub("\\:Abs", " × Abs", str)
  str <- gsub("\\:Gen", " × Gen", str)
  str <- gsub("\\:Age", " × Age", str)
  str <- gsub("\\:Edu", " × Edu", str)
  str <- gsub("\\:Pre", " × Pre", str)
  return(str)
}

# function for printing out rsq values for lme4 models
rsq_table_fun <- function(model_list) {
  tab <- rsquared(modelList = model_list) %>%
    mutate_at(vars(Marginal, Conditional), ~ round(., 2)) %>%
    rownames_to_column("model") %>%
    mutate(model = paste0("(", model, ")")) %>%
    select(model, Marginal, Conditional) %>%
    gather(`R-squared Type`, rsq, -model) %>%
    spread(model, rsq) %>%
    arrange(desc(`R-squared Type`)) %>%
    kable() %>%
    kable_styling()
  
  return(tab)
}


# SCORING AND RELIABILITY -----

# function for calculating Cronbach's alpha
alpha_fun <- function(df, which_vars, which_country, which_keys = NULL,
                      which_use = NULL){
  
  if (which_country != "ALL") {
    df0 <- df %>% filter(country == which_country)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>% select(!!which_vars)
  
  res <- psych::alpha(df0, keys = which_keys, use = "pairwise")
  res_alpha <- res$total["raw_alpha"] %>% as.numeric()
  
  return(res_alpha)  
}


# function for scoring scales after omitting items
score_fun <- function(df, var_omit = NA, 
                      var_group = c("country", "subject_id")){
  
  if (!is.na(var_omit)) {
    df0 <- df %>% select(-!!var_omit)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>%
    gather(question, response, -!!var_group) %>%
    group_by_at(var_group) %>%
    summarise(score = mean(response, na.rm = T)) %>%
    ungroup()
  
  return(df0)
  
}


# function for getting ICC stat
icc_fun <- function(df, var_name = NA, 
                    var1 = "response", var2 = "recoded",
                    which_model = "oneway", which_type = "consistency",
                    which_unit = "single") {
  
  df0 <- df %>%
    filter(question == var_name) %>%
    select_at(c(var1, var2))
  
  res <- irr::icc(df0, model = which_model, type = which_type, unit = which_unit)
  
  icc <- res$value
  
  return(icc)
  
}


# function for plotting responses to sensory questions
plot_fun <- function(df, exclude_exclude = T, 
                     which_response = "response", which_type,
                     point_color = c("type", "sense"),
                     add_connectors = T) {
  
  if (exclude_exclude == T) {
    df <- df %>%
      filter(exclude == F)
  }
  
  groups <- c("country", "population", "data_source", "scale", "sense")

  if ("data_set" %in% names(df)) {
    groups <- c(groups, "data_set")
  }
  
  df <- df %>%
    filter(type %in% which_type) %>%
    filter(!is.na(population))
  
  df_scores <- df %>%
    group_by_at(c(groups, "subject_id")) %>%
    summarise(mean_score = mean(!!sym(which_response), na.rm = T)) %>% 
    ungroup() %>%
    mutate(sense = factor(sense,
                          levels = levels_senses,
                          labels = gsub("_", " ", levels_senses)),
           population = factor(population, 
                               levels = c("charismatic", "local",
                                          "general population", "undergraduates")))
  
  df_mb <- df_scores %>%
    group_by_at(groups) %>%
    multi_boot_standard(col = "mean_score", na.rm = T) %>%
    ungroup()
  
  g <- df_mb %>%
    ggplot(aes(x = sense, y = mean, shape = population, size = population)) +
    facet_grid(country ~ data_source, scales = "free_y", space = "free_y")
  
  if (add_connectors == TRUE) {
    if ("data_set" %in% names(df)) {
      g <- g +
        geom_line(aes(group = interaction(population, data_set)),
                  position = position_dodge(width = 0.5),
                  color = "gray50", lty = 2, size = 0.5, shape = NULL)
    } else {
      g <- g +
        geom_line(aes(group = population),
                  position = position_dodge(width = 0.5),
                  color = "gray50", lty = 2, size = 0.5, shape = NULL)
    }
  }

  if (point_color == "type") {
    point_color <- case_when(which_type == "immaterial secular" ~ "darkblue",
                             which_type == "immaterial ambiguous" ~ "darkorange2",
                             which_type == "immaterial spiritual" ~ "forestgreen",
                             grepl("in the mind", which_type) ~ "firebrick",
                             which_type == "truly sensory" ~ "goldenrod")
    
    g <- g + 
      geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                      position = position_dodge(width = 0.5),
                      color = point_color, fill = "white")

  } else if (point_color == "sense") {
    
    g <- g + 
      geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper, color = sense),
                      position = position_dodge(width = 0.5),
                      fill = "white") +
      scale_color_brewer(palette = "Paired", drop = F)
    
  } 
  
  g <- g +
    scale_shape_manual(values = c(23, 17, 19, 15), drop = F) +
    scale_size_manual(values = c(0.2, rep(0.5, 3)), drop = F) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(subtitle = paste("Order of senses:", 
                          paste(gsub("_", " ", levels_senses), collapse = ", ")),
         x = "Sensory modality",
         y = "Mean of participants' scores")
  
  if (length(levels(factor(df$scale_cat))) == 1) {
    
    scale_cat <- df %>% distinct(scale_cat) %>% pull(scale_cat)
    g <- g +
      labs(title = paste0(toupper(scale_cat), ": '", which_type, "' items"))
  }
  
  return(g)
}

# function for listing items by data source, item type, sense, etc.

item_tab_fun <- function(df, exclude_exclude = T, 
                         type_list = c("truly sensory",
                                       "immaterial secular",
                                       "immaterial ambiguous",
                                       "immaterial spiritual",
                                       "in the mind spiritual",
                                       "in the mind"),
                         title = NA) {
  tab <- df %>%
    filter(country == "US") %>%
    group_by(data_source) %>%
    top_n(1, subject_id) %>%
    ungroup()
  
  if (exclude_exclude == TRUE) {
    tab <- tab %>%
      filter(exclude != TRUE)
  }
  
  tab <- tab %>%
    filter(type %in% type_list) %>%
    distinct(type, sense, question_text, data_source) %>%
    group_by(type, sense, question_text) %>%
    summarise(data_sources = paste(data_source, collapse = ", ")) %>%
    ungroup() %>%
    select(type, sense, data_sources, question_text) %>%
    arrange(type, sense, data_sources) %>%
    rename_all(~ gsub("_", " ", .)) %>%
    mutate_all(~ gsub("_", " ", .)) %>%
    kbl(caption = ifelse(is.na(title), "", title)) %>%
    kable_styling() %>%
    # kable_paper(full_width = F) %>%
    collapse_rows(1:4, valign = "top")
  
  return(tab)
}

# make a function for rounding
round_x <- function(n, x = 2) { format(round(n, x), nsmall = x) }

# make a function for effect-coding and specifying which level to drop
contr_effect_fun <- function(df, var_name, level_drop) {
  
  # get levels in the variable
  levels_contr <- levels(factor(df[[var_name]]))
  
  # check that level_drop is in the data
  if (!(level_drop %in% levels_contr)) {
    print(paste0("Level '", level_drop, "' not found in variable '", var_name, ".'"))
  } else {
    
    # make a blank dataframe with rows and columns for all levels of the variable
    new_contr <- data.frame(matrix(ncol = length(levels_contr), nrow = length(levels_contr)))
    rownames(new_contr) <- levels_contr
    colnames(new_contr) <- levels_contr
    
    # put in the 1s and 0s
    for (i in 1:length(levels_contr)) {
      j <- length(levels_contr) - i
      k <- length(levels_contr) - j - 1
      new_contr[levels_contr[i],] <- c(rep(0, k), 1, rep(0, j))
    }
    
    # drop the dropped level by putting in -1s
    new_contr[level_drop,] <- rep(-1, length(levels_contr))
    
    # drop the dropped level from the columns
    new_contr <- new_contr %>% select(-contains(level_drop))
    
    # add "_GM" to the end of variable names
    colnames(new_contr) <- paste(colnames(new_contr), "GM", sep = "_")
    
    new_contr <- as.matrix(new_contr)
    
    return(new_contr)
  }
}
