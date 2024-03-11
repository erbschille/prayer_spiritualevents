# levels
levels_country <- c("US", "Ghana", "Thailand", "China", "Vanuatu")
levels_site <- c("urban", "rural")
levels_religion <- c("charismatic", "local")
levels_researcher <- c("JBrahinsky", "Jdulin", "VDzokoto",
                       "Faulino", "Emng", "Rsmith")
levels_senses <- c("visual", "auditory", "tactile", "olfactory", 
                   "gustatory", "near_tangible", "bodily_other")
levels_types <- c("truly sensory", 
                  "immaterial secular",
                  "immaterial ambiguous",
                  "immaterial spiritual",
                  "in the mind spiritual")
levels_type_colors <- c("yellow", "blue", "orange", "green", "red")
levels_vvp_code <- c("voice", "vision", "touch", "taste", "smell", "presence", "sleep_paralysis")
levels_scales <- c("spiritual events", "launay-slade hallucinations",
                   "posey-losch hearing events", "taves inoe", "sensory events",
                   # "ratio of the senses (part 1)",
                   "ratio of the senses (part 2)", 
                   "absorption", 
                   "sensory seeking",
                   "worthman hypnogogic events")
levels_datasources <- c("interviews", "interviews (frequency)", "spiritual epi",
                        "taves inoe", "packet 1", "packet 2", "packet 3", 
                        "packet 6", "packet 7", "packet 8")



# contrasts (effect-coding)
contrasts_country <- cbind("_gh" = c(-1, 1, 0, 0, 0),
                           "_th" = c(-1, 0, 1, 0, 0),
                           "_ch" = c(-1, 0, 0, 1, 0),
                           "_vt" = c(-1, 0, 0, 0, 1))
contrasts_site <- cbind("_rural" = c(-1, 1))
contrasts_religion <- cbind("_char" = c(1, -1))
