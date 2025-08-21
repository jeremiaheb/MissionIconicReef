MIR_domain_dens_by_year <- function(dataset, species = NULL, length = NULL, year = NULL, title = NULL) {

  inside <- getDomainDensity(dataset, species$SPECIES_CD, group = species, years = year, status = 1, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "M:IR") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <- getDomainDensity(dataset, species$SPECIES_CD, group = species, years = year, status = 0, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "Outside") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside, out)

  p <- ggplot(a, aes(x = YEAR, y = density, color = protection, group = protection)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25, size = 0.5) +
    ggtitle(title) +
    theme_Publication(base_size = 15) +
    scale_color_manual(name = "Protection Status",
                       values = c("M:IR" = "springgreen3", "Outside" = "deepskyblue4")) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Year") +
    ylab("Density ind/177m2") +
    facet_wrap(~ GROUP, scales = "free_y")

  return(p)
}


MIR_domain_occ_by_year <- function(dataset, species = NULL, length = NULL, year = NULL, title = NULL) {

  inside <- getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 1, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "M:IR") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <- getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 0, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "Outside") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside, out)

  p <- ggplot(a, aes(x = YEAR, y = occurrence, color = protection, group = protection)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  width = 0.25, size = 0.5) +
    ggtitle(title) +
    theme_Publication(base_size = 15) +
    scale_color_manual(name = "Protection Status",
                       values = c("M:IR" = "springgreen3", "Outside" = "deepskyblue4")) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Year") +
    ylab("Relative Occurrence") +
    facet_wrap(~ GROUP, scales = "free_y")

  return(p)
}

#
# MIR_LF <- function(df, spp, bin_size, yrs = NULL, spp_name) {
#
#     x <- getDomainLengthFrequency(df, species = spp, merge_protected = F) %>%
#     group_by(YEAR, SPECIES_CD, protected_status) %>%
#     nest() %>%
#     mutate(Lf = map(data, ~ .x %>%
#                       data.frame() %>%
#                       full_join(., data.frame(length_class = seq(1,max(.$length_class),0.5))) %>%
#                       select(length_class, frequency) %>%
#                       replace(., is.na(.), 0) %>%
#                       mutate(bin= as.numeric(cut(length_class, seq(0,max(length_class) + 5,bin_size)))) %>%
#                       arrange(length_class) %>%
#                       group_by(bin) %>%
#                       summarise(freq = sum(frequency)))) %>%
#     unnest(Lf) %>%
#     select(YEAR, SPECIES_CD, protected_status, bin , freq) %>%
#     ungroup() %>%
#     mutate(value = freq, variable = if_else(protected_status == 1, "M:IR","open"))
#
#     y <- x %>%
#       filter(YEAR == yrs) %>%
#       select(YEAR, SPECIES_CD, variable, bin, value) %>%
#       pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
#       pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
#       mutate(bin = as.numeric(bin))
#
#     plot_bins(x = y, ttle = paste0(spp_name, " ", yrs), bin_size = bin_size)
#
# }
#
# MIR_LF_yr <- function(df, spp, bin_size, yrs = NULL, spp_name, category, custom_title = NULL) {
#   x <- getDomainLengthFrequency(df, species = spp, merge_protected = F) %>%
#     group_by(YEAR, SPECIES_CD, protected_status) %>%
#     nest() %>%
#     mutate(Lf = map(data, ~ .x %>%
#                       data.frame() %>%
#                       full_join(., data.frame(length_class = seq(1,max(.$length_class),0.5))) %>%
#                       select(length_class, frequency) %>%
#                       replace(., is.na(.), 0) %>%
#                       mutate(bin = as.numeric(cut(length_class, seq(0, max(length_class) + 5, bin_size)))) %>%
#                       arrange(length_class) %>%
#                       group_by(bin) %>%
#                       summarise(freq = sum(frequency)))) %>%
#     unnest(Lf) %>%
#     select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
#     ungroup() %>%
#     mutate(value = freq, variable = if_else(protected_status == 1, "M:IR", "open"))
#
#   y <- x %>%
#     filter(YEAR %in% yrs, variable == category) %>%
#     select(YEAR, SPECIES_CD, variable, bin, value) %>%
#     pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
#     pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
#     mutate(bin = as.numeric(bin))
#   if (is.null(custom_title)) {
#     custom_title <- paste0(spp_name, " - ", category)
#   }
#   plot_bins_yr(x = y, ttle = custom_title, bin_size = bin_size, category = category, spp_name = spp_name)
# }

# Function will default to compare 2022 and 2024. Repeat panel syntax below to add more years for comparison
# render_LF_for_species <- function(df, SPECIES_CD, COMNAME, yrs = c(2022, 2024), default_bin = 5) {
#
#   # Look up bin size from bin_size_lookup, fallback to default_bin
#   bin_size <- if (SPECIES_CD %in% names(bin_size_lookup)) {
#     bin_size_lookup[[SPECIES_CD]]
#   } else {
#     default_bin
#   }
#
#   # ---- Individual year plots ----
#   p1 <- MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs[1], spp_name = COMNAME)
#   p2 <- MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs[2], spp_name = COMNAME)
#
#   # ---- Combined across years (by category) ----
#   p3 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
#                   spp_name = COMNAME, category = "M:IR",
#                   custom_title = paste(COMNAME, "- M:IR"))
#
#   p4 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
#                   spp_name = COMNAME, category = "open",
#                   custom_title = paste(COMNAME, "- Open"))
#
#   # ---- Combine into one panel ----
#   (p1 | p2) / (p3 | p4)
# }
#
# bin_size_lookup <- list(
#   "EPI ITAJ" = 10
# )

# Length Frequency bin size calculation
compute_bin_size <- function(max_size, target_bins = 10) {
  if (is.null(max_size) || is.na(max_size) || max_size <= 0) return(5)

  breaks <- pretty(c(0, max_size), n = target_bins)

  # Compute approximate bin size as the difference between first two breaks
  if (length(breaks) > 1) {
    bin_size <- breaks[2] - breaks[1]
  } else {
    bin_size <- 5  # fallback
  }

  return(bin_size)
}

# Length frequency for commparing inside to outside
MIR_LF <- function(df, spp, bin_size, yrs = NULL, spp_name) {

  x <- getDomainLengthFrequency(df, species = spp, merge_protected = FALSE) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      # Ensure at least one length_class for empty data
                      full_join(., data.frame(length_class = if (nrow(.) > 0) seq(1, max(.$length_class), 0.5) else 1)) %>%
                      select(length_class, frequency) %>%
                      replace(is.na(.), 0) %>%
                      mutate(bin = as.numeric(cut(length_class,
                                                  breaks = seq(0, max(length_class, na.rm = TRUE) + bin_size, bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency, na.rm = TRUE))
    )) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
    ungroup() %>%
    mutate(value = freq, variable = if_else(protected_status == 1, "M:IR", "open"))

  y <- x %>%
    filter(YEAR == yrs) %>%
    select(YEAR, SPECIES_CD, variable, bin, value) %>%
    pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
    pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
    mutate(bin = as.numeric(bin))

  if (all(is.na(y$bin)) || nrow(y) == 0) y$bin <- 0

  plot_bins(x = y, ttle = paste0(spp_name, " ", yrs), bin_size = bin_size)
}

# Length frequency for comparing inside and outside in one year
MIR_LF_yr <- function(df, spp, bin_size, yrs = NULL, spp_name, category, custom_title = NULL) {

  x <- getDomainLengthFrequency(df, species = spp, merge_protected = FALSE) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      full_join(., data.frame(length_class = if (nrow(.) > 0) seq(1, max(.$length_class), 0.5) else 1)) %>%
                      select(length_class, frequency) %>%
                      replace(is.na(.), 0) %>%
                      mutate(bin = as.numeric(cut(length_class,
                                                  breaks = seq(0, max(length_class, na.rm = TRUE) + bin_size, bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency, na.rm = TRUE))
    )) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
    ungroup() %>%
    mutate(value = freq, variable = if_else(protected_status == 1, "M:IR", "open"))

  y <- x %>%
    filter(YEAR %in% yrs, variable == category) %>%
    select(YEAR, SPECIES_CD, variable, bin, value) %>%
    pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
    pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
    mutate(bin = as.numeric(bin))

  if (all(is.na(y$bin)) || nrow(y) == 0) y$bin <- 0
  if (is.null(custom_title)) custom_title <- paste0(spp_name, " - ", category)

  plot_bins_yr(x = y, ttle = custom_title, bin_size = bin_size, category = category, spp_name = spp_name)
}

#Function that outputs plots for showing all relevant LF plots for comparing 2 years
#Defaults to 2022/2024 if not specified in .qmd.
#To compare more than 2 years, add extra panels for each plot type
render_LF_plots <- function(df, SPECIES_CD, COMNAME, max_size = NULL, yrs = c(2022, 2024), target_bins = 10) {

  # ---- Compute  bin size ----
  bin_size <- if (!is.null(manual_bin) && SPECIES_CD %in% names(manual_bin)) {
    manual_bin[[SPECIES_CD]]   # use manual bin from vector
  } else {
    compute_bin_size(max_size, target_bins)  # dynamic default
  }

  # Individual year plots
  p1 <- MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs[1], spp_name = COMNAME)
  p2 <- MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs[2], spp_name = COMNAME)

  # Combined category plots
  p3 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
                  spp_name = COMNAME, category = "M:IR",
                  custom_title = paste(COMNAME, "- M:IR"))

  p4 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
                  spp_name = COMNAME, category = "open",
                  custom_title = paste(COMNAME, "- Open"))

  (p1 | p2) / (p3 | p4)
}

#Manually adjust the bin size based on species code
manual_bin <- c("STE PLAN" = 2, "EPI MORI" = 5)
