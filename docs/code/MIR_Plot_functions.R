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


MIR_LF <- function(df, spp, bin_size, yrs = NULL, spp_name) {

    x <- getDomainLengthFrequency(df, species = spp, merge_protected = F) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      full_join(., data.frame(length_class = seq(1,max(.$length_class),0.5))) %>%
                      select(length_class, frequency) %>%
                      replace(., is.na(.), 0) %>%
                      mutate(bin= as.numeric(cut(length_class, seq(0,max(length_class) + 5,bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency)))) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin , freq) %>%
    ungroup() %>%
    mutate(value = freq, variable = if_else(protected_status == 1, "M:IR","open"))

    y <- x %>%
      filter(YEAR == yrs) %>%
      select(YEAR, SPECIES_CD, variable, bin, value) %>%
      pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
      pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
      mutate(bin = as.numeric(bin))

    plot_bins(x = y, ttle = paste0(spp_name, " ", yrs), bin_size = bin_size)

}

MIR_LF_yr <- function(df, spp, bin_size, yrs = NULL, spp_name, category, custom_title = NULL) {
  x <- getDomainLengthFrequency(df, species = spp, merge_protected = F) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      full_join(., data.frame(length_class = seq(1,max(.$length_class),0.5))) %>%
                      select(length_class, frequency) %>%
                      replace(., is.na(.), 0) %>%
                      mutate(bin = as.numeric(cut(length_class, seq(0, max(length_class) + 5, bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency)))) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
    ungroup() %>%
    mutate(value = freq, variable = if_else(protected_status == 1, "M:IR", "open"))

  y <- x %>%
    filter(YEAR %in% yrs, variable == category) %>%  # Filter by category (M:IR or open)
    select(YEAR, SPECIES_CD, variable, bin, value) %>%
    pivot_wider(names_from = bin, values_from = value, values_fill = 0) %>%
    pivot_longer(!c(YEAR, SPECIES_CD, variable), names_to = "bin", values_to = "value") %>%
    mutate(bin = as.numeric(bin))

  # If no custom title is provided, default to species name and category
  if (is.null(custom_title)) {
    custom_title <- paste0(spp_name, " - ", category)
  }

  # Pass spp_name, category, and custom_title to plot_bins_yr
  plot_bins_yr(x = y, ttle = custom_title, bin_size = bin_size, category = category, spp_name = spp_name)
}
