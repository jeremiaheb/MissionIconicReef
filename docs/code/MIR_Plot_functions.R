MIR_domain_dens_by_year <- function(dataset, species = NULL, length = NULL, year = NULL, title = NULL, print_dataframe = FALSE) {

  if (is.data.frame(species)) {
    species <- species$SPECIES_CD
  }
  inside <- getDomainDensity(dataset, species, years = year, status = 1, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "M:IR") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <- getDomainDensity(dataset, species, years = year, status = 0, length_bins = length) %>%
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
    facet_wrap(~ SPECIES_CD, scales = "free_y")


  if (print_dataframe) { print(list(a, p)) } else {print(p)}
}


MIR_domain_occ_barplot <- function(dataset, species, year = NULL, length = NULL, title = NULL) {

  inside <-  getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 1, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protection = "M:IR") %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <-  getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 0, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protection = "Outside") %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside,out)

  p <- ggplot(a, aes(x=reorder_where(GROUP, -occurrence, protection == "M:IR"), y=occurrence, fill = protection)) +
    geom_col(position = position_dodge(0.9),
             width = .8,
             color="black",
             size=.5) +
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  position = position_dodge(0.9),
                  width = 0.15,
                  size = 0.5) +
    ggtitle(title) +
    theme_Publication(base_size = 20) +
    scale_color_Publication() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 10)) +
    scale_x_discrete(labels = function(x) { sub("\\s","\n", x) }) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
    scale_fill_manual(values=c('springgreen3','deepskyblue4','gold1')) +
    ylab("Relative Occurrence")

  return(p)
}

reorder_where <- function (x, by, where, fun = mean, ...) {
  xx <- x[where]
  byby <- by[where]
  byby <- tapply(byby, xx, FUN = fun, ...)[x]
  reorder(x, byby)
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
