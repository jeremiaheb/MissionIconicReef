MIR_domain_dens_barplot <- function(dataset, species, length = NULL, title = NULL) {

  inside <-  getDomainDensity(dataset, species$SPECIES_CD, group = species, status = 1, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protection = "M:IR") %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <-  getDomainDensity(dataset, species$SPECIES_CD, group = species, status = 0, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protection = "Outside") %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside,out)

  yupper <- max(a$density + a$SE)

  p <- ggplot(a, aes(x=reorder_where(GROUP, -density, protection == "M:IR"), y=density, fill = protection)) +
    geom_col(position = position_dodge(0.9),
             width = .8,
             color="black",
             size=.5) +
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
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
    scale_y_continuous(expand = c(0,0), limits = c(0,yupper + yupper*.01)) +
    scale_fill_manual(values=c('springgreen3','deepskyblue4','gold1')) +
    ylab("density ind/177m2")

  return(p)
}

MIR_domain_occ_barplot <- function(dataset, species, length = NULL, title = NULL) {

  inside <-  getDomainOccurrence(dataset, species$SPECIES_CD, group = species, status = 1, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protection = "M:IR") %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <-  getDomainOccurrence(dataset, species$SPECIES_CD, group = species, status = 0, length_bins = length) %>%
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

  inside <- binned_LenFreq(d = df, spp = spp, bin_size = bin_size, st = 1, colName = "M:IR")
  outside <- binned_LenFreq(d = df, spp = spp, bin_size = bin_size, st = 0, colName = "Outside")

  l <- full_join(inside, outside) %>%
    replace(., is.na(.), 0) %>%
    pivot_longer(cols = -bin, names_to = "variable", values_to = "value") %>%
    plot_bins(ttle = spp_name, bin_size = bin_size)

  return(l)

}
