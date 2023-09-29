plot_domain_den_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getDomainDensity(dataset, species, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$density + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=density, group = 1)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    xlab("Year") + ylab("density ind/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
}

plot_domain_den_by_year_by_prot <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getDomainDensity(dataset, species, length_bins = length, merge_protected = F) %>%
    filter(!protected_status == "all") %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protected_status = as_factor(protected_status)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$density + a$SE)
  
   p <- ggplot(a, aes(x=YEAR, y=density, color = protected_status, group = protected_status)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    scale_color_Publication() +
    xlab("Year") + ylab("Density ind/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
  
}

plot_stratum_den_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getStratumDensity(dataset, species, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            PROT = as_factor(PROT)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
    
  
  yupper = max(a$density + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=density, color = PROT, group = PROT)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25,
                  size = 0.5) + 
    facet_wrap(~STRAT, scales = "fixed") +
    ggtitle(title) + 
    theme_Publication(base_size = 18) +
    scale_color_Publication() +
    xlab("Year") + ylab("Density ind/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
  
}

plot_domain_occ_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe =FALSE) {
  
  a <- getDomainOccurrence(dataset, species, length_bins = length) %>%
    mutate(SE = sqrt(var),
           YEAR = as_factor(YEAR)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$occurrence + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=occurrence, group = 1)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    xlab("Year") + ylab("Occurrence")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
}

plot_domain_occ_by_year_by_prot <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getDomainOccurrence(dataset, species, length_bins = length, merge_protected = F) %>%
    filter(!protected_status == "all") %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protected_status = as_factor(protected_status)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$occurrence + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=occurrence, color = protected_status, group = protected_status)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    xlab("Year") + ylab("Occurrence")
  
  # print(list(a,p))
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
}

plot_stratum_occ_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getStratumOccurrence(dataset, species, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            PROT = as_factor(PROT)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  
  yupper = max(a$occurrence + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=occurrence, color = PROT, group = PROT)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  width = 0.25,
                  size = 0.5) + 
    facet_wrap(~STRAT, scales = "fixed") +
    ggtitle(title) + 
    theme_Publication(base_size = 18) +
    scale_color_Publication() +
    xlab("Year") + ylab("Occurrence")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
  
}

plot_domain_biomass_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getDomainBiomass(dataset, species, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$biomass + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=biomass, group = 1)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = biomass - SE, ymax = biomass + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    xlab("Year") + ylab("Biomass kg/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
}

plot_domain_biomass_by_year_by_prot <- function(dataset, species, title = NULL, print_dataframe = FALSE) {
  
  a <-  getDomainBiomass(dataset, species, merge_protected = F) %>%
    filter(!protected_status == "all") %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            protected_status = as_factor(protected_status))
  
  yupper = max(a$biomass + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=biomass, color = protected_status, group = protected_status)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = biomass - SE, ymax = biomass + SE),
                  width = 0.25,
                  size = 0.5) + 
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    scale_color_Publication() +
    xlab("Year") + ylab("biomass kg/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
  
}

plot_stratum_biomass_by_year <- function(dataset, species, length = NULL, title = NULL, print_dataframe = FALSE) {
  
  a <-  getStratumBiomass(dataset, species, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            YEAR = as_factor(YEAR),
            PROT = as_factor(PROT)) %>%
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  
  yupper = max(a$biomass + a$SE)
  
  p <- ggplot(a, aes(x=YEAR, y=biomass, color = PROT, group = PROT)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = biomass - SE, ymax = biomass + SE),
                  width = 0.25,
                  size = 0.5) + 
    facet_wrap(~STRAT, scales = "fixed") +
    ggtitle(title) + 
    theme_Publication(base_size = 18) +
    scale_color_Publication() +
    xlab("Year") + ylab("biomass kg/177m2")
  
  ifelse(isTRUE(print_dataframe), print(list(a,p)), print(p))
  
}

plot_domain_LF_by_year <- function(data, species, bin_size, yrs = NULL, vertical_line = NULL, title = NULL) {
  
  a <- lenFreq_MultiYear_by_spp(df = data, spp = species, bin_size = bin_size, yrs = yrs, spp_name = species)
  
  plot_bins(x = a, ttle = title, bin_size = bin_size,vline_at_lc = vertical_line)
  
  
}

plot_cv_vs_occurrence <- function(df, yr, plotTitle){
  library(grid)
  library(gridExtra)
  
  reg <- df$sample_data[1,]$REGION
  
  allocation_spp <- case_when(reg == "DRY TORT" | reg == "FLA KEYS"           ~ list(c("MYC BONA", "EPI MORI", "LAC MAXI", "LUT ANAL", "OCY CHRY")),
                              reg == "SEFCRI"                                 ~ list(c("BAL CAPR", "EPI MORI", "LAC MAXI", "LUT ANAL", "LUT GRIS", "OCY CHRY")),
                              reg == "PRICO" | reg == "STTSTJ" | reg == "STX" ~ list(c("ACA COER", "BAL VETU" , "CEP FULV", "CHA CAPI", "HAE FLAV", "OCY CHRY", "SPA VIRI", "EPI GUTT"))) %>%
    unlist()
  
  a <- getDomainDensity(x = df, species = df$taxonomic_data$SPECIES_CD, years = yr ) %>%
    mutate(CV_d = (sqrt(var) / density))
  
  b <- a %>%
    filter(SPECIES_CD %in% allocation_spp)
  c <- a %>%
    filter(!SPECIES_CD %in% allocation_spp) %>%
    filter(CV_d <= .25) %>%
    slice_min(., order_by = CV_d, n = (50 - length(allocation_spp)))
  
  d <- rbind(b,c)
  
  a_occ <- getDomainOccurrence(df, species = d$SPECIES_CD, years = yr)
  
  f <- d %>%
    merge(., select(.data = a_occ, c("SPECIES_CD", "occurrence"))) %>%
    merge(., df$taxonomic_data) %>%
    select(SPECIES_CD, YEAR, REGION, occurrence, var, CV_d, COMNAME)
  
  return(CV_vs_metric(data = f, metric = "occurrence", ptitle = plotTitle))
  
}

#same as plotSubregionDens -- ignore
# plot_topN_density <- function(data, yr, topOcc_n) {
#   
#   plotSubregionDens(df1 = data, yr = yr, topOcc_n = topOcc_n)
#   
# }


