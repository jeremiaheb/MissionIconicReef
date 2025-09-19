# Strata table
render_strata_table <- function(df, caption = "Number of sites sampled by year") {

  table <- df %>%
    group_by(YEAR, PROT, STRAT) %>%
    summarise(n = n_distinct(PRIMARY_SAMPLE_UNIT), .groups = "drop") %>%
    mutate(description = case_when(
      STRAT == "FK01" ~ "Inshore reefs, all depths",
      STRAT == "FK02" ~ "Mid-channel patch reefs, all depths",
      STRAT == "FK03" ~ "Offshore patch, all depths",
      STRAT == "FK04" ~ "Forereef, low rugosity, <12m",
      STRAT == "FK05" ~ "Forereef, high rugosity, <12m",
      TRUE ~ STRAT
    ),
    PROT = case_when(
      PROT == 0 ~ "Outside",
      PROT == 1 ~ "Inside",
      TRUE ~ as.character(PROT)
    ),
    PROT = factor(PROT, levels = c("Outside", "Inside"))) %>%
    select(YEAR, PROT, STRAT, description, n) %>%
    pivot_wider(names_from = YEAR, values_from = n, values_fill = 0) %>%
    arrange(PROT, STRAT) %>%
    ungroup()

  if (knitr::is_html_output()) {
    DT::datatable(
      table,
      class = "cell-border stripe",
      rownames = FALSE,
      colnames = c("Study Area", "Strata Name", "Strata Description", sort(unique(df$YEAR))),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold;',
        caption
      ),
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        info = FALSE,
        paging = FALSE,
        searching = FALSE
      )
    )
  } else {
    knitr::kable(
      table,
      caption = caption,
      booktabs = TRUE,
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "hold_position"),
        font_size = 10
      )
  }
}

# Species Table

render_species_table <- function(spp_list, caption = "Table 2: Fish Species") {

  # Add image paths if missing
  if (!"img_path" %in% colnames(spp_list)) {
    spp_list <- spp_list %>%
      mutate(img_path = file.path("species_photos", paste0(gsub(" ", "_", SPECIES_CD), ".png")))
  }

  if (knitr::is_html_output()) {
    # HTML table with clickable thumbnails
    spp_table_html <- spp_list %>%
      mutate(Photo = ifelse(
        file.exists(img_path),
        paste0(
          '<img src="', img_path,
          '" height="60" style="cursor:pointer;" onclick="showLightbox(\'', img_path, '\')">'
        ),
        ""
      )) %>%
      select(SPECIES_CD, COMNAME, SCINAME, Photo)

    htmltools::tagList(
      # Lightbox div + JS
      htmltools::HTML('
        <div id="lightbox" style="display:none;position:fixed;z-index:9999;left:0;top:0;width:100%;height:100%;background:rgba(0,0,0,0.8);justify-content:center;align-items:center;" onclick="this.style.display=\'none\'">
          <img id="lightbox-img" style="max-width:90%;max-height:90%;">
        </div>
        <script>
          function showLightbox(src){
            document.getElementById("lightbox-img").src = src;
            document.getElementById("lightbox").style.display = "flex";
          }
        </script>
      '),
      DT::datatable(
        spp_table_html,
        escape = FALSE,
        class = "cell-border stripe",
        rownames = FALSE,
        colnames = c("Species Code", "Common Name", "Scientific Name", "Photo"),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; font-weight: bold;',
          caption
        ),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          info = FALSE,
          paging = FALSE,
          searching = FALSE
        )
      ) %>%
        DT::formatStyle(columns = "SCINAME", fontStyle = "italic"))

  } else {
    # PDF table with images (blank if missing)
    spp_table_pdf <- spp_list %>%
      mutate(Photo = paste0("\\includegraphics[width=3cm]{", img_path, "}")) %>%
      select(SPECIES_CD, COMNAME, SCINAME, Photo)

    knitr::kable(
      spp_table_pdf,
      format = "latex",
      caption = caption,
      col.names = c("Species Code", "Common Name", "Scientific Name", "Photo"),
      booktabs = TRUE,
      escape = FALSE,
      sanitize.text.function = identity
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "hold_position"),
        font_size = 9
      ) %>%
      kableExtra::column_spec(3, italic = TRUE)
  }
}
#Density function

MIR_domain_dens_by_year <- function(dataset, species = NULL, length = NULL, year = NULL, title = NULL, caption = NULL) {

  strat_number = dataset$stratum_data %>%
    reframe(strat_num = n_distinct(STRAT), .by = c(YEAR, PROT)) %>%
    mutate(YEAR = as_factor(YEAR))

  inside <- getDomainDensity(dataset, species$SPECIES_CD, group = species, years = year,
                             status = 1, length_bins = length) %>%
    mutate(PROT = 1,
           SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "M:IR") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)


  out <- getDomainDensity(dataset, species$SPECIES_CD, group = species, years = year,
                          status = 0, length_bins = length) %>%
    mutate(PROT = 0,
           SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "Outside") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside, out) %>% left_join(strat_number)

  #Compute significance
  signif_table <- a %>%
    group_by(YEAR, GROUP) %>%
    nest() %>%
    mutate(signif = map(.f = apply_ttest_dens, .x = data)) %>%
    mutate(signif = if_else(grepl("NOT", signif), FALSE, TRUE))

  a <- a %>%
    left_join(signif_table %>% select(GROUP, YEAR, signif),
              by = c("GROUP", "YEAR"))

  #Labeling significance
  signif_df <- inside %>%
    select(GROUP, YEAR, density_in = density, SE_in = SE) %>%
    left_join(out %>% select(GROUP, YEAR, density_out = density, SE_out = SE),
              by = c("GROUP", "YEAR")) %>%
    left_join(signif_table %>% select(GROUP, YEAR, signif),
              by = c("GROUP", "YEAR")) %>%
    filter(signif) %>%
    mutate(
      y_pos = (
        if_else(density_in > density_out, density_in - SE_in, density_out - SE_out) +
          if_else(density_in > density_out, density_out + SE_out, density_in + SE_in)
      ) / 2
    )

  p <- ggplot(a, aes(x = YEAR, y = density, color = protection, group = protection)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25, size = 0.5) +
    ggtitle(title) +
    labs(
        title = title,
        caption = caption)+
    theme_Publication(base_size = 15) +
    theme(plot.caption = element_text(hjust = 0.5))+
    scale_color_manual(name = "Protection Status",
                       values = c("M:IR" = "springgreen3", "Outside" = "deepskyblue4")) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Year") +
    ylab("Relative Density (177 ind/m²)") +
    facet_wrap(~ GROUP, scales = "free_y") +
    geom_text(data = signif_df,
               aes(x = YEAR, y = y_pos, label = "*"),
               inherit.aes = FALSE, size = 8,
               vjust = .75,
               color = "black")
  return(p)

}
#Occurrence function
MIR_domain_occ_by_year <- function(dataset, species = NULL, length = NULL, year = NULL, title = NULL, caption = NULL) {

  strat_number = dataset$stratum_data %>%
    reframe(strat_num = n_distinct(STRAT), .by = c(YEAR, PROT)) %>%
    mutate(YEAR = as_factor(YEAR))

  inside <- getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 1, length_bins = length) %>%
    mutate(PROT = 1,
           SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "M:IR") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  out <- getDomainOccurrence(dataset, species$SPECIES_CD, group = species, years = year, status = 0, length_bins = length) %>%
    mutate(PROT = 0,
           SE = sqrt(var),
           YEAR = as_factor(YEAR),
           protection = "Outside") %>%
    filter(if (!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)

  a <- rbind(inside, out) %>% left_join(strat_number)

  signif_table <- a %>%
    group_by(YEAR, GROUP) %>%
    nest() %>%
    mutate(signif = map(.f = apply_ttest_occ, .x = data)) %>%
    mutate(signif = if_else(grepl("NOT", signif), FALSE, TRUE))


  a <- a %>%
    left_join(signif_table %>% select(GROUP, YEAR, signif),
              by = c("GROUP", "YEAR"))

  signif_df <- inside %>%
    select(GROUP, YEAR, occ_in = occurrence, SE_in = SE) %>%
    left_join(out %>% select(GROUP, YEAR, occ_out = occurrence, SE_out = SE),
              by = c("GROUP", "YEAR")) %>%
    left_join(signif_table %>% select(GROUP, YEAR, signif),
              by = c("GROUP", "YEAR")) %>%
    filter(signif) %>%
    mutate(
      y_pos = (
        if_else(occ_in > occ_out, occ_in - SE_in, occ_out - SE_out) +
          if_else(occ_in > occ_out, occ_out + SE_out, occ_in + SE_in)
      ) / 2
    )

  # Plot
  p <- ggplot(a, aes(x = YEAR, y = occurrence, color = protection, group = protection)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                  width = 0.25, size = 0.5) +
    ggtitle(title) +
    labs(
        title = title,
        caption = caption)+
    theme_Publication(base_size = 15) +
    theme(plot.caption = element_text(hjust = 0.5))+
    scale_color_manual(name = "Protection Status",
                       values = c("M:IR" = "springgreen3", "Outside" = "deepskyblue4")) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Year") +
    ylab("Relative Occurrence") +
    facet_wrap(~ GROUP, scales = "free_y") +
    geom_text(data = signif_df,
              aes(x = YEAR, y = y_pos, label = "*"),
              inherit.aes = FALSE, size = 8,
              vjust = 0.75,
              color = "black")

  return(p)
}

compute_bin_size <- function(max_size, target_bins = 10) {
  # Handle invalid input
  if (is.null(max_size) || is.na(max_size) || max_size <= 0) {
    return(5)
  }

  # Define the only valid bin sizes
  possible_bin_sizes <- c(2, 5, 10)

  # Calculate how many bins each possible size would create
  num_bins <- max_size / possible_bin_sizes

  # Find which bin size gets us closest to the target number of bins
  closest_match_index <- which.min(abs(num_bins - target_bins))

  # Return the bin size that corresponds to the best match
  return(possible_bin_sizes[closest_match_index])
}


# Length frequency for comparing inside to outside
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
    filter(YEAR %in% sort(unique(YEAR), decreasing = TRUE)[1:3])%>%
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

#Function that outputs plots for showing all relevant LF plots
#Defaults to 2022/2024
render_LF_plots <- function(df, SPECIES_CD, COMNAME, max_size = NULL, yrs = c(2022, 2024), target_bins = 8) {

  # ---- Compute  bin size ----
  bin_size <- if (!is.null(manual_bin) && SPECIES_CD %in% names(manual_bin)) {
    manual_bin[[SPECIES_CD]]   # use manual bin from vector
  } else {
    compute_bin_size(max_size, target_bins)  # dynamic default
  }

  # Individual year plots. Auto-generates more plots if more years are added
  panels <- lapply(yrs, function(year) {
    MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = year, spp_name = COMNAME)
  })

  # Combined category plots
  p1 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
                  spp_name = COMNAME, category = "M:IR",
                  custom_title = paste(COMNAME, "- M:IR"))

  p2 <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
                  spp_name = COMNAME, category = "open",
                  custom_title = paste(COMNAME, "- Open"))

  #panel of combined plots
  n_per_row <- 2
  n_needed <- n_per_row - (length(panels) %% n_per_row)
  if (n_needed != n_per_row) {
    panels <- c(panels, replicate(n_needed, patchwork::plot_spacer(), simplify = FALSE))
  }
  # Split panels into rows for the top section
  row_panels <- split(panels, ceiling(seq_along(panels) / n_per_row))
  combined_rows <- lapply(row_panels, function(row) Reduce(`|`, row))
  combined_years <- Reduce(`/`, combined_rows)

  # Bottom row stays as is (2 plots)
  bottom_row <- p1 | p2

  # Combine top and bottom, without padding bottom row
  final_plot <- combined_years / bottom_row


  return(invisible(final_plot))
}

#Manually adjust the bin size based on species code
manual_bin <- c("HAE FLAV" = 5, "CEP CRUE" = 5, "CAL CALA" = 5, "CAL NODO" = 5)



