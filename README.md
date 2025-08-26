
<img src="docs/MIR.png" style="display: block; margin: auto;"
width="300" />

# M:IR RVC Analysis

### The Data

Sites at MIR locations, “inside”, were compared to sites in the rest of
the Florida Keys. Since MIR sites only comprised of stratum FK01-FK05,
FK06-FK09 were removed from the “outside” data set. (Files containing the data 
used are inside of the "data" folder)

### Documents

The folder labeled "docs" contains "Index.qmd", a quarto document, which is used to generate both the 
HTML and the PDF outputs. 
The sub-folder "code" contains .R files for functions used in the analysis.


### Editing the .qmd

After making edits to the .qmd, be sure to render the PDF before rendering the 
HTML so that the PDF button on the HTML page contains any updates made to the file.


### Change species displayed in the document

To add more species to the document, insert the species code as seen in line 128 of index.qmd. 
Ensure that the species code is capitalized and in quotation marks and propper syntax of 
commas is used. After adding the species code here, all plots and table will be 
reproduced on render with the new species.

```
target_species <- c(
  "HAE FLAV", "SPA VIRI", "SCA GUAC", "STE PLAN",
  "CAL CALA", "CAL NODO", "EPI MORI")
```

### Customizing length frequency bins

Length frequency plot bins are automatically created using a function that determines 
the ideal bin size based on a species hypothetical max size. For select species, 
the automatic bin size may not be suitible. 
To manually adjust the bin size, open MIR_Plot_functions.R and look to line 328. 
```
manual_bin <- c("STE PLAN" = 2, "EPI MORI" = 5)
```
Customize the bin size following the above logic.

### Merging Species
In some cases, it may be desirable to view the density, occurrence, and relative 
length frequency of multiple species merged together. In this case, instead of using 
MIR_data and spp_list when calling functions in index.qmd, MIR_data_merged and 
spp_list_merged are used. Merged species and spp_list_merged are created in line 
152 of index.qmd using the following syntax.

```
MIR_data_merged <- MIR_data
MIR_data_merged$sample_data <- MIR_data$sample_data %>%
  mutate(SPECIES_CD = ifelse(SPECIES_CD %in% c("CAL CALA", "CAL NODO"), "CAL CALA", SPECIES_CD))
spp_list_merged <- spp_list %>%
  mutate(
    COMNAME = ifelse(SPECIES_CD %in% c("CAL CALA", "CAL NODO"), "Porgy", COMNAME),
    SPECIES_CD = ifelse(SPECIES_CD %in% c("CAL CALA", "CAL NODO"), "CAL CALA", SPECIES_CD),
    SCINAME = ifelse(SPECIES_CD == "CAL CALA", "Calamus spp.", SCINAME) 
  ) %>%
  distinct(SPECIES_CD, COMNAME, SCINAME, .keep_all = TRUE)
```
Repeat above with respective SPECIES_CD for merging species data.

### Adding Species Photos

Species photos are added to the species table from the folder “species_photos” 
in the “docs” folder. To add a species photo, place it in the folder with the 
following syntax “SPECIES_CD.png”.  If the species was imputed into 
target_species, the photo will appear automatically in the table. 



------------------------------------------------------------------------


