
<img src="docs/MIR.png" style="display: block; margin: auto;"
width="300" />

# M:IR RVC Analysis

## Instructions for use

### Change species displayed in the document
1. Open the file named "Index.qmd"
2. Scroll to line 156 and find the object named "target species"
3. Add the desired list of species codes using the existing format
  3a. The species code is the first three letters of the genus and species names 
    (e.g., EPI ITAJ for Epinephelus itajara, the goliath grouper) 
4. Ensure that each species code is surrounded by quotation marks and each 
  species is separated separated by commas. 
5. Click render to HTML or PDF view the new report

### Customizing length frequency bins


###

### The Data

Sites at MIR locations, “inside”, were compared to sites in the rest of
the Florida Keys. Since MIR sites only comprised of stratum FK01-FK05,
FK06-FK09 were removed from the “outside” data set. (Files containing the data 
used are inside of the "data" folder)

## Documents

The folder labeled "docs" contains "Index.qmd", a quarto document, which is used to generate both the 
HTML and the PDF outputs. 
The sub-folder "code" contains .R files for functions used in the analysis.


## Editing the .qmd

After making edits to the .qmd, be sure to render the PDF before rendering the 
HTML so that the PDF button on the HTML page contains any updates made to the file.

------------------------------------------------------------------------


