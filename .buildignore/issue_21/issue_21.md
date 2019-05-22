Issue 21
================

Jess,

I downloaded the data (and removed the link).

From the two issues you mention, I think only one is actionable.

  - (not actionable) `xl_to_df()` does not exist in the package. Did it
    ever existed? I suspect that what you are trying to accomplish it
    already done by some of the `xlff_to_*()` variants. I have now made
    the [help file visible on the package
    website](https://forestgeo.github.io/fgeo.misc/reference/xlff_to_output.html).
    The examples section should clarify what these functions do.

  - (actionable) I changed the code to not-fail when the column `tag`
    doesn’t exist. I’m not sure why isn’t there (although you may have
    explained)–I leave it to you to decide. Now the code produces output
    but there are a bunch of warnings that I encourage you to read and
    try to make sense of. See below:

<!-- end list -->

``` r
library(purrr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(readr)
library(fs)
library(here)
#> here() starts at C:/Users/LeporeM/Documents/Dropbox/git/fgeo.misc

# install.packages("fgeo.tool")
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
# devtools::install_github("forestgeo/fgeo.misc")
library(fgeo.misc)

packageVersion("fgeo.tool")
#> [1] '1.2.4'
packageVersion("fgeo.misc")
#> [1] '0.0.0.9002'

input <- here(".buildignore/issue_21/input")

# The input/ folder contains .xlsx file
dir_ls(input)
#> C:/Users/LeporeM/Documents/Dropbox/git/fgeo.misc/.buildignore/issue_21/input/15106_13112019-05-02_AS_t.xlsx
#> C:/Users/LeporeM/Documents/Dropbox/git/fgeo.misc/.buildignore/issue_21/input/15712019-05-10MS_t.xlsx

list_of_dataframes <- xlff_to_list(input, first_census = FALSE)
#> Warning: Unknown or uninitialised column: 'new_stem'.
#> Warning: Length of logical index must be 1 or 64, not 0
#> Warning: `new_secondary_stems` has cero rows.
#> Warning: Filling every cero-row dataframe with NAs (new_secondary_stems).
#> Warning: Unknown or uninitialised column: 'new_stem'.
#> Warning: Length of logical index must be 1 or 33, not 0
#> Warning: `new_secondary_stems` has cero rows.
#> Warning: Filling every cero-row dataframe with NAs (new_secondary_stems).
single_dataframe <- reduce(list_of_dataframes, bind_rows)

as_tibble(single_dataframe)
#> # A tibble: 113 x 24
#>    submission_id start_form_time~ end_form_time_s~ sheet section_id quadrat
#>    <chr>         <chr>            <chr>            <chr> <chr>      <chr>  
#>  1 1f11a77f-dea~ 2019-05-01 09:5~ 2019-05-10 09:1~ form~ <NA>       <NA>   
#>  2 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  3 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  4 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  5 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  6 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  7 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  8 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#>  9 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#> 10 1f11a77f-dea~ <NA>             <NA>             orig~ <NA>       1311   
#> # ... with 103 more rows, and 18 more variables: mainstem_tag <chr>,
#> #   stem_tag <chr>, spcode <chr>, px <chr>, py <chr>, dbh <chr>,
#> #   status <chr>, codes <chr>, hom <chr>, notes <chr>, dbh_2019 <chr>,
#> #   status_2019 <chr>, codes_2019 <chr>, notes_2019 <chr>,
#> #   new_secondary <chr>, scientific_name <chr>, unique_stem <chr>,
#> #   date <chr>

glimpse(single_dataframe)
#> Observations: 113
#> Variables: 24
#> $ submission_id         <chr> "1f11a77f-deae-477b-97ac-68d61399336a", ...
#> $ start_form_time_stamp <chr> "2019-05-01 09:58:07 AM", NA, NA, NA, NA...
#> $ end_form_time_stamp   <chr> "2019-05-10 09:18:52 AM", NA, NA, NA, NA...
#> $ sheet                 <chr> "form_meta_data", "original_stems", "ori...
#> $ section_id            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
#> $ quadrat               <chr> NA, "1311", "1311", "1311", "1311", "131...
#> $ mainstem_tag          <chr> NA, "130903", "132986", "130902", "13090...
#> $ stem_tag              <chr> NA, "130903", "132986", "130902", "13090...
#> $ spcode                <chr> NA, "LIST2", "TORA2", "LIST2", "ACRU", "...
#> $ px                    <chr> NA, "1.6", "1.6", "1.1", "0", "0.6", "1....
#> $ py                    <chr> NA, "0.6", "0.6", "1.3", "0.3", "2", "6....
#> $ dbh                   <chr> NA, "32.5", "2.7", "28.5", "20", "3.1", ...
#> $ status                <chr> NA, "LI", "LI", "LI", "LI", "LI", "LI", ...
#> $ codes                 <chr> NA, "A", "NA", "NA", "L", "NA", "NA", "M...
#> $ hom                   <chr> NA, "1.32", "1.3", "1.3", "1.3", "1.3", ...
#> $ notes                 <chr> NA, "NA", "on 130903", "NA", "changed fr...
#> $ dbh_2019              <chr> NA, "32.9", "3.5", "29.2", "20", "3.4", ...
#> $ status_2019           <chr> NA, "LI", "LI", "LI", "LI", "LI", "LI", ...
#> $ codes_2019            <chr> NA, NA, NA, NA, "L", NA, NA, NA, "XY", N...
#> $ notes_2019            <chr> NA, "TORAR 132986", NA, NA, NA, NA, "Epi...
#> $ new_secondary         <chr> NA, "False", "False", "False", "False", ...
#> $ scientific_name       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
#> $ unique_stem           <chr> "notag_NA", "notag_130903", "notag_13298...
#> $ date                  <chr> "2019-05-02", "2019-05-02", "2019-05-02"...

# You can now save it as you please, for example, as a .csv
output <- here(".buildignore/issue_21/output/single_dataframe.csv")
write_csv(single_dataframe, path = output)

# The output/ folder now contains the file we just saved
dir_ls(path_dir(output))
#> C:/Users/LeporeM/Documents/Dropbox/git/fgeo.misc/.buildignore/issue_21/output/single_dataframe.csv
```
