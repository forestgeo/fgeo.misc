Issue 21
================

> we would like to have the “Team” column from the root worksheet added
> to each stem similar to the date.

> Also, Harvard Forest would like to use the R code as well. Their data
> includes a “Sub\_Quadrat” column on the root sheet of the workbook
> that currently doesn’t populate like the “Team” column when running
> the code.

> I realize there will be little differences from site to site like
> this, so not sure how to avoid these issues going forward.

``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(fs)
  
  # install.packages("fgeo.tool")
  library(fgeo.tool)
  # devtools::install_github("forestgeo/fgeo.misc")
  library(fgeo.misc)
})

packageVersion("fgeo.tool")
#> [1] '1.2.5'
packageVersion("fgeo.misc")
#> [1] '0.0.0.9003'
```

> I would like to export the files separately to complete the vetting
> process … However, trying the xlff\_to\_xl function is writing the
> files as .NA instead of .xlsx.

Thanks\! The file extensions should now be correct. The problem is a bug
(I think) in the fs package (see
<https://github.com/r-lib/fs/issues/205>).

(FYI, notice that the file extension is something you can change
manually. Just rename the file to give it the correct extension; then
the program associated to that extension should know how to open it.)

``` r
input <- here(".buildignore/input")
dir_ls(input)
#> /home/mauro/fgeo.misc/.buildignore/input/15106_13112019-05-02_AS_t.xlsx
#> /home/mauro/fgeo.misc/.buildignore/input/15712019-05-10MS_t.xlsx

output <- here(".buildignore/issue_21/output")
xlff_to_csv(input, output, first_census = FALSE)
#> Warning: Unknown or uninitialised column: 'new_stem'.
#> Warning: Length of logical index must be 1 or 64, not 0
#> Warning: `new_secondary_stems` has cero rows.
#> Warning: Filling every cero-row dataframe with NAs (new_secondary_stems).
#> Warning: Unknown or uninitialised column: 'new_stem'.
#> Warning: Length of logical index must be 1 or 33, not 0
#> Warning: `new_secondary_stems` has cero rows.
#> Warning: Filling every cero-row dataframe with NAs (new_secondary_stems).

dir_ls(output)
#> /home/mauro/fgeo.misc/.buildignore/issue_21/output/15106_13112019-05-02_AS_t.csv
#> /home/mauro/fgeo.misc/.buildignore/issue_21/output/15712019-05-10MS_t.csv
#> /home/mauro/fgeo.misc/.buildignore/issue_21/output/single_dataframe.csv
```

> Can I use xlff\_to\_xl to write multiple files to one location?

Yes, as shown above.

> Also, we would like to have the “Team” column from the root worksheet
> added to each stem similar to the date.

Okay (see <https://github.com/forestgeo/fgeo.misc/issues/22>)
