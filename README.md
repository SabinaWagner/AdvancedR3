Notes and exercises for the Advanced R3 course

# AdvancedR3:

This project contains notes and exercises for the advanced r3 course

# Brief description of folder and file contents

The following folders contain:

-   `data/`: cvs data files used in the exercises
-   `docs/`: quarto documents for notes, exercises and report
-   `R/`: finished R functions for exercises

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("pak")
pak::pak()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
