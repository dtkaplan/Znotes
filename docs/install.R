# Installation script for *MOSAIC Calculus* software

cat("Installing MOSAIC Calculus software.")

install.packages("remotes")
remotes::install_github("dtkaplan/Zcalc")
remotes::install_github("dtkaplan/Znotes")



# install.packages(c("bookdown","bslib", "downlit", "here", "DT", "kableExtra"))


# Still need to install shell scripts for Sandbox and Drill
cat("Local Sandbox and Drill apps not yet installed.")
