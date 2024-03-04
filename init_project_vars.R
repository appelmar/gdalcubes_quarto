# This script updates build date and version automatically and sets environment variables
# for the quarto project.
v = as.character(packageVersion("gdalcubes"))
d = format(Sys.Date()) #as.POSIXct(Sys.time(),"UTC")

cat("GDALCUBES_VERSION: ", v, "\n",sep = "", file = "_variables.yml", append = FALSE)
cat("BUILD_DATE: ",d, "\n",sep = "",file = "_variables.yml",  append =TRUE)
