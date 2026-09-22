files <- list.files("A:/casact/pcpa-2026", recursive = TRUE, full.names = FALSE)
r_files <- grep("\\.(R|Rmd|qmd)$", files, value = TRUE, ignore.case = TRUE)
print(r_files)
