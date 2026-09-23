# Check dangling blobs in git
blobs <- system("git fsck --lost-found", intern = TRUE)
blob_ids <- sub("dangling blob ", "", grep("dangling blob", blobs, value = TRUE))

cat("Found", length(blob_ids), "dangling blobs.\n")

for (id in blob_ids) {
  content <- system(paste("git cat-file -p", id), intern = TRUE)
  if (any(grepl("Wolverine|Project Wolverine|CAS PCPA|Master Implementation", content))) {
    cat(">>> FOUND Code.R blob:", id, "with", length(content), "lines!\n")
    # Write to a recovery file
    writeLines(content, paste0("scratch/recovered_Code_", substr(id, 1, 8), ".R"))
  }
}
