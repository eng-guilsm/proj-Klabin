library(pdftools)

pdf_path <- file.path(
  dirname(getwd()),
  "Klabin KLBN11 EquityResearch 2025.pdf"
)

cat("Reading PDF from:", pdf_path, "\n")
txt <- pdf_text(pdf_path)
cat("Pages extracted:", length(txt), "\n")

out_path <- file.path(getwd(), "pdf_extracted.txt")
writeLines(txt, out_path)
cat("Saved to:", out_path, "\n")
