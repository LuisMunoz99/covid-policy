# Authors: LM
# Maintainers: LM
# Date: 1/20/24
# =========================================

# --- libs --- 
if (!require(pacman)) install.packages("pacman")
p_load(googledrive)

args <- list(input = "https://docs.google.com/document/d/1IQgkub7HU-VZeCIAb8Vn51d9Q7lxHzN2/edit?usp=sharing&ouid=110086971077082392812&rtpof=true&sd=true", 
             output = here::here("method/output/methodological_appen.docx"))

# --- Import data --- 
doc <- drive_get(as_id(args$input))

# --- Export ---
drive_download(doc, path = args$output, overwrite = TRUE)
