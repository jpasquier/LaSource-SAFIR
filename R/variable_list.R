library(RCurl)
library(XML)
library(writexl)

# Working directory
setwd("~/Projects/LaSource/De Goumoëns - SAFIR/")

# ------------------------------- REDCap data ------------------------------- #

# API
uri   <- "https://lren.chuv.ch/redcap/api/"

# Token
z <- "misc/redcap_safir.token"
token <- readChar(z, file.info(z)$size - 1)
rm(z)

# Import data with the API (RCurl)
safir <- read.csv(text = postForm(
  uri = uri,
  token = token,
  content = 'record',
  format = 'csv'
))
safir_metadata <- xmlToDataFrame(postForm(
  uri = uri,
  token = token,
  content = 'metadata',
  format = 'xml'
))

# ---------------------------- List of variables ---------------------------- #

# Phase
safir$phase <- c(screening_et_1re_r_arm_1 = "bl", phase_1_arm_1 = "ph1",
                 phase_2_arm_1 = "ph2", phase_3_arm_1 = "ph3",
                 followup_arm_1 = "fu")[safir$redcap_event_name]

# List of variables
vtbl <- do.call(rbind, lapply(names(safir), function(v) {
  if (v == "phase") return(NULL)
  P <- c("bl", "ph1", "ph2", "ph3", "fu")
  r <- sapply(P, function(p) any(!is.na(safir[safir$phase == p, v])))
  # max number of different value per patient
  if (!any(r)) {
    n <- NA
  } else if (v != "record_id") {
    fml <- as.formula(paste(v, "~ record_id"))
    n <- aggregate(fml, safir, function(z) length(unique(z)))
    n <- max(n[[2]])
  } else {
    n <- 1
  }
  cbind(data.frame(field_name = v), t(r), nval_max = n)
}))

# Merge with metadata
vtbl$dummy_index <- 1:nrow(vtbl)
V <- c("field_name", "form_name", "field_type", "field_label")
vtbl <- merge(vtbl, safir_metadata[V], by = "field_name", all.x = TRUE)
vtbl <- vtbl[order(vtbl$dummy_index), names(vtbl) != "dummy_index"]
rm(V)

# Export list of variables
write_xlsx(vtbl, "results/variable_list.xlsx")
