library(RCurl)
library(XML)
library(labelled)
library(readxl)

# Working directory
setwd("~/Projects/LaSource/De Goumoëns - SAFIR/")

# Import REDCap data with the API (RCurl)
f <- "/mnt/ramdisk/lasource_safir.rda"
if (file.exists(f)) {
  load(f)
} else {
  uri   <- "https://lren.chuv.ch/redcap/api/"
  z <- "misc/redcap_safir.token"
  token <- readChar(z, file.info(z)$size - 1)
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
  save(safir, safir_metadata, file = f)
  rm(token, uri, z)
}
rm(f)

# Copy data
dta <- safir

# Remove ICLS participants
dta <- dta[!grepl("^ICLS", dta$record_id), ]

# Remove empty variables
dta <- dta[apply(!is.na(dta), 2, any)]

# Dates
dta[dta$date_de_naissance == "10.04.40", "date_de_naissance"] <- "10.04.1940"
dta[dta$date_screening == "03.12.19", "date_screening"] <- "03.12.2019"
dta[dta$date_rencontre_pr_tude == "04.12.19", "date_rencontre_pr_tude"] <-
  "04.12.2019"
for (v in grep("^date", names(dta), value = TRUE)) {
 dta[[v]] <- as.Date(dta[[v]], "%d.%m.%Y")
}

# Recoding of interview length
dta$temps_d_entretien <- trimws(dta$temps_d_entretien)
dta$temps_d_entretien[!grepl("^[0-9]+$", dta$temps_d_entretien)] <- NA
dta$temps_d_entretien <- as.numeric(dta$temps_d_entretien)

# Character variables to keep
V0 <- c("record_id", "redcap_event_name", "code_d_identificationpt",
        "profession", "code_d_identificationfm", "profession_fm")
V1 <- names(dta)[sapply(dta, class) == "character"]
(V1 <- V1[!(V1 %in% V0)])
dta <- dta[!(names(dta) %in% V1)]
rm(V0, V1)

# Delete phone numbers
dta <- dta[!grepl("num_ro_de_t_l_phone", names(dta))]

# Patient id
pid <- unique(na.omit(cbind(dta["record_id"], patient_id = as.numeric(sub(
  "^FAM([0-9]+)(PT|FM).+$", "\\1", dta$code_d_identificationpt)))))
dta <- merge(dta, pid, by = "record_id", all.x = TRUE)
dta <- dta[c(1, ncol(dta), 2:(ncol(dta) - 1))]
rm(pid)

# Phase
names(dta)[names(dta) == "redcap_event_name"] <- "phase"
dta$phase <- c(screening_et_1re_r_arm_1 = "bl", phase_1_arm_1 = "ph1",
                 phase_2_arm_1 = "ph2", phase_3_arm_1 = "ph3",
                 followup_arm_1 = "fu")[dta$phase]

# Type of variable (patient or family_member)
v <- as.data.frame(read_xlsx("documents/Copie de variable_list_VdG.xlsx"))
v <- v[c("field_name", "patient_variable", "family_variable")]
v$var_type <- as.vector(as.matrix(!is.na(v[2:3])) %*% matrix(1:2, ncol = 1))
v$var_type <- factor(v$var_type, 1:2, c("patient", "family_member"))
v <- v[c("field_name", "var_type")]
var_type <- merge(data.frame(field_name = names(dta)), v,
                  by = "field_name", all = TRUE)
rm(v)

# Correction : atteinte fonctionnelle d1
#dta[c("record_id", "patient_id",  "phase", "atteinte_fonctionnelle")]
dta[dta$record_id == 2 & dta$phase == "bl", "atteinte_fonctionnelle"] <- 3

# Correction et remplissage : atteinte fonctionnelle d10
#dta[c("record_id", "patient_id",  "phase", "atteinte_fonctionnelle_d10")]
dta[dta$record_id == 2 & dta$phase == "bl", "atteinte_fonctionnelle_d10"] <- 3
dta[dta$record_id == 7 & dta$phase == "bl", "atteinte_fonctionnelle_d10"] <- 3

# Remplissage : atteinte fonctionnelle d30
#dta[c("record_id", "patient_id",  "phase", "atteinte_fonctionnelle_d30")]
dta[dta$record_id == 7 & dta$phase == "bl", "atteinte_fonctionnelle_d30"] <- 2

# Correction : date de transfert
#dta[c("record_id", "patient_id",  "phase", "length_of_stay_d")]
dta[dta$record_id == 4 & dta$phase == "ph1", "date_de_transfert"] <-
  "2020-01-07"

# Remplissage : date d'hospitalisation
dta[dta$record_id == 1 & dta$phase == "ph1", "date_d_hospitalisation"] <-
  dta[dta$record_id == 1 & dta$phase == "bl", "date_d_hospitalisation"]
dta[dta$record_id == 7 & dta$phase == "ph1", "date_d_hospitalisation"] <-
  dta[dta$record_id == 7 & dta$phase == "bl", "date_d_hospitalisation"]

# New variables
dta$age <- as.numeric(dta$date_d_hospitalisation - dta$date_de_naissance)
dta$age <- dta$age / 365.2425
dta$age_fm <- as.numeric(dta$date_d_hospitalisation - dta$date_de_naissance_fm)
dta$age_fm <- dta$age_fm / 365.2425
dta$age_cat <- cut(dta$age, c(0, 55, 70, Inf), c("<55", "55-69", ">=70"),
                   right = FALSE)
dta$age_fm_cat <- cut(dta$age_fm, c(0, 55, 70, Inf), c("<55", "55-69", ">=70"),
                      right = FALSE)
dta$length_of_stay_d <-
  as.numeric(dta$date_de_transfert - dta$date_d_hospitalisation)

# Profession
dta$profession_ <- NA
dta[dta$phase == "bl", "profession_"] <- "Retraité(e)"
dta$profession_ <- factor(dta$profession_)
dta$profession_fm_ <- ifelse(dta$profession_fm == "", NA, ifelse(
  grepl("retrait", tolower(dta$profession_fm)), "Retraité(e)", "Actif(ve)"))

# Patient variables
var_pt <- var_type[var_type$var_type %in% "patient", "field_name"]
var_pt <- c(var_pt, "profession_", "age", "age_cat", "length_of_stay_d")

# Patient variables which have several different values per timepoint
v <- sapply(var_pt, function(v) {
  fml <- as.formula(paste(v, "~ patient_id + phase"))
  any(aggregate(fml, dta, function(z) length(unique(z)) > 1)[[3]])
})
(v <- names(v)[v])
var_pt_0 <- var_pt
var_pt <- var_pt[!(var_pt %in% v)]
var_pt_mv <- v
rm(v, var_pt_0)

# Patient database
Merge <- function(x, y) merge(x, y, by = c("patient_id", "phase"), all = TRUE)
dta_pt <- Reduce(Merge, lapply(var_pt, function(v) {
  fml <- as.formula(paste(v, "~ patient_id + phase"))
  aggregate(fml, dta, unique)
}))
dta_pt$phase <- factor(dta_pt$phase, c("bl", "ph1", "ph2", "ph3", "fu"))
dta_pt <- dta_pt[order(dta_pt$patient_id, dta_pt$phase), ]
rm(Merge)

# Composition de la famille
comp <- aggregate(record_id ~ patient_id + phase, dta,
                  function(z) length(unique(z)))
names(comp)[3] <- "composition"
dta_pt <- merge(dta_pt, comp, by = c("patient_id", "phase"), sort = FALSE)
rm(comp)

# Family member variables
var_fm <- var_type[var_type$var_type %in% "family_member", "field_name"]
var_fm <- c(var_fm, "age_fm", "age_fm_cat", "profession_fm_")

# Family members database
dta_fm <- dta[c("record_id", "patient_id", "phase", var_fm)]
dta_fm$phase <- factor(dta_fm$phase, c("bl", "ph1", "ph2", "ph3", "fu"))
dta_fm <- dta_fm[order(dta_fm$patient_id, dta_fm$phase), ]

# Codebook
cb <- safir_metadata[c("field_name", "select_choices_or_calculations")]
cb <- cb[cb$select_choices_or_calculations != "", ]
cb <- do.call(rbind, lapply(1:nrow(cb), function(i) {
  z <- strsplit(cb[i, "select_choices_or_calculations"], " \\| ")[[1]]
  z <- strsplit(z, ", ")
  val <- as.numeric(sapply(z, function(x) x[[1]]))
  lab <- sapply(z, function(x) x[[2]])
  data.frame(variable = cb[i, "field_name"], value = val, label = lab)
}))

# Add labels / binary variables / xxx_complete variables
for (u in c("dta_pt", "dta_fm")) {
  d <- get(u)
  V <- unique(cb$variable)
  V <- V[V %in% names(d)]
  for (v in V) {
    i <- cb$variable == v
    d[[v]] <- labelled(d[[v]], setNames(cb[i, "value"], cb[i, "label"]))
  }
  for (v in names(d)[sapply(d, class) %in% c("numeric", "integer")]) {
    if (all(is.na(d[[v]]) | d[[v]] %in% 0:1)) d[[v]] <- as.logical(d[[v]])
  }
  l <- c("Incomplete" = 0, "Unverified" = 1, "Complete" = 2)
  for (v in grep("_complete$", names(d), value = TRUE)) {
    d[[v]] <- labelled(d[[v]], l)
  }
  assign(u, d)

}
rm(d, i, l, u, V, v)

# Scores: BriefCOPE
L = list(
  self_distraction = c(1, 19),
  active_coping = c(2, 7),
  denial = c(3, 8),
  substance_use = c(4, 11),
  emotional_support = c(5, 15),
  instrumental_support = c(10, 23),
  disengagement = c(6, 16),
  venting = c(9, 21),
  reframing = c(12, 17),
  planning = c(14, 25),
  humor = c(18, 28),
  acceptance = c(20, 24),
  religion = c(22, 27),
  self_blame = c(13, 26)
)
en_to_fr <- c(1, 20, 3, 4, 5, 6, 2, 21, 9, 10, 22, 11, 12, 13, 14,
              15, 26, 16, 17, 8, 18, 7, 19, 23, 24, 25, 27, 28)
L <- lapply(L, function(z) en_to_fr[z])
for(v in names(L)) {
  x <- paste0("cope", L[[v]])
  dta_fm[[paste0("cope_", v)]] <- apply(dta_fm[x], 1, sum)
}
L = list(
  avoidant_coping = c("denial", "substance_use", "venting", "disengagement",
                      "self_distraction", "self_blame"),
  approach_coping = c("active_coping", "reframing", "planning", "acceptance",
                      "emotional_support", "instrumental_support")
)
for (v in names(L)) {
  x <- paste0("cope_", L[[v]])
  dta_fm[[v]] <- apply(dta_fm[x], 1, sum)
}
rm(L, v, x)


# Scores: Family functionning
# !!! ffcoll 5 items (not 4) !!!
dta_fm <- within(dta_fm, {
  ff_expressing_emotions =
    apply(dta_fm[grep("^ffemo_[1-4]$", names(dta_fm))], 1, sum)
  ff_collabo = apply(dta_fm[grep("^ffcoll_[1-5]$", names(dta_fm))], 1, sum)
  ff_comm = apply(dta_fm[grep("^ffcomm_[1-4]$", names(dta_fm))], 1, sum)
  ff_behav = apply(dta_fm[grep("^ffcomp_[1-4]$", names(dta_fm))], 1, sum)
})
dta_fm$ff_total <-
  with(dta_fm, ff_expressing_emotions + ff_collabo + ff_comm + ff_behav)

# Scores: Perceived support
dta_fm <- within(dta_fm, {
  ps_cog = apply(dta_fm[grep("^cog_[1-5]$", names(dta_fm))], 1, sum)
  ps_emo = apply(dta_fm[grep("^emo_[1-9]$", names(dta_fm))], 1, sum)
})
dta_fm$ps_total <-
  with(dta_fm, ps_cog + ps_emo)
