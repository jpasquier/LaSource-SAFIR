library(RCurl)
library(XML)
library(ggplot2)
library(labelled)
library(parallel)
library(readxl)
library(writexl)

options(mc.cores = detectCores() - 1)

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

# --------------------------------------------------------------------------- #

#   safir <- read.csv(text = postForm(
#     uri = uri,
#     token = token,
#     content = 'record',
#     format = 'csv',
#     type='eav'
#   ))
#
# # Checkboxes
# ckbx <- safir_metadata[safir_metadata$field_type == "checkbox", "field_name"]
# i <- safir$field_name %in% ckbx
# safir[i, "field_name"] <- paste(safir[i, "field_name"], safir[i, "value"],
#                                 sep = "___")
# safir[i, "value"] <- 1
# rm(i)
#
# # Flatten data
# Z <- c(bl = "screening_et_1re_r_arm_1", ph1 = "phase_1_arm_1",
#        ph2 = "phase_2_arm_1", ph3 = "phase_3_arm_1",
#        fu = "followup_arm_1")
# dta <- lapply(Z, function(z) {
#   d <- safir[safir$event_id == z, c("record", "field_name", "value")]
#   d <- pivot_wider(d, names_from = field_name, values_from = value)
#   for(v in names(d)[sapply(d, class) == "character"]) {
#     x <- d[[v]]
#     if (all(is.na(x) | grepl("^[0-9]+$", x))) d[[v]] <- as.integer(x)
#   }
#   # Remove ICLS participants
#   d <- d[!grepl("^ICLS", d$record_id), ]
#   # Dates
#   v <- "date_de_naissance"
#   if (any(names(d) == v)) d[d[[v]] %in% "10.04.40", v] <- "10.04.1940"
#   v <- "date_screening"
#   if (any(names(d) == v)) d[d[[v]] %in% "03.12.19", v] <- "03.12.2019"
#   v <- "date_rencontre_pr_tude"
#   if (any(names(d) == v)) d[d[[v]] %in% "04.12.19", v] <- "04.12.2019"
#   for (v in grep("^date", names(d), value = TRUE)) {
#     d[[v]] <- as.Date(d[[v]], "%d.%m.%Y")
#   }
#   # Recoding of interview length
#   if (any(names(d) == "temps_d_entretien")) {
#     d$temps_d_entretien <- trimws(d$temps_d_entretien)
#     d$temps_d_entretien[!grepl("^[0-9]+$", d$temps_d_entretien)] <- NA
#     d$temps_d_entretien <- as.numeric(d$temps_d_entretien)
#   }
#   d
# })

# --------------------------------------------------------------------------- #

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
        "code_d_identificationfm")
V1 <- names(dta)[sapply(dta, class) == "character"]
(V1 <- V1[!(V1 %in% V0)])
dta <- dta[!(names(dta) %in% V1)]
rm(V0, V1)

# Delete phone numbers
dta <- dta[!grepl("num_ro_de_t_l_phone", names(dta))]

# Constant variables
#names(dta)[sapply(dta, function(z) length(unique(z[!is.na(z)])) == 1)]

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

# Patient variables
var_pt <- var_type[var_type$var_type %in% "patient", "field_name"]
var_pt <- c(var_pt, "age", "age_cat", "length_of_stay_d")

# Patient variables which have several different values per timepoint
v <- var_type[var_type$var_type %in% "patient", "field_name"]
v <- sapply(v, function(v) {
  fml <- as.formula(paste(v, "~ patient_id + phase"))
  any(aggregate(fml, dta, function(z) length(unique(z)) > 1)[[3]])
})
(v <- names(v)[v])
var_pt_0 <- var_pt
var_pt <- var_pt[!(var_pt %in% v)]
var_pt_mv <- v
rm(v)

# Patient database
Merge <- function(x, y) merge(x, y, by = c("patient_id", "phase"), all = TRUE)
dta_pt <- Reduce(Merge, lapply(var_pt, function(v) {
  fml <- as.formula(paste(v, "~ patient_id + phase"))
    aggregate(fml, dta, unique)
}))
dta_pt$phase <- factor(dta_pt$phase, c("bl", "ph1", "ph2", "ph3", "fu"))
dta_pt <- dta_pt[order(dta_pt$patient_id, dta_pt$phase), ]

# Family member variables
#fm_vars <- grep("^(cog|emo|ff(emo|coll|comm|comp))_[0-9]+$",
#                names(dta), value = TRUE)
#fm_vars <- c("genre_fm", "niveau_d_tude_fm", "lien_avec_le_patient",
#             "habite_avec_le_patient", paste0("cope", 1:28), fm_vars)
var_fm <- var_type[var_type$var_type %in% "family_member", "field_name"]
var_fm <- c(var_fm, "age_fm", "age_fm_cat")

# Family members database
dta_fm <- dta[c("record_id", "patient_id", "phase", var_fm)]
dta_fm$phase <- factor(dta_fm$phase, c("bl", "ph1", "ph2", "ph3", "fu"))
dta_fm <- dta_fm[order(dta_fm$patient_id, dta_fm$phase), ]

# # List of variables
# vtbl <- do.call(rbind, lapply(names(dta), function(v) {
#   P <- c("bl", "ph1", "ph2", "ph3", "fu")
#   r <- sapply(P, function(p) any(!is.na(dta[dta$phase == p, v])))
#   # max number of different value per record_id
#   if (v != "record_id") {
#     fml <- as.formula(paste(v, "~ record_id"))
#     n <- aggregate(fml, dta, function(z) length(unique(z)))
#     n <- max(n[[2]])
#   } else {
#     n <- 1
#   }
#   cbind(data.frame(variable = v), t(r), nval_max = n)
# }))
# # Baseline variables
# (vbl <- vtbl[vtbl$bl & vtbl$nval_max == 1, ])
# vbl <- vbl$variable
# vbl <- vbl[vbl != "atteinte_fonctionnelle_d30"]
#
# # Baseline dataframe
# bl <- dta[dta$phase == "bl", vbl]
#
# # Longitudinal dataframe
# lg <- dta[!(names(dta) %in% vbl[!(vbl %in% c("record_id", "patient_id"))])]
# rm(dta, vbl)

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
for(v in names(L)) {
  x <- paste0("cope", L[[v]])
  dta_fm[[paste0("cope_", v)]] <- apply(dta_fm[x], 1, mean)
}

# Scores: Family functionning
# !!! ffcoll 5 items (not 4) !!!
ff_expressing_emotions <-
  apply(dta_fm[grep("^ffemo_[1-4]$", names(dta_fm))], 1, mean)
ff_collabo <- apply(dta_fm[grep("^ffcoll_[1-5]$", names(dta_fm))], 1, mean)
ff_comm <- apply(dta_fm[grep("^ffcomm_[1-4]$", names(dta_fm))], 1, mean)
ff_behav <- apply(dta_fm[grep("^ffcomp_[1-4]$", names(dta_fm))], 1, mean)

# Scores: Perceived support
ps_cog <- apply(dta_fm[grep("^cog_[1-5]$", names(dta_fm))], 1, mean)
ps_emo <- apply(dta_fm[grep("^emo_[1-9]$", names(dta_fm))], 1, mean)

# Descriptive analyses
figs <- list(cat = list(), num = list())
figs <- list(pt = figs, fm = figs) 
tabs <- lapply(c(pt = "pt", fm = "fm"), function(s) {
  d <- paste0("dta_", s)
  X <- names(get(d))
  X <- X[!(X %in% c("record_id", "patient_id", "phase"))]
  W <- list(c("factor", "logical", "haven_labelled"), c("integer", "numeric"))
  X_cat <- X[sapply(get(d)[X], function(z) any(class(z) %in% W[[1]]))]
  X_num <- X[sapply(get(d)[X], function(z) any(class(z) %in% W[[2]]))]
  X_num <- X_num[!(X_num %in% X_cat)]
  fsttl <- c(pt = "patients", fm = "family members")[s]
  list(
    cat = do.call(rbind, lapply(X_cat, function(x) {
      dta_x <- na.omit(get(d)[c("phase", x)])
      dta_x$phase <- droplevels(dta_x$phase)
      if (any(class(dta_x[[x]]) == "haven_labelled")) {
        dta_x[[x]] <- to_factor(dta_x[[x]])
      }
      if (nrow(dta_x) == 0) {
        return(NULL)
      }
      if (any(class(dta_x[[x]]) == "factor")) {
        dta_x[[x]] <- droplevels(dta_x[[x]])
        lvls <- levels(dta_x)
      } else {
        lvls <- NULL
      }
      tab <- table(dta_x$phase, dta_x[[x]])
      tab <- merge(as.data.frame(tab), as.data.frame(prop.table(tab, 1)),
                   by = c("Var1", "Var2"), all = TRUE, sort = FALSE)
      colnames(tab) <- c("phase", "value", "n", "prop")
      if (!is.null(lvls)) tab$value <- factor(tab$value, lvls)
      tab <- tab[order(tab$phase, tab$value), ]
      rotate <- any(nchar(as.character(tab$value)) > 8)
      fig <- ggplot(tab, aes(x = value, y = prop)) +
        geom_bar(stat="identity", fill = "steelblue") +
        scale_y_continuous(labels=scales::percent) +
        labs(x = x, y = "Fréquences relatives", title = x, subtitle = fsttl) +
        facet_grid(cols = vars(phase)) +
        theme_classic()
      if (rotate) {
        fig <- fig +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      figs[[s]]$cat <<- append(figs[[s]]$cat, setNames(list(fig), x))
      cbind(variable = x, tab)
    })),
    num = do.call(rbind, lapply(X_num, function(x) {
      dta_x <- na.omit(get(d)[c("phase", x)])
      dta_x$phase <- droplevels(dta_x$phase)
      if (nrow(dta_x) == 0) {
        return(NULL)
      }
      dta_x[[x]] <- as.numeric(dta_x[[x]])
      fig <- ggplot(dta_x, aes_string(x = "phase", y = x)) +
        geom_boxplot() +
        labs(x = "", title = x, subtitle = fsttl) +
        theme_classic()
      figs[[s]]$num <<- append(figs[[s]]$num, setNames(list(fig), x))
      nobs <- length
      q25 <- function(z) quantile(z, .25)
      q75 <- function(z) quantile(z, .75)
      fcts <- c("nobs", "mean", "sd", "min", "q25", "median", "q75", "max")
      Merge <- function(u, v) merge(u, v, by = "phase")
      fml <- as.formula(paste(x, "~phase"))
      tab <- Reduce(Merge, lapply(fcts, function(fct) {
        z <- aggregate(fml, dta_x, get(fct))
        names(z)[2] <- fct
        z
      }))
      cbind(variable = x, tab)
    }))
  )
})

# Export results
outdir <- paste0("results/analyses_descriptives_",
                 format(Sys.Date(), "%Y%m%d"))
if (!dir.exists(outdir)) dir.create(outdir)
L <- do.call(append, lapply(names(tabs), function(u) {
  z <- tabs[[u]]
  names(z) <- paste(u, names(z), sep = "_")
  z
}))
L <-L[!(sapply(L, is.null))]
L <- append(L, list(var_pt_mv = data.frame(variable = var_pt_mv)))
write_xlsx(L, file.path(outdir, "tables.xlsx"))
d <- file.path(outdir, "figures")
if (!dir.exists(d)) dir.create(d)
for(z in figs) {
  for (w in z) {
    for (v in names(w)) {
      tiff(file.path(d, paste0("fig_", v, ".tiff")), width = 2400,
           height = 1800, res = 480, compression = "zip")
      print(w[[v]])
      dev.off()
    }
  }
}
rm(L, d, z, w, v)

