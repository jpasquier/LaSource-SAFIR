library(writexl)

# Working directory
setwd("~/Projects/LaSource/De Goumoëns - SAFIR/")

# Source preprocessing script
source("R/preprocessing.R")

# Tables 2 & 3
tbls <- lapply(1:2, function(k) {
  V <- list(
    list(
      c("age", "length_of_stay_d"),
      c("genre", "type_d_atteinte_c_r_brale", "s_v_rit_de_la_l_sion",
        paste0("atteinte_fonctionnelle", c("", "_d10", "_d30")),
        "niveau_d_tude", "profession_", "composition")
    ),
    list(
      c("age_fm"),
      c("genre_fm", "profession_fm_", "niveau_d_tude_fm",
        "lien_avec_le_patient", "habite_avec_le_patient")
    )
  )
  d <- subset(get(c("dta_pt", "dta_fm")[k]), phase == c("bl", "ph1")[k])
  rbind(
    do.call(rbind, lapply(V[[k]][[1]], function(v) {
      x <- d[[v]][!is.na(d[[v]])]
      data.frame(
        variable = v,
        n = length(x),
        value = "median/IQR",
        `median / n` = median(x),
        `IQR / %` = IQR(x),
        check.names = FALSE
      )
    })),
    do.call(rbind, lapply(V[[k]][[2]], function(v) {
      x <- d[[v]][!is.na(d[[v]])]
      x <- to_factor(x)
      n <- table(x)
      p <- prop.table(n)
      na <- rep(NA, max(0, length(n) - 1))
      data.frame(
        variable = c(v, na),
        n = c(sum(n), na),
        value = names(n),
        `median / n` = as.vector(n),
        `IQR / %` = as.vector(p),
        check.names = FALSE
      )
    }))
  )
})

# Table 4
V <- c("avoidant_coping", "approach_coping", "ff_expressing_emotions",
       "ff_collabo", "ff_comm", "ff_behav", "ff_total", "ps_cog", "ps_emo",
       "ps_total")
d <- dta_fm[apply(!is.na(dta_fm[V]), 1, any), c("record_id", "phase", V)]
d <- reshape(d, varying = V, v.names = "value", timevar = "variable",
             times = V, idvar = c("record_id", "phase"), direction = "long")
d <- reshape(d, v.names = "value", timevar = "record_id",
             idvar = c("variable", "phase"), direction = "wide")
d$variable <- factor(d$variable, V)
names(d) <- sub("^value\\.", "fm.", names(d))
d <- d[order(d$variable, d$phase),
       c("variable", "phase", sort(grep("^fm", names(d), value = TRUE)))]
d$mean <- apply(d[grep("^fm", names(d))], 1, mean, na.rm = TRUE)
d$sd <- apply(d[grep("^fm", names(d))], 1, sd, na.rm = TRUE)
rownames(d) <- NULL
tbls[[3]] <- d
rm(d, V)

# Export tables
names(tbls) <- paste0("Table", 2:4)
write_xlsx(tbls,
           paste0("results/tables_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

# sessionInfo
sink(paste0("results/sessinInfo_", format(Sys.Date(), "%Y%m%d"), ".txt"))
print(sessionInfo(), locale = FALSE)
sink()
