# Walmart - revision reproducible del Caso 1, Estadistica Aplicada.
# Unidad: tienda-semana. Horizonte: una semana, con observaciones hasta t-1.
# No usa test.csv ni sampleSubmission de Kaggle.

# Setup universal de rutas (compatible con RStudio, Rscript y cualquier PC)
options(stringsAsFactors = FALSE, scipen = 999, warn = 1)
set.seed(2026L)

# Detectar la carpeta del proyecto de forma dinámica
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  return(getwd())
}

# Definición automática de directorios
script_dir <- get_script_dir()
# Si el script está dentro de la carpeta 'codigo', sube un nivel para definir el proyecto
project <- if (basename(script_dir) == "codigo") dirname(script_dir) else script_dir

data_dir <- Sys.getenv("WALMART_DATA_DIR", unset = file.path(project, "datos"))
out <- Sys.getenv("WALMART_RESULTS_DIR", unset = file.path(project, "resultados"))

# Definición de subcarpetas independientes para el aislamiento estricto de binarios
out_bin <- file.path(out, "binarios")
out_img <- file.path(out, "graficos")

# Crear carpetas automáticamente si no existen
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(out_bin, recursive = TRUE, showWarnings = FALSE)
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

local_lib <- file.path(project, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
pkgs <- c("rpart", "randomForest", "gbm", "glmnet")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Instale paquetes faltantes: ", paste(missing, collapse = ", "), ". Vea LEEME.")

# Escritura explícita con codificación UTF-8 para salidas tabulares (.csv)
save_csv <- function(x, name) {
  file_path <- file.path(out, paste0(name, ".csv"))
  write.csv(x, file = file_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# Escritura explícita con conexión UTF-8 para salidas de texto plano (.txt)
save_txt <- function(text, name) {
  file_path <- file.path(out, if (grepl("\\.txt$", name)) name else paste0(name, ".txt"))
  con <- file(file_path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con)
}

save_txt(capture.output(sessionInfo()), "sessionInfo.txt")

load_csv <- function(name) {
  paths <- c(file.path(data_dir, name), file.path(data_dir, name, name))
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(paths)) stop("Falta ", name, " dentro de ", data_dir)
  read.csv(paths[1], na.strings = c("", "NA"), check.names = FALSE, encoding = "UTF-8")
}

raw <- load_csv("train.csv"); features <- load_csv("features.csv"); stores <- load_csv("stores.csv")
check_cols <- function(x, nms) if (!all(nms %in% names(x))) stop("Columnas faltantes: ", paste(setdiff(nms, names(x)), collapse = ","))
check_cols(raw, c("Store", "Dept", "Date", "Weekly_Sales", "IsHoliday"))
check_cols(features, c("Store", "Date", "IsHoliday", "Temperature", "Fuel_Price", paste0("MarkDown", 1:5)))
check_cols(stores, c("Store", "Type", "Size"))
raw$Date <- as.Date(raw$Date); features$Date <- as.Date(features$Date)
stopifnot(!anyNA(raw[c("Store", "Dept", "Date", "Weekly_Sales", "IsHoliday")]),
          all(is.finite(raw$Weekly_Sales)), !anyDuplicated(raw[c("Store", "Dept", "Date")]),
          !anyDuplicated(features[c("Store", "Date")]), !anyDuplicated(stores["Store"]))

audit <- data.frame(item = c("filas_departamento", "ventas_negativas_conservadas", "ventas_cero",
                             "duplicados_clave", "tiendas"),
                    value = c(nrow(raw), sum(raw$Weekly_Sales < 0), sum(raw$Weekly_Sales == 0), 0, nrow(stores)))
save_csv(audit, "auditoria_datos")
save_csv(data.frame(variable = names(features), missing = colSums(is.na(features)),
                    pct = 100 * colMeans(is.na(features))), "faltantes_features")

sales <- aggregate(Weekly_Sales ~ Store + Date, raw, sum)
counts <- aggregate(Dept ~ Store + Date, raw, length); names(counts)[3] <- "Departments"
save_csv(counts, "departamentos_por_tienda_semana")

holiday <- unique(raw[c("Store", "Date", "IsHoliday")])
stopifnot(!anyDuplicated(holiday[c("Store", "Date")]))
hc <- merge(holiday, features[c("Store", "Date", "IsHoliday")], by = c("Store", "Date"))
stopifnot(nrow(hc) == nrow(holiday), all(hc$IsHoliday.x == hc$IsHoliday.y))

dat <- merge(sales, features, by = c("Store", "Date"), all.x = TRUE)
dat <- merge(dat, stores, by = "Store", all.x = TRUE)
dat <- dat[order(dat$Store, dat$Date), ]; rownames(dat) <- NULL
stopifnot(nrow(dat) == nrow(sales), !anyNA(dat[c("Type", "IsHoliday")]))

date_values <- sort(unique(dat$Date)); W <- length(date_values)
stopifnot(W >= 135, all(vapply(split(dat$Date, dat$Store), function(d)
  identical(as.character(d), as.character(date_values)) && all(diff(d) == 7), logical(1))))

dat$week_idx <- match(dat$Date, date_values)
dat$time_idx <- dat$week_idx
iso <- as.integer(format(dat$Date, "%V"))
dat$sin1 <- sin(2 * pi * iso / 52.1775); dat$cos1 <- cos(2 * pi * iso / 52.1775)
dat$sin2 <- sin(4 * pi * iso / 52.1775); dat$cos2 <- cos(4 * pi * iso / 52.1775)
dat$IsHoliday <- as.numeric(dat$IsHoliday)

lag_n <- function(v, k) c(rep(NA_real_, k), head(v, -k))
roll <- function(v, k) vapply(seq_along(v), function(i) if (i <= k) NA_real_ else mean(v[(i - k):(i - 1)]), numeric(1))
external <- c("Temperature", "Fuel_Price", paste0("MarkDown", 1:5))

for (nm in c("sales_lag1", "sales_lag4", "sales_lag52", "sales_roll4", "sales_roll13",
             paste0(external, "_lag1"), paste0(external, "_missing"))) dat[[nm]] <- NA_real_

for (ii in split(seq_len(nrow(dat)), dat$Store)) {
  v <- dat$Weekly_Sales[ii]
  for (k in c(1, 4, 52)) dat[[paste0("sales_lag", k)]][ii] <- lag_n(v, k)
  for (k in c(4, 13)) dat[[paste0("sales_roll", k)]][ii] <- roll(v, k)
  for (nm in external) {
    previous <- lag_n(dat[[nm]][ii], 1)
    dat[[paste0(nm, "_lag1")]][ii] <- previous
    dat[[paste0(nm, "_missing")]][ii] <- as.integer(is.na(previous))
  }
}

base_features <- c("time_idx", "sin1", "cos1", "sin2", "cos2", "IsHoliday", "sales_lag1", "sales_lag4",
                   "sales_lag52", "sales_roll4", "sales_roll13", paste0(external, "_lag1"),
                   paste0(external, "_missing"))
valid <- dat$week_idx > 52
test_start <- W - 12L; cal_start <- test_start - 6L; val_start <- cal_start - 13L
dev <- which(valid & dat$week_idx < val_start)
val <- which(valid & dat$week_idx >= val_start & dat$week_idx < cal_start)
cal <- which(valid & dat$week_idx >= cal_start & dat$week_idx < test_start)
test <- which(valid & dat$week_idx >= test_start)

levels_store <- sort(unique(dat$Store[dev]))
stopifnot(all(dat$Store %in% levels_store))
for (s in levels_store[-1]) dat[[paste0("Store_", s)]] <- as.integer(dat$Store == s)
feature_names <- c(base_features, paste0("Store_", levels_store[-1]))
x <- as.matrix(dat[feature_names]); storage.mode(x) <- "double"; y <- dat$Weekly_Sales
stopifnot(!any(c("Weekly_Sales", "CPI", "Unemployment") %in% feature_names))

starts <- val_start - c(39L, 26L, 13L)
folds <- lapply(starts, function(s) list(train = which(valid & dat$week_idx < s),
                                         validation = which(valid & dat$week_idx >= s & dat$week_idx < s + 13L)))
windows <- do.call(rbind, lapply(seq_along(folds), function(k) {
  f <- folds[[k]]; stopifnot(max(dat$Date[f$train]) < min(dat$Date[f$validation]))
  data.frame(stage = paste0("CV", k), train_start = min(dat$Date[f$train]), train_end = max(dat$Date[f$train]),
             eval_start = min(dat$Date[f$validation]), eval_end = max(dat$Date[f$validation]),
             n_train = length(f$train), n_eval = length(f$validation))
}))
for (nm in c("Seleccion", "Calibracion", "Test")) {
  a <- switch(nm, Seleccion = dev, Calibracion = c(dev, val), Test = c(dev, val))
  b <- switch(nm, Seleccion = val, Calibracion = cal, Test = test)
  windows <- rbind(windows, data.frame(stage = nm, train_start = min(dat$Date[a]), train_end = max(dat$Date[a]),
                                       eval_start = min(dat$Date[b]), eval_end = max(dat$Date[b]), n_train = length(a), n_eval = length(b)))
}
save_csv(windows, "particiones_temporales")
save_csv(data.frame(Store = dat$Store, Date = dat$Date, Weekly_Sales = y, Type = dat$Type,
                    etapa = ifelse(!valid, "historia_rezagos", ifelse(dat$week_idx < val_start, "desarrollo",
                                                                      ifelse(dat$week_idx < cal_start, "seleccion", ifelse(dat$week_idx < test_start, "calibracion", "test"))))),
         "datos_agregados_particion")

metric <- function(y, p) {
  stopifnot(length(y) == length(p), all(is.finite(p)))
  c(RMSE = sqrt(mean((y - p)^2)), MAE = mean(abs(y - p)), R2 = 1 - sum((y - p)^2) / sum((y - mean(y))^2))
}

prep_fit <- function(X) {
  med <- apply(X, 2, function(v) if (all(is.na(v))) 0 else median(v, na.rm = TRUE))
  for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- med[j]
  mu <- colMeans(X); sdv <- sqrt(colMeans(sweep(X, 2, mu)^2)); keep <- which(sdv > 1e-10)
  list(med = med, mu = mu, sd = sdv, keep = keep)
}

prep_apply <- function(X, p) {
  for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- p$med[j]
  sweep(sweep(X[, p$keep, drop = FALSE], 2, p$mu[p$keep]), 2, p$sd[p$keep], "/")
}

spec <- function(model, lambda = NA_real_, alpha = NA_real_, df = NA_real_, cp = NA_real_, mtry = NA_real_,
                 depth = NA_real_, shrinkage = NA_real_) data.frame(model, lambda, alpha, df, cp, mtry, depth, shrinkage)

grid <- rbind(spec("OLS"), do.call(rbind, lapply(c(.0001, .001, .01, .1, 1, 10), function(l) spec("Ridge", l, 0))),
              do.call(rbind, lapply(c(.0001, .001, .003, .01, .03, .1), function(l) spec("Lasso", l, 1))),
              do.call(rbind, lapply(c(.25, .5, .75), function(a) do.call(rbind, lapply(c(.001, .003, .01, .03, .1), function(l) spec("ElasticNet", l, a))))),
              do.call(rbind, lapply(c(3, 5), function(d) spec("Spline", df = d))),
              do.call(rbind, lapply(c(.001, .005, .02), function(c) spec("Tree", cp = c))),
              do.call(rbind, lapply(c(5, 12, 24), function(m) spec("RandomForest", mtry = m))),
              do.call(rbind, lapply(c(1, 2, 3), function(d) do.call(rbind, lapply(c(.03, .08), function(s) spec("Boosting", depth = d, shrinkage = s))))))
grid$id <- seq_len(nrow(grid)); chosen <- NULL

fit_model <- function(s, rows) {
  fam <- s$model
  if (fam %in% c("PrevWeek", "Seasonal")) return(list(family = fam))
  if (fam == "Averaging") return(list(family = fam, members = lapply(c("Lasso", "RandomForest", "Boosting"),
                                                                     function(m) fit_model(chosen[chosen$model == m, ], rows))))
  rawx <- x[rows, , drop = FALSE]; pp <- prep_fit(rawx); X <- prep_apply(rawx, pp)
  basis <- list()
  if (fam == "Spline") for (nm in c("time_idx", "sales_lag1", "sales_lag52", "sales_roll13")) {
    b <- splines::ns(X[, nm], df = s$df); basis[[nm]] <- b
    X <- cbind(X[, setdiff(colnames(X), nm), drop = FALSE], setNames(as.data.frame(b), paste0(nm, "_sp", seq_len(ncol(b)))))
    X <- as.matrix(X)
  }
  ym <- mean(y[rows]); ys <- sqrt(mean((y[rows] - ym)^2)); Y <- (y[rows] - ym) / ys
  set.seed(2026L + sum(rows) %% 100000L)
  obj <- switch(fam,
                OLS = lm.fit(cbind(Intercept = 1, X), Y),
                Spline = lm.fit(cbind(Intercept = 1, X), Y),
                Ridge = glmnet::glmnet(X, Y, alpha = 0, lambda = sort(unique(c(10, s$lambda)), decreasing = TRUE),
                                       standardize = FALSE, intercept = TRUE, control = list(thresh = 1e-10, maxit = 100000)),
                Lasso = glmnet::glmnet(X, Y, alpha = 1, lambda = sort(unique(c(1, s$lambda)), decreasing = TRUE),
                                       standardize = FALSE, intercept = TRUE, control = list(thresh = 1e-10, maxit = 100000)),
                ElasticNet = glmnet::glmnet(X, Y, alpha = s$alpha, lambda = sort(unique(c(1, s$lambda)), decreasing = TRUE),
                                            standardize = FALSE, intercept = TRUE, control = list(thresh = 1e-10, maxit = 100000)),
                Tree = rpart::rpart(response ~ ., data = data.frame(response = Y, X), method = "anova",
                                    control = rpart::rpart.control(cp = s$cp, xval = 0, minsplit = 20, maxdepth = 20)),
                RandomForest = randomForest::randomForest(X, Y, ntree = 300, mtry = min(s$mtry, ncol(X)), nodesize = 5),
                Boosting = gbm::gbm.fit(X, Y, distribution = "gaussian", n.trees = 500, interaction.depth = s$depth,
                                        shrinkage = s$shrinkage, bag.fraction = .8, n.minobsinnode = 10, keep.data = FALSE, verbose = FALSE))
  if (fam %in% c("Ridge", "Lasso", "ElasticNet") && obj$jerr != 0) stop("glmnet no convergio")
  list(family = fam, spec = s, prep = pp, basis = basis, object = obj, ym = ym, ys = ys,
       train_end = max(dat$Date[rows]))
}

predict_model <- function(m, X) {
  if (m$family == "PrevWeek") return(as.numeric(X[, "sales_lag1"]))
  if (m$family == "Seasonal") return(as.numeric(X[, "sales_lag52"]))
  if (m$family == "Averaging") return(rowMeans(sapply(m$members, function(mm) predict_model(mm, X))))
  Z <- prep_apply(X, m$prep)
  if (m$family == "Spline") for (nm in names(m$basis)) {
    b <- predict(m$basis[[nm]], Z[, nm])
    Z <- cbind(Z[, setdiff(colnames(Z), nm), drop = FALSE], setNames(as.data.frame(b), paste0(nm, "_sp", seq_len(ncol(b)))))
    Z <- as.matrix(Z)
  }
  pr <- switch(m$family,
               OLS = {b <- m$object$coefficients; b[is.na(b)] <- 0; cbind(1, Z) %*% b},
               Spline = {b <- m$object$coefficients; b[is.na(b)] <- 0; cbind(1, Z) %*% b},
               Ridge = predict(m$object, newx = Z, s = m$spec$lambda),
               Lasso = predict(m$object, newx = Z, s = m$spec$lambda),
               ElasticNet = predict(m$object, newx = Z, s = m$spec$lambda),
               Tree = predict(m$object, newdata = as.data.frame(Z)),
               RandomForest = predict(m$object, newdata = Z),
               Boosting = predict(m$object, newdata = Z, n.trees = 500, type = "response"))
  as.numeric(pr) * m$ys + m$ym
}

cat("Auditoria completa:", nrow(raw), "filas,", nrow(dat), "tienda-semana. CV:", nrow(grid), "configuraciones.\n")
cv <- list(); ci <- 1L
for (k in seq_along(folds)) {
  f <- folds[[k]]
  for (j in seq_len(nrow(grid))) {
    m <- fit_model(grid[j, ], f$train); p <- predict_model(m, x[f$validation, , drop = FALSE])
    cv[[ci]] <- data.frame(grid[j, ], fold = k, as.list(metric(y[f$validation], p))); ci <- ci + 1L
  }
  cat("CV", k, "completada\n")
}
cv <- do.call(rbind, cv); save_csv(cv, "cv_metricas_por_fold")
cv_mean <- aggregate(cbind(RMSE, MAE, R2) ~ id, cv, mean)
cv_mean <- merge(grid, cv_mean, by = "id", sort = FALSE); save_csv(cv_mean, "cv_hiperparametros")

chosen <- do.call(rbind, lapply(unique(grid$model), function(nm) {
  z <- cv_mean[cv_mean$model == nm, ]; grid[match(z$id[which.min(z$RMSE)], grid$id), ]
}))
chosen <- rbind(chosen, cbind(spec("Averaging"), id = NA_integer_),
                cbind(spec("PrevWeek"), id = NA_integer_), cbind(spec("Seasonal"), id = NA_integer_))
save_csv(chosen, "hiperparametros_elegidos")

val_models <- lapply(seq_len(nrow(chosen)), function(j) fit_model(chosen[j, ], dev))
names(val_models) <- chosen$model
val_preds <- sapply(val_models, function(mm) predict_model(mm, x[val, , drop = FALSE]))
baseline_rmse <- metric(y[val], val_preds[, "PrevWeek"])["RMSE"]

family_name <- function(nm) if (nm %in% c("PrevWeek", "Seasonal")) "Baseline" else if (nm == "OLS") "Lineal" else if (nm %in% c("Ridge", "Lasso", "ElasticNet")) "Regularizado" else if (nm == "Spline") "Flexible" else if (nm == "Tree") "Arbol" else "Ensemble"

val_metrics <- do.call(rbind, lapply(colnames(val_preds), function(nm) {
  p <- val_preds[, nm]; z <- data.frame(model = nm, family = family_name(nm), as.list(metric(y[val], p)))
  z$RMSE_Type_C <- metric(y[val][dat$Type[val] == "C"], p[dat$Type[val] == "C"])["RMSE"]
  z$improvement_pct <- 100 * (1 - z$RMSE / baseline_rmse); z
}))
save_csv(val_metrics, "validacion_modelos")
save_csv(data.frame(Store = dat$Store[val], Date = dat$Date[val], Actual = y[val], val_preds), "validacion_predicciones")

selected <- val_metrics$model[which.min(val_metrics$RMSE)]
final_train <- c(dev, val)
final_model <- fit_model(chosen[chosen$model == selected, ], final_train)
pcal <- predict_model(final_model, x[cal, , drop = FALSE])

qtype <- vapply(sort(unique(dat$Type[cal])), function(t) {
  e <- abs(y[cal] - pcal)[dat$Type[cal] == t]; quantile(e, min(1, ceiling((length(e) + 1) * .95) / length(e)), type = 1)
}, numeric(1))
save_csv(data.frame(Type = names(qtype), half_width = qtype, n = as.integer(table(dat$Type[cal]))), "calibracion_intervalos")

freeze <- list(selected = selected, spec = chosen[chosen$model == selected, ], fit = final_model,
               intervals = qtype, features = feature_names, train_end = max(dat$Date[final_train]),
               calibration_end = max(dat$Date[cal]), protocol = "one_step_weekly_observed_lags_no_refit",
               seed = 2026L)

# Aislamiento de archivo binario RDS en la subcarpeta 'binarios/'
saveRDS(freeze, file.path(out_bin, "sistema_congelado_antes_test.rds"))
save_txt(c(paste("Modelo:", selected), "Criterio: menor RMSE en seleccion cronologica",
           "Test evaluado solo despues de guardar el sistema; no se selecciona con test."),
         "seleccion_congelada.txt")

pred <- predict_model(final_model, x[test, , drop = FALSE]); prev <- dat$sales_lag1[test]
half <- qtype[dat$Type[test]]
detail <- data.frame(Store = dat$Store[test], Date = dat$Date[test], Type = dat$Type[test],
                     Actual = y[test], Predicted = pred, PrevWeek = prev, Lower = pred - half, Upper = pred + half)
detail$Covered <- detail$Actual >= detail$Lower & detail$Actual <= detail$Upper
save_csv(detail, "test_predicciones")

test_metrics <- rbind(data.frame(model = selected, as.list(metric(y[test], pred))),
                      data.frame(model = "PrevWeek", as.list(metric(y[test], prev))))
save_csv(test_metrics, "test_modelo_y_baseline")

subgroup <- do.call(rbind, lapply(sort(unique(detail$Type)), function(t) {
  z <- detail[detail$Type == t, ]; data.frame(Type = t, n = nrow(z), as.list(metric(z$Actual, z$Predicted)),
                                              RMSE_baseline = metric(z$Actual, z$PrevWeek)["RMSE"], coverage = mean(z$Covered), mean_width = mean(z$Upper - z$Lower))
})); save_csv(subgroup, "test_subgrupos")

weekly <- do.call(rbind, lapply(split(detail, detail$Date), function(z) data.frame(Date = z$Date[1],
                                                                                   Actual = sum(z$Actual), Predicted = sum(z$Predicted), RMSE = sqrt(mean((z$Predicted - z$Actual)^2)),
                                                                                   RMSE_baseline = sqrt(mean((z$PrevWeek - z$Actual)^2)), Bias = mean(z$Predicted - z$Actual))))
weekly <- weekly[order(weekly$Date), ]; save_csv(weekly, "test_semanal")

cat("Modelo seleccionado:", selected, "\n"); print(val_metrics); print(test_metrics)

set.seed(20261109L)
dates <- sort(unique(detail$Date)); Tn <- length(dates); B <- 5000L
block_ci <- do.call(rbind, lapply(c(2L, 3L, 4L), function(L) {
  draws <- replicate(B, {
    starts <- sample.int(Tn - L + 1L, ceiling(Tn / L), replace = TRUE)
    wi <- unlist(lapply(starts, function(s) s:(s + L - 1L)))[seq_len(Tn)]
    ii <- unlist(lapply(wi, function(j) which(detail$Date == dates[j])))
    a <- sqrt(mean((detail$Predicted[ii] - detail$Actual[ii])^2))
    b <- sqrt(mean((detail$PrevWeek[ii] - detail$Actual[ii])^2))
    c(diff = a - b, improvement = 100 * (1 - a / b))
  })
  data.frame(model = selected, comparison = paste(selected, "menos PrevWeek"), block_weeks = L,
             point = test_metrics$RMSE[1] - test_metrics$RMSE[2],
             lower = quantile(draws[1, ], .025), upper = quantile(draws[2, ], .975),
             improvement_pct = 100 * (1 - test_metrics$RMSE[1] / test_metrics$RMSE[2]),
             improvement_low = quantile(draws[2, ], .025), improvement_high = quantile(draws[2, ], .975),
             replicates = B, test_weeks = Tn)
})); save_csv(block_ci, "bootstrap_bloques_moviles")

om <- val_models[["OLS"]]; Z <- prep_apply(x[dev, , drop = FALSE], om$prep)
X0 <- cbind(Intercept = 1, Z); qr0 <- qr(X0); keep <- sort(qr0$pivot[seq_len(qr0$rank)])
X <- X0[, keep, drop = FALSE]; oy <- (y[dev] - om$ym) / om$ys
ofit <- lm.fit(X, oy); e <- ofit$residuals; n <- nrow(X); p <- ncol(X)
save_csv(data.frame(variable = setdiff(colnames(X0), colnames(X))), "ols_columnas_alias")

bread <- solve(crossprod(X))
meat <- function(g) { sc <- rowsum(X * e, as.character(g)); G <- nrow(sc)
crossprod(sc) * G / (G - 1) * (n - 1) / (n - p) }
g1 <- dat$Store[dev]; g2 <- dat$Date[dev]
V <- bread %*% (meat(g1) + meat(g2) - meat(interaction(g1, g2, drop = TRUE))) %*% bread
V <- (V + t(V)) / 2

ev <- eigen(V, symmetric = TRUE); neg <- sum(ev$values < 0)
if (neg) V <- ev$vectors %*% diag(pmax(ev$values, 0)) %*% t(ev$vectors)

j <- match("sales_lag52", colnames(X)); stopifnot(!is.na(j))
beta <- ofit$coefficients[j]; se <- sqrt(V[j, j]); dfc <- min(length(unique(g1)), length(unique(g2))) - 1L
factor_units <- om$ys / om$prep$sd["sales_lag52"]; crit <- qt(.975, dfc)
inference <- data.frame(parameter = "sales_lag52", slope = beta * factor_units,
                        low = (beta - crit * se) * factor_units, high = (beta + crit * se) * factor_units,
                        change_per_100000 = beta * factor_units * 100000,
                        low_per_100000 = (beta - crit * se) * factor_units * 100000,
                        high_per_100000 = (beta + crit * se) * factor_units * 100000,
                        p_value = 2 * pt(-abs(beta / se), dfc), df = dfc, rows = n,
                        stores = length(unique(g1)), weeks = length(unique(g2)), negative_eigenvalues_clipped = neg)
save_csv(inference, "inferencia_ols_cluster")

vif <- vapply(seq.int(2, ncol(X)), function(j) {
  r <- lm.fit(X[, -j, drop = FALSE], X[, j])$residuals
  sum((X[, j] - mean(X[, j]))^2) / sum(r^2)
}, numeric(1))
save_csv(data.frame(variable = colnames(X)[-1], VIF = vif), "ols_vif")

r_original <- e * om$ys; fitted <- y[dev] - r_original
acf_store <- vapply(split(seq_along(e), g1), function(ii) cor(head(e[ii], -1), tail(e[ii], -1)), numeric(1))
save_csv(data.frame(Store = names(acf_store), residual_lag1_correlation = acf_store), "ols_autocorrelacion")
save_csv(data.frame(n = n, rank = p, columns = ncol(X0), condition_number = kappa(X),
                    residual_sd = sd(r_original), median_lag1_correlation = median(acf_store),
                    max_VIF = max(vif)), "ols_diagnosticos")

selected_val <- val_models[[selected]]
reference <- metric(y[val], val_preds[, selected])["RMSE"]
set.seed(341L)
imp <- sapply(base_features, function(nm) mean(replicate(5, {
  xp <- x[val, , drop = FALSE]; xp[, nm] <- sample(xp[, nm])
  metric(y[val], predict_model(selected_val, xp))["RMSE"] - reference
})))
save_csv(data.frame(variable = names(imp), increase_RMSE = imp), "importancia_permutacion_validacion")

g <- as.numeric(quantile(x[dev, "sales_lag52"], seq(.05, .95, length.out = 15)))
ice_rows <- vapply(sort(unique(dat$Type[val])), function(t) {
  ii <- which(dat$Type[val] == t); ii[which.min(abs(x[val, "sales_lag52"][ii] - median(x[val, "sales_lag52"][ii])))]
}, integer(1))
pdp <- sapply(g, function(v) {xp <- x[val, , drop = FALSE]; xp[, "sales_lag52"] <- v; predict_model(selected_val, xp)})
pdp_table <- data.frame(sales_lag52 = g, PDP = colMeans(pdp))
for (i in seq_along(ice_rows)) pdp_table[[paste0("ICE_Type_", names(ice_rows)[i])]] <- pdp[ice_rows[i], ]
save_csv(pdp_table, "pdp_ice")

lm0 <- val_models[["Lasso"]]
bc <- as.matrix(coef(lm0$object, s = lm0$spec$lambda))
save_csv(data.frame(variable = rownames(bc), coefficient_standardized = bc[, 1]), "coeficientes_lasso")

colors <- c("#17324D", "#008C95", "#D28B25", "#8861A9")

# Aislamiento de gráficos de imagen (.png) en la subcarpeta 'graficos/'
png_out <- function(name, w = 1600, h = 950) {
  png(file.path(out_img, paste0(name, ".png")), width = w, height = h, res = 160)
  par(mar = c(4.5, 5, 3, 1), col.axis = "#304455", col.lab = "#304455", col.main = "#17324D")
}

png_out("01_historia_ventas")
hist_week <- aggregate(Weekly_Sales ~ Date, dat, sum)
plot(hist_week$Date, hist_week$Weekly_Sales / 1e6, type = "l", lwd = 2, col = colors[1],
     xlab = "Fecha", ylab = "Ventas totales (millones u.m.)", main = "Historico completo de ventas de las 45 tiendas")
abline(v = as.numeric(date_values[c(val_start, cal_start, test_start)]), lty = 2, col = colors[2:4])
legend("topleft", c("Ventas", "Seleccion", "Calibracion", "Test"), col = colors, lty = c(1, 2, 2, 2), bty = "n"); dev.off()

png_out("02_validacion")
z <- val_metrics[order(val_metrics$RMSE, decreasing = TRUE), ]
par(mar = c(5.2, 9, 3, 2))
barplot(z$RMSE / 1000, names.arg = z$model, horiz = TRUE, las = 1, col = ifelse(z$model == selected, colors[2], colors[1]),
        xlim = c(0, max(z$RMSE) / 1000 * 1.08), xlab = "RMSE por tienda-semana (miles u.m.)", main = "Comparacion en validacion cronologica")
abline(v = baseline_rmse / 1000, lty = 2, col = colors[3]); dev.off()

png_out("03_test_ventas")
plot(weekly$Date, weekly$Actual / 1e6, type = "o", pch = 16, col = colors[1], lwd = 2,
     ylim = range(c(weekly$Actual, weekly$Predicted)) / 1e6,
     xlab = "Semana de test", ylab = "Ventas totales (millones u.m.)", main = paste("Test final -", selected))
lines(weekly$Date, weekly$Predicted / 1e6, type = "o", pch = 17, col = colors[2], lwd = 2)
legend("topleft", c("Observadas", "Pronosticadas"), col = colors[1:2], lty = 1, pch = c(16, 17), bty = "n"); dev.off()

png_out("04_test_error")
matplot(weekly$Date, cbind(weekly$RMSE, weekly$RMSE_baseline) / 1000, type = "o", pch = c(16, 17),
        col = colors[2:3], lty = 1, lwd = 2, xlab = "Semana de test", ylab = "RMSE (miles u.m.)",
        main = "Error semanal del sistema frente al baseline")
legend("topleft", c(selected, "Semana previa"), col = colors[2:3], lty = 1, bty = "n"); dev.off()

png_out("05_ols_diagnosticos", 1600, 1200)
par(mfrow = c(2, 2), mar = c(5, 4.7, 3, 1))
plot(fitted / 1000, r_original / 1000, pch = 16, cex = .35, col = adjustcolor(colors[1], .35),
     xlab = "Ajustadas (miles u.m.)", ylab = "Residuos (miles u.m.)", main = "Residuos frente a ajuste"); abline(h = 0, col = colors[3])
qqnorm(e, pch = 16, cex = .35, main = "Normalidad de residuos OLS", xlab = "Cuantiles teoricos", ylab = "Cuantiles de residuos"); qqline(e, col = colors[3])
plot(fitted / 1000, sqrt(abs(e)), pch = 16, cex = .35, col = adjustcolor(colors[2], .35),
     xlab = "Ajustadas (miles u.m.)", ylab = "Raiz de |residuo estandarizado|", main = "Variacion de dispersion")
barplot(acf_store, col = colors[2], border = NA, xlab = "Tienda", ylab = "Correlacion lag 1", main = "Dependencia residual por tienda"); dev.off()

png_out("06_interpretabilidad", 1700, 900)
par(mfrow = c(1, 2), mar = c(4.5, 9, 3, 1))
ii <- head(order(imp, decreasing = TRUE), 8)
barplot(rev(imp[ii]) / 1000, names.arg = rev(names(imp)[ii]), horiz = TRUE, las = 1, cex.names = .8, col = colors[2],
        main = "Permutacion en validacion", xlab = "Aumento de RMSE (miles u.m.)")
par(mar = c(4.5, 5, 3, 1))
matplot(g / 1000, as.matrix(pdp_table[, -1]) / 1000, type = "l", lty = c(1, 2, 2, 2), lwd = c(3, 1.5, 1.5, 1.5),
        col = colors, xlab = "Venta t-52 (miles u.m.)", ylab = "Prediccion (miles u.m.)", main = paste("PDP e ICE de", selected))
legend("topleft", c("PDP", paste("ICE tipo", names(ice_rows))), col = colors, lty = c(1, 2, 2, 2), bty = "n", cex = .8); dev.off()

png_out("07_intervalos_ejemplo")
sid <- sort(unique(detail$Store[detail$Type == "A"]))[1]
zz <- detail[detail$Store == sid, ]; zz <- zz[order(zz$Date), ]
plot(zz$Date, zz$Actual / 1000, type = "n", ylim = range(c(zz$Lower, zz$Upper, zz$Actual)) / 1000,
     xlab = "Semana de test", ylab = "Venta (miles u.m.)", main = paste("Intervalo empirico nominal 95% - tienda", sid))
polygon(c(zz$Date, rev(zz$Date)), c(zz$Lower, rev(zz$Upper)) / 1000, col = adjustcolor(colors[2], .2), border = NA)
lines(zz$Date, zz$Predicted / 1000, lwd = 2, col = colors[2]); lines(zz$Date, zz$Actual / 1000, lwd = 2, col = colors[1])
legend("topleft", c("Observada", "Pronostico", "Intervalo"), col = c(colors[1:2], adjustcolor(colors[2], .4)), lty = c(1, 1, 1), bty = "n"); dev.off()

dictionary <- data.frame(variable = feature_names, type = "numerica", unit = "indicador",
                         availability = "conocida al predecir t", description = feature_names)
for (i in seq_len(nrow(dictionary))) {
  nm <- dictionary$variable[i]
  if (grepl("^sales_", nm)) {dictionary$unit[i] <- "u.m."; dictionary$availability[i] <- "solo ventas hasta t-1"}
  if (grepl("_lag1$", nm)) dictionary$availability[i] <- "observacion de t-1, supuesto disponible al cierre"
  if (grepl("^Store_", nm)) dictionary$description[i] <- "Indicador de tienda conocida; categoria base omitida"
  if (grepl("_missing$", nm)) dictionary$description[i] <- "1 si el valor rezagado falta; imputacion mediana ajustada en entrenamiento"
}
dictionary <- rbind(data.frame(variable = c("Weekly_Sales", "Store", "Date", "Type", "Size", "CPI", "Unemployment", "Dept"),
                               type = c("continua", "identificador", "fecha", "categorica", "continua", "continua", "continua", "identificador"),
                               unit = c("u.m.", "ID", "semana", "A/B/C", "tamano original", "indice", "tasa", "ID"),
                               availability = c("al terminar t", "estatica", "calendario", "estatica", "estatica", "publicacion desconocida", "publicacion desconocida", "historica"),
                               description = c("Objetivo agregado por tienda-semana", "Clave", "Semana objetivo", "Subgrupo, no predictor",
                                               "No predictor: absorbida por tienda", "Excluida del modelo", "Excluida del modelo", "Solo agregacion")), dictionary)
save_csv(dictionary, "diccionario_variables")

save_csv(data.frame(check = c("claves_unicas", "calendario_regular", "folds_temporales", "sin_objetivo_predictor",
                              "seleccion_antes_test", "calibracion_independiente", "predicciones_finitas", "modelo_guardado"),
                    status = "PASS"), "controles_pipeline")

summary <- data.frame(selected = selected, rows_raw = nrow(raw), rows_aggregated = nrow(dat), rows_modelable = sum(valid),
                      stores = nrow(stores), start = as.character(min(dat$Date)), end = as.character(max(dat$Date)),
                      validation_RMSE = val_metrics$RMSE[val_metrics$model == selected], test_RMSE = test_metrics$RMSE[1],
                      baseline_RMSE = test_metrics$RMSE[2], improvement_pct = 100 * (1 - test_metrics$RMSE[1] / test_metrics$RMSE[2]),
                      coverage = mean(detail$Covered), interval_width = mean(detail$Upper - detail$Lower))
save_csv(summary, "resumen")

# Aislamiento de archivo binario RDS en la subcarpeta 'binarios/'
saveRDS(list(x = x, y = y, dat = dat, dev = dev, val = val, cal = cal, test = test, folds = folds,
             chosen = chosen, selected = selected, val_predictions = val_preds, final_model = final_model),
        file.path(out_bin, "estado_auditoria.rds"))

save_txt(capture.output(sessionInfo()), "sessionInfo.txt")
cat("FINALIZADO. Resultados en", out, "\n"); print(summary)