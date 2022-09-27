# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

yogurt$brand <- factor(yogurt$brand, levels = c(
  "weight", "hiland", "yoplait", "dannon"))

# ============================================================================
# Estimate homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl_pref <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'brand')
)

summary(mnl_pref)

# Run a MNL model in the WTP Space using a multistart:
wtp_mnl_pref <- wtp(mnl_pref, priceName = 'price')
mnl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = 'brand',
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    numMultiStarts = 10,
    keepAllRuns = TRUE, 
    startVals = wtp_mnl_pref$Estimate)
)

# Save results
saveRDS(mnl_pref,
        here::here('models', 'mnl_pref.Rds'))
saveRDS(mnl_wtp,
        here::here('models', 'mnl_wtp.Rds'))

# ============================================================================
# Estimate heterogeneous MXL models

yogurt_neg_price <- yogurt
yogurt_neg_price$price <- -1*yogurt$price

mxl_pref1 <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'brand'),
  randPars   = c(brand = 'n'),
  options = list(
    keepAllRuns = TRUE,
    numMultiStarts = 10)
)

mxl_pref2 <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'brand'),
  randPars   = c(price = 'n', brand = 'n'),
  options = list(
    keepAllRuns = TRUE,
    numMultiStarts = 10)
)

mxl_pref3 <- logitr(
  data       = yogurt_neg_price,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'brand'),
  randPars   = c(price = 'ln', brand = 'n'),
  options = list(
    keepAllRuns = TRUE,
    numMultiStarts = 10)
)

mxl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = 'brand',
  priceName  = 'price',
  randPars   = c(brand = 'n'),
  modelSpace = 'wtp',
  options    = list(
    keepAllRuns = TRUE,
    numMultiStarts = 10)
)

# Save results
saveRDS(mxl_pref1, here::here('models', 'mxl_pref1.Rds'))
saveRDS(mxl_pref2, here::here('models', 'mxl_pref2.Rds'))
saveRDS(mxl_pref3, here::here('models', 'mxl_pref3.Rds'))
saveRDS(mxl_wtp,   here::here('models', 'mxl_wtp.Rds'))
