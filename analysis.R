#' ---
#' title: "BFLPE Moderators"
#' author: "Ivan Ropovik"
#' date: "`r Sys.Date()`"
#' output:
#'    html_document:
#'       toc: true
#'       toc_float: true
#'       code_folding: show
#'       fig_retina: 2
#' always_allow_html: yes
#' ---
#+ setup, include=FALSE
knitr::opts_chunk$set(echo=FALSE, warning = FALSE, fig.width = 10, fig.height = 10)
options(kableExtra.auto_format = FALSE)

# IDEAS
# moderovat vztah medzi class-level achievement a individualnym selfkonceptom: (1) priemernou znamkou (2) variabilitou znamok (znamky D8_a MAT, D8_b CJ)
# SES, gender ratio (D1 1 = female, 2 = male), ci spravil prijimacky (D6, 1 = ano, 2 = ne), sense of belonging (A1, inverzne polozky), supportive climate (C4a cestina C4b matematika, budto vsetky polozky alebo c(a c d f i h), nie su inverzne),
# Akademicka marnost (B5_1:B5_5), Fixny/growth mindset (B5_6, B5_7)

# Install required R libraries if not installed already
list.of.packages <- c("lavaan", "lme4", "lmerTest", "ggplot2", "tidyr", "tidyverse", "semPlot", "psych", "GPArotation", "ICC", "mice", "Amelia", "haven", "survey", "lavaan.survey", "semTools", "knitr", "kableExtra", "gplots", "naniar", "careless", "corpcor", "effectsize", "RColorBrewer", "haven", "performance", "skimr", "corrr", "moments", "corrplot", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
#+ include = FALSE
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# Read in the data

# The attached dataset contains primarily the data for the 34 SAL items. Since the data come from a large-scale national study on education, some of the variables are still under embargo.
# Although some of these variables like type of school are not publicly shared, they are available upon request. The code thus does not run out-of-the box when missing variables are involved.
# The script mainly documents the analytic workflow underlying the analyses reported in the paper.

dat <- read_sav("SYRIzak.SAV")
# View(full.data)

# Define the threshold for the maximum number of consecutive identical responses for the identification of careless responders
carelessThreshold <- 20

# Define the number of bootstrap samples
nboot <- 200

# Select the variables for the CFA
dat <- dat %>% select(B1_1:B2_11, D1, D6, D8_a, D8_b, B5_1:B5_5, B5_6, B5_7, A1_1:A1_7, C4a_1:C4a_9, C4b_1:C4b_9, TotalM, RelM, IRT_M, TotalCJ, RelCJ, IRT_CJ, SES, IDclass, ID_skola, STUWGT, D8askew, D8bskew)

# Inverse-scale self-concept items
dat <- dat %>% mutate(across(starts_with("B2_"), ~ 5 - .))

dat$careless.resp <- longstring(dat[,1:29], avg = FALSE)
dat <- dat[dat$careless.resp < carelessThreshold,]

dat <- dat %>% group_by(IDclass) %>%  # Group data by class
  mutate(gender.prop = mean(D1 == 2), # Compute the mean of D1 == 2 within each group
         vg.prop = mean(D6 == 1))

##################################

# Classes/schools visualization -------------------------------------------
# Calculate the number of unique classes per school
classes_per_school <- dat %>%
  group_by(ID_skola) %>%
  summarise(n_classes = n_distinct(IDclass)) %>%
  ungroup()

#'# Descriptives
#'

# Subset the dataset and remove rows with >80% missing data
datDesc <- dat %>% 
  select(-IDclass, -ID_skola, -STUWGT) %>% 
  as.data.frame() %>%
  # Calculate the proportion of missing values for each row
  mutate(missingProp = rowMeans(is.na(.))) %>%
  # Filter out rows with more than 80% missing data
  filter(missingProp < 0.8) %>%
  # Remove the missingProp column
  select(-missingProp)

#'#### Gender proportion
(gender.prop <- paste(round(table(dat$D1[dat$D1 == 1])/length(dat$D1[!is.na(dat$D1)])*100, 2), "%", sep = "")) # Proportion of gender == 1
#' Gender == 1 (Girls) represents `r gender.prop` of the sample.
#' 

#'#### Number of Classes per School
ggplot(classes_per_school, aes(x = n_classes)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", boundary = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of Number of Classes per School",
    x = "Number of Classes",
    y = "Number of Schools"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

#'## Basic descriptives
descPsych <- describe(datDesc[-1], skew = TRUE, ranges = TRUE, type = 2)
kable(descPsych, "html", digits = 2) %>%
      kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12, position = "left")

## Skimr summary for a quick overview
descSkimr <- skim(datDesc)

#'## Missingness Analysis
## Missingness pattern visualization
vis_miss(datDesc) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#'#### Overall percentage of missing data
paste(round(sum(is.na(dat[,1:34]))/prod(dim(dat[,1:34]))*100, 3), "%", sep = "")
#'#### Percentage of missing data after removing empty rows
paste(round(sum(is.na(datDesc))/prod(dim(datDesc))*100, 3), "%", sep = "")

## Missingness summary
missingnessSummary <- datDesc %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Percent_Missing") %>%
  arrange(desc(Percent_Missing))

#'## Distribution Plots
numeric_vars <- datDesc %>% 
  select(where(is.numeric)) %>% 
  names()

# Split variables into groups for readability
var_groups <- split(numeric_vars, ceiling(seq_along(numeric_vars)/6))

for (i in seq_along(var_groups)) {
  plots <- lapply(var_groups[[i]], function(var) {
    ggplot(datDesc, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      geom_density(color = "red", linewidth = 1) +
      labs(title = var, x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  do.call(grid.arrange, c(plots, ncol = 3))
}

#'## Correlation heatmap
datDesc %>%
  select(where(is.numeric) & -c(IDclass, vg.prop, gender.prop)) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method = "color",
           type = "full", 
           
           number.cex = 0.7, 
           tl.cex = 0.7, 
           diag = FALSE)

# Outlier Detection
## Z-score for potential outliers
outlierSummary <- datDesc %>%
  select(where(is.numeric)) %>%
  summarise(across(
    .cols = everything(),
    .fns = list(
      zScoreOutliers = ~sum(abs(scale(.)) > 3.29, na.rm = TRUE)
    )
  )) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Outliers")

# Create visualization of grade distributions
createGradeDistributionPlots <- function(data) {
  # Plot for Math grades
  plot_math <- ggplot(data, aes(x = D8_a)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~IDclass) +
    labs(title = "Distribution of Math Grades by Class",
         x = "Math Grade",
         y = "Frequency") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
  # Plot for Language grades
  plot_lang <- ggplot(data, aes(x = D8_b)) +
    geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
    facet_wrap(~IDclass) +
    labs(title = "Distribution of Language Grades by Class",
         x = "Language Grade",
         y = "Frequency") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
  return(list(math = plot_math, language = plot_lang))
}

#'## Grade distribution plots
createGradeDistributionPlots(dat)

#####################
# Descriptive
# Frequency tables (in % responses)
#+eval = FALSE
(freq.items <- lapply(dat[,1:29], function(x){round(table(x, useNA = "ifany")/length(!is.na(x)), 2)}))

#'# SAL analysis
#'
#'## Polychoric correlation heatmap
#'
polychoric.cor <- round(polychoric(dat[1:26], correct = FALSE, smooth = TRUE,
                             global = FALSE, na.rm = TRUE)$rho, 2)
# polychoric.cor[upper.tri(polychoric.cor)] <- NA
heatmap.2(polychoric.cor, notecex = 1, notecol = "black", dendrogram = "none",
          cellnote = matrix(gsub("0\\.", "\\.", sprintf("%.2f", polychoric.cor)), nrow(polychoric.cor), ncol(polychoric.cor)),
          density.info = "density", key.title = "Density plot",
          col = RColorBrewer::brewer.pal(n = 9, name = "RdYlBu"),
          Rowv = FALSE, Colv = FALSE, trace = "none", tracecol = "#303030",
          lmat=rbind(c(0, 3), c(2,1), c(0,4)), lhei=c(.1, 5.5, 2), lwid=c(0.1, 4))

#'## Polychoric correlation matrix
#'Matrix can be scrolled in every direction
polychoric.cor[upper.tri(polychoric.cor)] <- ""
polychoric.cor <- as.data.frame(polychoric.cor)
kable(polychoric.cor, "html") %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), font_size = 12, position = "left") %>%
  kableExtra::scroll_box(height = 100, width = 200)

#'### Mean item correlation
#' Mean inter-item polychoric correlation
mean(abs(as.numeric(unlist(polychoric.cor))), na.rm = T)

# Computation of polychoric covariance matrix
# SDs <- describe(data[1:29], na.rm = TRUE)$sd
# polychoric.cov <- cor2cov(polychoric.cor$rho, SDs)

#'## CFA model
#' 10-factor structure, all factors intercorrelated.
#'

model <- '
INSMOT =~ B1_3 + B1_8 + B1_12
EFFPER =~ B1_4 + B1_7 + B1_11 + B1_15
SELFEF =~ B1_10 + B1_5 + B1_14
CEXP =~ B1_2 + B1_6 + B1_9 + B1_13
SCVERB =~ B2_1 + B2_4 + B2_7 + B2_10
SCMATH =~ B2_2 + B2_5 + B2_9 + B2_11
SCACAD =~ B2_3 + B2_6 + B2_8
'
#' Some very rough guide to model fit interpretation:
#'
# For an approximately fitting model, CFI and TLI should be >.95, RMSEA < .06. If the model does not fit the data (based on a significant chi^2), we cannot rely on model parameters. Usually, they may be slightly off for a misspecified model (say in units), but with severe misspecification, they can be way off (sometimes even in tenths).
#'
#' The model was estimated using the Weighted Least Squares Means- and Variance-adjusted fit function while explicitly modeling the ordered nature of the indicators.
#' The given type of estimator (1) is robust with respect to the assumption of normal distribution of errors (especially kurtosis; not likely in Likert scales), (2) induces less bias in parameter estimation and model fit test of misspecified models, and (3) the proportion of Type I errors in assessing correctly specified models with the given data is way more similar to the apriori defined nominal α value, as compared to, e.g., the method of maximum likelihood (Beauducel, Herzberg, 2009).
#'
fit <- cfa(model = model, data = dat, meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus", sampling.weights = "STUWGT", #cluster = "IDclass", 
                    estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
                    ordered = names(dat)[1:26])
#'The model failed the model test. That means, that we can reject the hypothesis of exact fit (which is not very surprising).
#'CFI and TLI are below the cutpoint of .95 for good approximate fit, RMSEA seems quite good - especially its upped bound CI does not cross .05. Given SRMR, there does not seem to be much global absolute misfit.
#'However, significant chi^2 indicates beyond-chance deviations of the data from the theoretized structure. Further detailed model diagnostics are needed.
#'

# Specification of the sampling design.
# The model applies the sampling weights (vaha6) and accounts for the two-level hierarchical structure of the data (children nested within classes nested within schools).
design <- svydesign(ids = ~ ID_skola + IDclass, data = dat, weights = ~STUWGT)
fitted.model <- lavaan.survey(lavaan.fit = fit, survey.design = design, estimator = "MLMVS")

#'### Model test and approximate fit indices
cfa.fit <- fitmeasures(fitted.model, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
cfa.fit

#'### Power analysis
#'
#'For approximate fit (based on the RMSEA distribution). Statistical power for the detection of a likely misspecified model (RMSEA > .08).
df <- fitted.model@test[[1]]$df
alpha <- .05
n <- nrow(dat)
rmsea0 <- .05           # RMSEA given H0
rmseaa <- .08           # RMSEA given Ha

ncp0 <- (n-1)*df*rmsea0^2 ;
ncpa <-(n-1)*df*rmseaa^2 ;
if(rmsea0 < rmseaa) {
  cval <- qchisq(1-alpha,df=df,ncp=ncp0)
  pwr.rmsea <- 1 - pchisq(cval,df=df,ncp=ncpa)
} else {
  cval <- qchisq(alpha,df=df,ncp=ncp0)
  pwr.rmsea <- pchisq(cval,df=df,ncp=ncpa)
}
rm(ncp0, ncpa, cval)
print(round(pwr.rmsea,10))

#'Given the model and the sample size, there is almost certainty that a badly fitting model would be flagged by RMSEA.
#'

#'### Parameter estimates
#' The following parts of the output is of interest:
#' "Latent variables" shows factor loadings (all of them significant); Standardized estimates can be found in collumn "Std.all". "Covariances" show the correlations between the factors. Standardized estimates are to be found in collumn "Std.all".
#' For the given target interpretation, Intercepts, Thresholds, Intercepts (...) are likely of secondary interest.
summary(fitted.model, standardized = TRUE)

#'### Mean factor loading
round(mean(inspect(fitted.model,what="std")$lambda[inspect(fitted.model,what="std")$lambda > .0]), 3)
#' For how many of the items does the factor explain more than half of their variance (lambda = sqrt(2)/2, ~.707)?
paste(round(table(inspect(fitted.model,what="std")$lambda[inspect(fitted.model,what="std")$lambda > .0] > .7)/34, 3)[2]*100, "%", sep = "")
#' How many of the items show a loading greater than .6?
paste(round(table(inspect(fitted.model,what="std")$lambda[inspect(fitted.model,what="std")$lambda > .0] > .6)/34, 3)[2]*100, "%", sep = "")

#'### Latent correlations
#' For convenience, latent correlations between the SAL factors in form of a matrix.
#'
lv.cor <- round(inspect(fitted.model, "cor.lv"), 2)
lv.cor[upper.tri(lv.cor)] <- ""
lv.cor <- as.data.frame(lv.cor)
kable(lv.cor, "html", digits = 2) %>%
  kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left")

#'### Latent partial correlation matrix
#'
#' Partial correlations where each correlation in the matrix represents the magnitude of the relationship after controlling for all the outher latent variables
lv.pcor <- round(cor2pcor(inspect(fitted.model, "cor.lv")), 2)
dimnames(lv.pcor) <- dimnames(lv.cor)
lv.pcor[upper.tri(lv.pcor)] <- ""
kable(lv.pcor, "html", digits = 2) %>%
  kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left")

#'### Diagram
semPaths(fitted.model, style = "mx", edge.label.cex = 0.7,
         sizeLat = 7, nCharNodes = 0, nDigits = 2, "Standardized",
         intercepts = FALSE, residuals = FALSE, what = "path",
         edge.label.position = .5, layout = "circle",
         node.width = 1.1, color = "white", thresholds = FALSE)

#'## SAL Model diagnostics
#'
#'### Modification indices
#' The next step is then to look at the modification indices table. Collumn "mi" stands for the modification index. It represents the change in chi square statistics if you free the given parameter. "~" stands for regression path (read as "predicted by"), "~~" denotes a correlation. "sepc.all" is the value of correlation or standardized regression path that the model missed.
#' Showing only the 10 most severe misspecifications, sorted by magnitude.
#'
#'
#residuals(fitted.model, type = "standardized")$cov
#sort(residuals(fitted.model, type = "standardized")$cov)
cfa.mod.ind <- modificationindices(fitted.model, alpha = .05, sort. = TRUE, maximum.number = 10)
kable(cfa.mod.ind, "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), full_width = F, font_size = 12, position = "left")

#'### Heatmap of standardized residuals
#' Model test indicates the presence of model misspecification. Apart from (global) approximate fit indices,
#' it is necessary to also analyze local sources of causal misspecification based on a matrix of standardized residuals.
#'
#' The same as shown by residuals can be seen on residuals heatmap.
residuals.std <- round(residuals(fitted.model, type = "normalized")$cov, 2)
heatmap.2(residuals.std, cellnote = residuals.std, notecex = 1, notecol = "#4ff92a", dendrogram = "none", Rowv = FALSE, Colv = FALSE, trace = "both", col = bluered, tracecol = "#303030",
          lmat=rbind(c(0, 3), c(2,1), c(0,4)), lhei=c(.1, 5.5, 2), lwid=c(0.1, 4))

#'### Matrix of raw residuals
residuals.raw <- round(residuals(fitted.model, type = "cor")$cov, 2)
residuals.raw[upper.tri(residuals.raw)] <- ""
residuals.raw <- as.data.frame(residuals.raw)
kable(residuals.raw, "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), full_width = F, font_size = 12, position = "left") %>%
  kableExtra::scroll_box(height = 100, width = 200)

#'#### Mean of absolute raw residual correlations
mean(abs(residuals(fitted.model, type = "cor")$cov[lower.tri(residuals(fitted.model, type = "cor")$cov)]))

#'### Visualization of significant misspecifications
#' For a better overview, here is the residual matrix which marks residual values >.1. Such values can be seen as worrying.
#'
#' **Number of variables**
p = 34
#'
pn <- (p*(p+1)/2 - p)
#'If the matrix contains (p(p+1)/2 - p) = `r pn` elements (sans diagonal), `r pn*.05`
#'can be significant at alpha = .05
#'
#' **Number of residuals significant at .05 level**
table(abs(residuals.std[lower.tri(residuals.std)]) > 1.96)[2]
#' **Number of residuals significant at .001 level**
table(abs(residuals.std[lower.tri(residuals.std)]) > 3.29)[2]
residuals.raw <- residuals(fitted.model, type = "cor")$cov
sign.residuals <- ifelse(residuals.raw > -5e-6 & residuals.raw < 5e-6, "Diag", ifelse(abs(residuals.raw) > .1, ">.1", "."))
sign.residuals[upper.tri(sign.residuals)] <- ""
sign.residuals <- as.data.frame(sign.residuals)
kable(sign.residuals, "html", digits = 2) %>%
  kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left") %>%
  scroll_box(height = 100, width = 200)

###########################################
#'# BFLPE analysis

## Define Survey Design
design <- svydesign(ids = ~ ID_skola + IDclass, data = dat, weights = ~ STUWGT)

## Initialize Lists to Store Results
cfaResults <- list()
mixedResults <- list()
semResults <- list()
allResults <- list()

## 5. Define Domains and Centerings
domains <- c("VERB", "MATH", "OVERALL")
centerings <- c("group", "grand")

## 6. Begin Analysis Loop
for (domain in domains) {
  for (centering in centerings) {
    # Create a unique key for storing results
    key <- paste(domain, centering, sep = "_")
    #message(sprintf("Running analysis for %s domain with %s centering", domain, centering))
    
    ## 6.1 Data Centering and Preparation
    
    ### 6.1.1 Select Achievement Variable Based on Domain
    achVar <- switch(domain,
                     VERB = "IRT_CJ",
                     MATH = "IRT_M",
                     OVERALL = "IRT_ACH")
    
    ### 6.1.2 Create Class-Level Variable Name
    classVar <- paste0(achVar, "_class")
    
    ### 6.1.3 Define Indicator Variables Based on Domain
    indVars <- switch(domain,
                      VERB = c("B2_1", "B2_4", "B2_7", "B2_10"),
                      MATH = c("B2_2", "B2_5", "B2_9", "B2_11"),
                      OVERALL = c("B2_3", "B2_6", "B2_8"))
    
    ### 6.1.4 Compute OVERALL Achievement if Needed
    if (domain == "OVERALL" && !"IRT_ACH" %in% names(dat)) {
      principalResult <- principal(dat[, c("IRT_CJ", "IRT_M")], nfactors = 1, rotate = "none")
      dat$IRT_ACH <- principalResult$scores[, 1]
    }
    
    ### 6.1.5 Filter Data for Complete Cases
    datFiltered <- dat %>%
      ungroup() %>%
      mutate(missingProp = rowMeans(is.na(.))) %>%
      filter(missingProp < 0.8) %>%
      select(-missingProp)
    
    ### 6.1.6 Filter for Minimum Class Size (>=15)
    datFiltered <- datFiltered %>%
      group_by(IDclass) %>%
      mutate(classSize = n()) %>%
      filter(classSize >= 15) %>%
      ungroup()
    
    ### 6.1.7 Fit CFA (Measurement Model)
    measModel <- paste0("SC", domain, " =~ ", paste(indVars, collapse = " + "))
    cfaFit <- cfa(measModel, data = datFiltered, missing = "fiml", estimator = "MLR",  sampling.weights = "STUWGT")
    cfaResults[[key]] <- cfaFit  # Store CFA Fit
    
    ### 6.1.8 Get Predicted Factor Scores
    datFiltered$score <- lavPredict(cfaFit, transform = TRUE)
    
    ### 6.1.9 Centering Achievement Variable
    if (centering == "group") {
      datFiltered <- datFiltered %>%
        group_by(IDclass) %>%
        mutate(
          !!classVar := mean(get(achVar), na.rm = TRUE),
          !!achVar := get(achVar) - get(classVar)
        ) %>%
        ungroup()
    } else if (centering == "grand") {
      grandMean <- mean(datFiltered[[achVar]], na.rm = TRUE)
      datFiltered <- datFiltered %>%
        group_by(IDclass) %>%
        mutate(
          !!classVar := mean(get(achVar), na.rm = TRUE),
          !!achVar := get(achVar) - grandMean
        ) %>%
        ungroup()
    }
    
    ## 6.2 Fit Mixed Model
    
    ### 6.2.1 Define Mixed Model Formula
    mixedFormula <- as.formula(paste("score ~", achVar, "+", classVar, "+ (1 | IDclass)"))
    
    ### 6.2.2 Fit Mixed Model
    mixedModel <- lmer(mixedFormula, data = datFiltered)
    
    ### 6.2.3 Calculate ICC
    iccValue <- performance::icc(mixedModel)$ICC_adjusted
    
    ### 6.2.4 Standardize Mixed Model Coefficients
    fixedEffects <- fixef(mixedModel)
    vc <- VarCorr(mixedModel)
    randomVar <- as.numeric(vc$IDclass[1])
    residualVar <- sigma(mixedModel)^2
    predSds <- sapply(names(fixedEffects)[-1], function(var) sd(datFiltered[[var]], na.rm = TRUE))
    outcomeSd <- sd(datFiltered$score, na.rm = TRUE)
    stdCoef <- fixedEffects[-1] * predSds / outcomeSd
    vcovMatrix <- vcov(mixedModel)[-1, -1]
    stdSe <- sqrt(diag(vcovMatrix)) * abs(predSds / outcomeSd)
    varExplainedL1 <- var(predict(mixedModel, re.form = NA), na.rm = TRUE)
    r2Within <- varExplainedL1 / (varExplainedL1 + residualVar)
    varExplainedL2 <- var(ranef(mixedModel)$IDclass[[1]], na.rm = TRUE)
    r2Between <- varExplainedL2 / (varExplainedL2 + randomVar)
    
    ### 6.2.5 Store Mixed Model Results
    mixedResults[[key]] <- list(
      coef = fixedEffects,
      se = sqrt(diag(vcov(mixedModel))),
      icc = iccValue,
      stdCoef = stdCoef,
      stdSe = stdSe,
      r2Within = r2Within,
      r2Between = r2Between
    )
    
    ## 6.3 Fit SEM Models (sem() and sam())
    
    ### 6.3.1 Define SEM Model Syntax
    modelSyntax <- switch(domain,
                          VERB = '
        # Measurement model
        SCVERB =~ B2_1 + B2_4 + B2_7 + B2_10
        
        # Structural model
        SCVERB ~ b1*IRT_CJ + b2*IRT_CJ_class
        
        # Class-level achievement
        IRT_CJ_class ~ IRT_CJ
      ',
                          MATH = '
        # Measurement model
        SCMATH =~ B2_2 + B2_5 + B2_9 + B2_11
        
        # Structural model
        SCMATH ~ b1*IRT_M + b2*IRT_M_class
        
        # Class-level achievement
        IRT_M_class ~ IRT_M
      ',
                          OVERALL = '
        # Measurement model
        SCACAD =~ B2_3 + B2_6 + B2_8
        
        # Structural model
        SCACAD ~ b1*IRT_ACH + b2*IRT_ACH_class
        
        # Class-level achievement
        IRT_ACH_class ~ IRT_ACH
      '
    )
    
    mlmModelSyntax <- switch(domain,
                             VERB = '
        # Multilevel model specification

        # Within-Level (Individual Level)
        level: 1

            # Measurement model with equality constraints and fixed loading
            SCVERBw =~ l1*B2_1 + l2*B2_4 + l3*B2_7 + l4*B2_10

            # Structural model
            SCVERBw ~ b.within*IRT_CJ  # Regression on individual achievement

        # Between-Level (Class Level)
        level: 2

            # Measurement model with equality constraints and fixed loading
            SCVERBb =~ l1*B2_1 + l2*B2_4 + l3*B2_7 + l4*B2_10

            # Structural model
            SCVERBb ~ b.between*IRT_CJ_class  # Regression on class-level achievement
            
            bflpe := b.between-b.within
    ',
                             
                             MATH = '
        # Multilevel model specification

        # Within-Level (Individual Level)
        level: 1

            # Measurement model with equality constraints and fixed loading
            SCMATHw =~ l1*B2_2 + l2*B2_5 + l3*B2_9 + l4*B2_11

            # Structural model
            SCMATHw ~ b.within*IRT_M  # Regression on individual achievement

        # Between-Level (Class Level)
        level: 2

            # Measurement model with equality constraints and fixed loading
            SCMATHb =~ l1*B2_2 + l2*B2_5 + l3*B2_9 + l4*B2_11

            # Structural model
            SCMATHb ~ b.between*IRT_M_class  # Regression on class-level achievement
            
            bflpe := b.between-b.within
    ',
                             
                             OVERALL = '
        # Multilevel model specification

        # Within-Level (Individual Level)
        level: 1

            # Measurement model with equality constraints and fixed loading
            SCACADw =~ l1*B2_3 + l2*B2_6 + l3*B2_8

            # Structural model
            SCACADw ~ b.within*IRT_ACH  # Regression on individual achievement

        # Between-Level (Class Level)
        level: 2

            # Measurement model with equality constraints and fixed loading
            SCACADb =~ l1*B2_3 + l2*B2_6 + l3*B2_8

            # Structural model
            SCACADb ~ b.between*IRT_ACH_class  # Regression on class-level achievement
            
            bflpe := b.between-b.within
    '
    )
    
    
    ### 6.3.2 Fit SEM Model using sem()
    # Using MLR estimator and FIML for robust fit indices
    semFitSem <- sem(modelSyntax, data = datFiltered, missing = "fiml", estimator = "MLR", sampling.weights = "STUWGT")
    
    ### 6.3.3 Fit SEM Model using sam()
    # Using ML estimator and sam.method = "local"
    semFitSam <- sam(modelSyntax, data = datFiltered, missing = "fiml", sam.method = "local", struc.args = list(estimator = "ML", cluster = "IDclass", sampling.weights = "STUWGT"), mm.args = list(estimator = "MLR"))
    
    ### 6.3.4 Store SEM Fit Results
    semResults[[key]] <- list(
      semFitSem = semFitSem,
      semFitSam = semFitSam
    )
    
    ## 6.4 Store All Results in `allResults` List
    allResults[[key]] <- list(
      domain = domain,
      centering = centering,
      data = datFiltered,
      cfa = cfaFit,
      mixed = mixedResults[[key]],
      semSem = semFitSem,
      semSam = semFitSam,
      nClasses = length(unique(datFiltered$IDclass)),
      avgClassSize = mean(table(datFiltered$IDclass)),
      nStudents = nrow(datFiltered)
    )
  }
}

## 7. Create Summary Tables

### 7.1 Create Summary Table for Linear Mixed Effects Models
lmeSummaryTable <- map_dfr(mixedResults, function(r) {
  tibble(
    Effect = c("Individual", "Contextual"),
    Estimate = r$coef[2:3],  # Assuming first coefficient is intercept
    SE = r$se[2:3],
    p_value = 2 * (1 - pnorm(abs(r$coef[2:3] / r$se[2:3]))),
    Std_Estimate = r$stdCoef,
    Std_SE = r$stdSe,
    ICC = rep(r$icc, 2),
    R2_within = rep(r$r2Within, 2),
    R2_between = rep(r$r2Between, 2)
  )
}, .id = "Model") %>%
  mutate(
    p_value = ifelse(p_value < .001, "< .001", sprintf("%.3f", p_value)),
    Estimate = sprintf("%.3f", Estimate),
    SE = sprintf("%.3f", SE),
    Std_Estimate = sprintf("%.3f", Std_Estimate),
    Std_SE = sprintf("%.3f", Std_SE),
    ICC = sprintf("%.3f", ICC),
    R2_within = sprintf("%.3f", R2_within),
    R2_between = sprintf("%.3f", R2_between)
  ) %>%
  separate(Model, into = c("Domain", "Centering"), sep = "_") %>%
  select(Domain, Centering, everything())

### 7.2 Create Summary Table for SEM Models
semSummaryTable <- map_dfr(semResults, function(fits) {
  # Extract parameter estimates from sam()
  semSamParams <- parameterEstimates(fits$semFitSam, standardized = TRUE)
  semStruct <- semSamParams %>% filter(op == "~") %>% slice(1:2)  # First two regression paths
  
  # Extract fit indices from sem()
  fitMeasuresList <- fitMeasures(fits$semFitSem, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                                   "cfi", "rmsea", "srmr"))
  
  # Handle potential missing fit indices
  chiSqScaled <- ifelse(!is.na(fitMeasuresList["chisq.scaled"]), fitMeasuresList["chisq.scaled"], NA)
  dfScaled <- ifelse(!is.na(fitMeasuresList["df.scaled"]), fitMeasuresList["df.scaled"], NA)
  pvalueScaled <- ifelse(!is.na(fitMeasuresList["pvalue.scaled"]), fitMeasuresList["pvalue.scaled"], NA)
  cfi <- ifelse(!is.na(fitMeasuresList["cfi"]), fitMeasuresList["cfi"], NA)
  rmsea <- ifelse(!is.na(fitMeasuresList["rmsea"]), fitMeasuresList["rmsea"], NA)
  srmr <- ifelse(!is.na(fitMeasuresList["srmr"]), fitMeasuresList["srmr"], NA)
  
  tibble(
    Effect = c("Individual", "Contextual"),
    Estimate = semStruct$est,
    SE = semStruct$se,
    p_value = semStruct$pvalue,
    Std_Estimate = semStruct$std.all,
    Std_SE = semStruct$se,
    ChiSq = rep(chiSqScaled, 2),
    df = rep(dfScaled, 2),
    ChiSq_p = rep(pvalueScaled, 2),
    CFI = rep(cfi, 2),
    RMSEA = rep(rmsea, 2),
    SRMR = rep(srmr, 2)
  )
}, .id = "Model") %>%
  mutate(
    p_value = ifelse(p_value < .001, "< .001", sprintf("%.3f", p_value)),
    Estimate = sprintf("%.3f", Estimate),
    SE = sprintf("%.3f", SE),
    Std_Estimate = sprintf("%.3f", Std_Estimate),
    Std_SE = sprintf("%.3f", Std_SE),
    ChiSq = ifelse(!is.na(ChiSq), sprintf("%.2f", ChiSq), "NA"),
    ChiSq_p = ifelse(!is.na(ChiSq_p),
                     ifelse(ChiSq_p < .001, "< .001", sprintf("%.3f", ChiSq_p)),
                     "NA"),
    CFI = ifelse(!is.na(CFI), sprintf("%.3f", CFI), "NA"),
    RMSEA = ifelse(!is.na(RMSEA), sprintf("%.3f", RMSEA), "NA"),
    SRMR = ifelse(!is.na(SRMR), sprintf("%.3f", SRMR), "NA")
  ) %>%
  separate(Model, into = c("Domain", "Centering"), sep = "_") %>%
  select(Domain, Centering, everything())

## 8. Generate Summary Tables Using `kable`

### 8.1 Linear Mixed Effects Model Results

# Prepare the LME Summary Table
lmeSummary <- lmeSummaryTable %>%
  rename(
    `Effect` = Effect,
    `Estimate` = Estimate,
    `SE` = SE,
    `p-value` = p_value,
    `Std. Estimate` = Std_Estimate,
    `Std. SE` = Std_SE,
    `ICC` = ICC,
    `R² Within` = R2_within,
    `R² Between` = R2_between
  )

#'## Linear Mixed Effects BFLPE Model
#'
# Print the LME Summary Table
lmeSummary %>% 
  kable(format = "html", digits = 3, 
        caption = "Linear Mixed Effects Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 3, "Unstandardized" = 3, "Standardized" = 2, "Model Metrics" = 3))

### 8.2 Structural Equation Model Results

# Prepare the SEM Summary Table
semSummary <- semSummaryTable %>%
  rename(
    `Effect` = Effect,
    `Estimate` = Estimate,
    `SE` = SE,
    `p-value` = p_value,
    `Std. Estimate` = Std_Estimate,
    `Std. SE` = Std_SE,
    `Chi-Square` = ChiSq,
    `df` = df,
    `Chi-Square p` = ChiSq_p,
    `CFI` = CFI,
    `RMSEA` = RMSEA,
    `SRMR` = SRMR
  )

#'## Structural Equation BFLPE Model
# Print the SEM Summary Table
semSummary %>% 
  kable(format = "html", digits = 3,
        caption = "Structural Equation Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 3, "Unstandardized" = 3, "Standardized" = 2, "Model Fit" = 6))

#'## BFLPE SEM Diagrams
## 9. Plot SEM Diagrams

for (domain in domains) {
  for (centering in centerings) {
    key <- paste(domain, centering, sep = "_")
    
    # Check if SEM Fit Exists
    if (!key %in% names(semResults)) {
      warning(sprintf("SEM results for %s not found. Skipping plot.", key))
      next
    }
    
    # Retrieve SEM Fit Object (semFitSem)
    semFitSem <- semResults[[key]]$semFitSem
    
    # Plot SEM Diagram using semFitSem
    semPaths(semFitSem, 
             whatLabels = "std", 
             layout = "tree2", 
             rotation = 3, 
             residuals = FALSE, 
             edge.label.cex = 1,
             nCharNodes = 0, 
             sizeMan = 8, 
             sizeLat = 8,
             title = FALSE,
             fade = FALSE)
    
    # Add Title
    title(main = sprintf("BFLPE Model - %s (%s Centering)", domain, centering))
  }
}

#'## Summary of Sample Sizes

# Extract Sample Sizes for Each Group-Centering Combination
sampleSizes <- map_dfr(allResults, function(r) {
  tibble(
    Domain = r$domain,
    Centering = r$centering,
    `N Students` = r$nStudents,
    `N Classes` = r$nClasses,
    `Avg Class Size` = round(r$avgClassSize, 1)
  )
})

# Print Sample Size Summary Table
sampleSizes %>%
  kable(format = "html", digits = 1, 
        caption = "Sample Size Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################
#'# BFLPE Moderators
# 
# Moderacia vztahu medzi class-level achievement a individualnym selfkonceptom: (1) priemernou znamkou (2) variabilitou znamok (znamky D8_a MAT, D8_b CJ)
# SES, gender ratio (D1 1 = female, 2 = male), ci spravil prijimacky (D6, 1 = ano, 2 = ne), sense of belonging (A1, inverzne polozky), supportive climate (C4a cestina C4b matematika, budto vsetky polozky alebo c(a c d f i h), nie su inverzne),
# Akademicka marnost (B5_1:B5_5), Fixny/growth mindset (B5_6, B5_7)
# 
# TODO: Ktore polozky su inverzne pri Sense of Belonging


# Helper function for group/grand mean centering
createCenteredData <- function(data, domain = c("VERB", "MATH", "OVERALL"), centeringType = c("group", "grand")) {
  centeringType <- match.arg(centeringType)
  domain <- match.arg(domain)
  
  # Select appropriate achievement variable based on domain
  achVar <- switch(domain,
                   VERB = "IRT_CJ",
                   MATH = "IRT_M",
                   OVERALL = "IRT_ACH"
  )
  
  # Create class-level variable name
  classVar <- paste0(achVar, "_class")
  
  # Get relevant variables for the domain
  indVars <- switch(domain,
                    VERB = c("B2_1", "B2_4", "B2_7", "B2_10"),
                    MATH = c("B2_2", "B2_5", "B2_9", "B2_11"),
                    OVERALL = c("B2_3", "B2_6", "B2_8")
  )
  
  # Create OVERALL achievement if needed
  if(domain == "OVERALL" && !"IRT_ACH" %in% names(data)) {
    data$IRT_ACH <- principal(data[, c("IRT_CJ", "IRT_M")], 
                              nfactors = 1, 
                              rotate = "none")$scores[, 1]
  }
  
  # Get all variables needed for analysis
  vars_needed <- c(indVars, achVar, "IDclass")
  
  # Filter for complete cases on necessary variables
  filtered_data <- data %>%
    ungroup() %>%
    mutate(missingProp = rowMeans(is.na(.))) %>%
    filter(missingProp < 0.8) %>%
    select(-missingProp)
  
  # Filter for minimum class size
  filtered_data <- filtered_data %>%
    group_by(IDclass) %>%
    mutate(classSize = n()) %>%
    filter(classSize >= 15) %>%
    ungroup()
  
  # First stage of SAM: Fit CFA (measurement model)
  measModel <- paste0("SC", domain, " =~ ", paste(indVars, collapse = " + "))
  fit.meas <- cfa(measModel, data = filtered_data, missing = "fiml", estimator = "MLR", sampling.weights = "STUWGT")
  
  # Get predicted factor scores
  filtered_data$score <- lavPredict(fit.meas, transform = TRUE)
  
  # Now do the centering on filtered data
  if(centeringType == "group") {
    filtered_data <- filtered_data %>%
      group_by(IDclass) %>%
      mutate(
        # Create class mean
        !!classVar := mean(get(achVar), na.rm = TRUE),
        # Create group-mean centered individual achievement
        !!achVar := get(achVar) - get(classVar)
      ) %>%
      ungroup()
  } else {
    # Grand mean centering
    grand_mean <- mean(filtered_data[[achVar]], na.rm = TRUE)
    filtered_data <- filtered_data %>%
      group_by(IDclass) %>%
      mutate(
        # Create class mean
        !!classVar := mean(get(achVar), na.rm = TRUE),
        # Create grand-mean centered individual achievement
        !!achVar := get(achVar) - grand_mean
      ) %>%
      ungroup()
  }
  
  return(filtered_data)
}

# Function to create moderator variables
createModerators <- function(data) {
  data %>%
    mutate(
      # Grade-related moderators
      avgGrade = rowMeans(across(c(D8_a, D8_b), .names = "grade_{.col}"), na.rm = TRUE),
      varGrade = apply(across(c(D8_a, D8_b)), 1, var, na.rm = TRUE),
      
      # PCA-based moderators
      senseBelonging = principal(across(matches("A1_")), nfactors = 1, rotate = "none")$scores[, 1],
      supportClimLang = principal(across(matches("C4a_")), nfactors = 1, rotate = "none")$scores[, 1],
      supportClimMath = principal(across(matches("C4b_")), nfactors = 1, rotate = "none")$scores[, 1],
      acadFutil = principal(across(starts_with("B5_")), nfactors = 1, rotate = "none")$scores[, 1],
      
      # Center all moderators within classes
      across(c(avgGrade, varGrade, SES, senseBelonging, supportClimLang, 
               supportClimMath, acadFutil, B5_6, B5_7, D8askew, D8bskew),
             list(centered = ~scale(., center = TRUE, scale = FALSE)),
             .names = "{.col}_centered")
    )
}


# Function to fit moderated mixed models
fitModeratedMixed <- function(data, moderator, domain = c("VERB", "MATH", "OVERALL")) {
  domain <- match.arg(domain)
  
  # Select appropriate achievement variables based on domain
  achVar <- switch(domain,
                   VERB = "IRT_CJ",
                   MATH = "IRT_M",
                   OVERALL = "IRT_ACH"
  )
  
  # Get class-level variable name
  classVar <- paste0(achVar, "_class")
  
  # Use score as dependent variable (from CFA predictions)
  # Create interaction term
  data[[paste0(classVar, "_", moderator)]] <- data[[classVar]] * data[[moderator]]
  
  # Fit model with interaction
  formula <- as.formula(paste("score ~", 
                              achVar, "+",
                              classVar, "*", moderator, "+",
                              "(1|IDclass)"))
  
  model <- lmer(formula, data = data)
  
  # Calculate ICC
  icc <- performance::icc(model)$ICC_adjusted
  
  # Get standardized coefficients
  std_results <- standardize_mixed_model(model, data)
  
  return(list(
    model = model,
    icc = icc,
    std_coef = std_results$std_coef,
    std_se = std_results$std_se,
    r2_within = std_results$r2_within,
    r2_between = std_results$r2_between
  ))
}

# Function to fit CFA for each domain with WLSMV
fitCFA <- function(data, domain = c("VERB", "MATH", "OVERALL")) {
  domain <- match.arg(domain)
  
  # Define measurement models
  models <- list(
    VERB = 'SCVERB =~ B2_1 + B2_4 + B2_7 + B2_10',
    MATH = 'SCMATH =~ B2_2 + B2_5 + B2_9 + B2_11',
    OVERALL = 'SCACAD =~ B2_3 + B2_6 + B2_8'
  )
  
  # Define ordered variables for each domain
  ordered_vars <- list(
    VERB = c("B2_1", "B2_4", "B2_7", "B2_10"),
    MATH = c("B2_2", "B2_5", "B2_9", "B2_11"),
    OVERALL = c("B2_3", "B2_6", "B2_8")
  )
  
  fit <- cfa(models[[domain]], 
             data = data,
             ordered = ordered_vars[[domain]],
             estimator = "WLSMV",
             sampling.weights = "STUWGT")
  #fit <- lavaan.survey(lavaan.fit = fit, survey.design = design)
  return(fit)
}

# Standardize coefs for mixed model
standardize_mixed_model <- function(model, data) {
  # Get fixed effects
  fixed_effects <- fixef(model)
  
  # Get variance components
  vc <- VarCorr(model)
  random_var <- as.numeric(vc$IDclass[1])
  residual_var <- sigma(model)^2
  
  # Get predictor standard deviations
  pred_names <- names(fixed_effects)[-1]  # exclude intercept
  pred_sds <- sapply(pred_names, function(var) {
    sd(data[[var]], na.rm = TRUE)
  })
  
  # Get outcome standard deviation
  outcome_sd <- sd(data$score, na.rm = TRUE)
  
  # Compute standardized coefficients
  std_coef <- fixed_effects[-1] * pred_sds / outcome_sd
  
  # Compute standard errors for standardized coefficients
  vcov_matrix <- vcov(model)[-1, -1]
  std_se <- sqrt(diag(vcov_matrix)) * abs(pred_sds / outcome_sd)
  
  # R-squared calculations
  var_explained_l1 <- var(predict(model, re.form = NA), na.rm = TRUE)
  r2_within <- var_explained_l1 / (var_explained_l1 + residual_var)
  
  var_explained_l2 <- var(ranef(model)$IDclass[[1]], na.rm = TRUE)
  r2_between <- var_explained_l2 / (var_explained_l2 + random_var)
  
  return(list(
    std_coef = std_coef,
    std_se = std_se,
    r2_within = r2_within,
    r2_between = r2_between
  ))
}

# Function to fit moderated SEM models
fitModeratedSEM <- function(data, moderator, domain = c("VERB", "MATH", "OVERALL")) {
  domain <- match.arg(domain)
  
  # Get appropriate achievement variable based on domain
  ach_var <- switch(domain,
                    VERB = "IRT_CJ",
                    MATH = "IRT_M",
                    OVERALL = "IRT_ACH"
  )
  
  # Define models with moderation
  models <- list(
    VERB = sprintf('
      # Measurement model
      SCVERB =~ B2_1 + B2_4 + B2_7 + B2_10
      
      # Structural model
      SCVERB ~ IRT_CJ + IRT_CJ_class + %s + IRT_CJ_class:%s
      
      # Class-level achievement
      # IRT_CJ_class ~ IRT_CJ
                   ', moderator, moderator),
    
    MATH = sprintf('
      # Measurement model
      SCMATH =~ B2_2 + B2_5 + B2_9 + B2_11
      
      # Structural model
      SCMATH ~ IRT_M + IRT_M_class + %s + IRT_M_class:%s
      
      # Class-level achievement
      # IRT_M_class ~ IRT_M
                   ', moderator, moderator),
    
    OVERALL = sprintf('
      # Measurement model
      SCACAD =~ B2_3 + B2_6 + B2_8
      
      # Structural model
      SCACAD ~ IRT_ACH + IRT_ACH_class + %s + IRT_ACH_class:%s
      
      # Class-level achievement
      # IRT_ACH_class ~ IRT_ACH
                      ', moderator, moderator)
  )
  
  # Fit the model
  fit <- sem(models[[domain]], 
             data = data,
             missing = "fiml",
             estimator = "MLR",
             cluster = "IDclass",
             sampling.weights = "STUWGT")
  #fit <- lavaan.survey(fit, survey.design = design)
  return(fit)
}

# Function to run moderation analysis
runModerationAnalysis <- function(data, moderator, domain, centeringType) {
  # 1. Prepare data with appropriate centering
  centeredData <- createCenteredData(data, domain, centeringType)
  
  # 2. Add moderators
  centeredData <- createModerators(centeredData)
  
  # 3. Fit CFA and get predicted scores
  cfaFit <- fitCFA(centeredData, domain)
  centeredData$score <- predict(cfaFit)
  
  # 4. Fit moderated mixed model - fixed order of arguments
  mixedResults <- fitModeratedMixed(centeredData, moderator, domain) # Removed "score"
  
  # 5. Fit moderated SEM
  semResults <- fitModeratedSEM(centeredData, moderator, domain)
  
  # Extract results
  mixed_coef <- fixef(mixedResults$model)
  mixed_se <- sqrt(diag(vcov(mixedResults$model)))
  
  sem_params <- parameterEstimates(semResults, standardized = TRUE)
  sem_struct <- sem_params[sem_params$op == "~", ]
  
  return(list(
    domain = domain,
    moderator = moderator,
    centering = centeringType,
    mixed = list(
      coef = mixed_coef,
      se = mixed_se,
      icc = mixedResults$icc,
      std_coef = mixedResults$std_coef,
      std_se = mixedResults$std_se,
      r2_within = mixedResults$r2_within,
      r2_between = mixedResults$r2_between
    ),
   sem = list(
      coef = sem_params,
      fit = fitMeasures(semResults, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                    "cfi", "rmsea", "srmr"))
    ),
    n_classes = length(unique(centeredData$IDclass)),
    avg_class_size = mean(table(centeredData$IDclass)),
    n_students = nrow(centeredData)
  ))
}

# Create summary tables for moderated LME models
createModeratedLMESummaryTable <- function(results) {
  # Create moderator label mapping
  mod_labels <- c(
    "avgGrade" = "Average Grade",
    "varGrade" = "Grade Variance",
    "SES" = "SES",
    "D1" = "Gender",
    "gender.prop" = "Gender Proportion",
    "D6" = "Admission success",
    "vg.prop" = "Track Proportion",
    "senseBelonging" = "Sense of Belonging",
    "supportClimLang" = "Support Climate Lang",
    "supportClimMath" = "Support Climate Math",
    "acadFutil" = "Academic Futility",
    "B5_6" = "Fixed Mindset 1",
    "B5_7" = "Growth Mindset 2",
    "D8askew" = "Grade Skew Math",
    "D8bskew" = "Grade Skew Lang"
  )
  
  # LME effects table
  lme_effects <- map_dfr(results, function(r) {
    # Get coefficients (excluding intercept)
    mixed_coef <- r$mixed$coef[-1]  # Remove intercept
    mixed_se <- r$mixed$se[-1]      # Remove intercept SE
    mixed_p <- 2 * (1 - pnorm(abs(mixed_coef / mixed_se)))
    
    tibble(
      Domain = r$domain,
      Moderator = paste0(mod_labels[r$moderator], " (", r$moderator, ")"),
      Centering = r$centering,
      Effect = c("Individual", "Contextual", "Moderator", "Interaction"),
      Estimate = mixed_coef,
      SE = mixed_se,
      p_value = mixed_p,
      Std_Estimate = r$mixed$std_coef,
      Std_SE = r$mixed$std_se,
      ICC = rep(r$mixed$icc, 4),
      R2_within = rep(r$mixed$r2_within, 4),
      R2_between = rep(r$mixed$r2_between, 4)
    )
  })
  
  # Format LME table
  lme_formatted <- lme_effects %>%
    mutate(
      p_value = ifelse(p_value < .001, "< .001", sprintf("%.3f", p_value)),
      Estimate = sprintf("%.3f", Estimate),
      SE = sprintf("%.3f", SE),
      Std_Estimate = sprintf("%.3f", Std_Estimate),
      Std_SE = sprintf("%.3f", Std_SE),
      ICC = sprintf("%.3f", ICC),
      R2_within = sprintf("%.3f", R2_within),
      R2_between = sprintf("%.3f", R2_between)
    ) %>%
    arrange(Domain, Centering, Moderator)
  
  return(list(
    table = lme_formatted,
    raw = lme_effects
  ))
}

createModeratedSEMSummaryTable <- function(results) {
  # Create moderator label mapping
  mod_labels <- c(
    "avgGrade" = "Average Grade",
    "varGrade" = "Grade Variance",
    "SES" = "SES",
    "D1" = "Gender",
    "vg.prop" = "Track Proportion",
    "D6" = "Admission success",
    "senseBelonging" = "Sense of Belonging",
    "supportClimLang" = "Support Climate Lang",
    "supportClimMath" = "Support Climate Math",
    "acadFutil" = "Academic Futility",
    "B5_6" = "Fixed Mindset 1",
    "B5_7" = "Growth Mindset 2",
    "D8askew" = "Grade Skew Math",
    "D8bskew" = "Grade Skew Lang"
  )
  
  # Create empty list to store all results
  all_effects <- list()
  
  # Counter for results
  counter <- 1
  
  # Process each result
  for(r in results) {
    sem_params <- r$sem$coef[r$sem$coef$op == "~", ]
    
    # For each result, create two rows: one for individual effects, one for contextual effects
    effects_tbl <- tibble(
      Domain = r$domain,
      Moderator = paste0(mod_labels[r$moderator], " (", r$moderator, ")"),
      Centering = r$centering,
      Effect = c("Individual", "Contextual", "Moderator", "Interaction"),
      Estimate = sem_params$est,
      SE = sem_params$se,
      p_value = sem_params$pvalue,
      Std_Estimate = sem_params$std.all,
      Std_SE = sem_params$se
    )
    
    # Get fit measures
    fit_measures <- r$sem$fit
    
    # Add fit measures
    effects_tbl <- effects_tbl %>%
      mutate(
        ChiSq = fit_measures["chisq.scaled"],
        df = fit_measures["df.scaled"],
        ChiSq_p = fit_measures["pvalue.scaled"],
        CFI = fit_measures["cfi"],
        RMSEA = fit_measures["rmsea"],
        SRMR = fit_measures["srmr"]
      )
    
    all_effects[[counter]] <- effects_tbl
    counter <- counter + 1
  }
  
  # Combine all results
  sem_effects <- bind_rows(all_effects)
  
  # Format table
  sem_formatted <- sem_effects %>%
    mutate(
      p_value = ifelse(p_value < .001, "< .001", sprintf("%.3f", p_value)),
      Estimate = sprintf("%.3f", Estimate),
      SE = sprintf("%.3f", SE),
      Std_Estimate = sprintf("%.3f", Std_Estimate),
      Std_SE = sprintf("%.3f", Std_SE),
      ChiSq = sprintf("%.2f", ChiSq),
      ChiSq_p = ifelse(ChiSq_p < .001, "< .001", sprintf("%.3f", ChiSq_p)),
      CFI = sprintf("%.3f", CFI),
      RMSEA = sprintf("%.3f", RMSEA),
      SRMR = sprintf("%.3f", SRMR)
    ) %>%
    arrange(Domain, Centering, Moderator, Effect)
  
  return(list(
    table = sem_formatted,
    raw = sem_effects
  ))
}

# Run moderation analyses
moderators <- c("avgGrade", "varGrade", "SES", "D1", "gender.prop", "D6", 
                "vg.prop", "senseBelonging", "supportClimLang", "supportClimMath", 
                "acadFutil", "B5_6", "B5_7", "D8askew", "D8bskew")

domains <- c("VERB", "MATH", "OVERALL")
centerings <- c("group", "grand")
moderation_results <- list()

#+ include = FALSE
for(domain in domains) {
  for(centering in centerings) {
    for(moderator in moderators) {
      #message(sprintf("Running analysis for %s domain with %s centering and %s moderator", 
      #                domain, centering, moderator))
      key <- paste(domain, centering, moderator)
      moderation_results[[key]] <- runModerationAnalysis(dat, moderator, domain, centering)
    }
  }
}

#+ include = TRUE
# Generate and print summary tables
lmeSummary <- createModeratedLMESummaryTable(moderation_results)
semSummary <- createModeratedSEMSummaryTable(moderation_results)

#'## BFLPE Moderation LME results
lmeSummary$table %>%
  kable(format = "html", digits = 3, 
        caption = "Moderated Linear Mixed Effects Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 4, "Unstandardized" = 3, "Standardized" = 2, "Model Fit" = 3))
#'## BFLPE Moderation SEM results
semSummary$table %>%
  kable(format = "html", digits = 3,
        caption = "Structural Equation Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 4, "Unstandardized" = 3, "Standardized" = 2, "Model Fit" = 6))

#'## Plots
# Prepare data for visualization
sem_moderation_plot_data <- semSummary$raw %>%
  # Keep only contextual effects interactions
  filter(Effect == "Interaction") %>%
  # Create significance indicator
  mutate(
    significant = p_value < .05,
    # Create error bars
    lower = Estimate - 1.96*SE,
    upper = Estimate + 1.96*SE,
    # Format moderator names for plotting
    Moderator = factor(Moderator)
  )

# Prepare data for visualization, excluding gender proportion
sem_moderation_plot_data <- semSummary$raw %>%
  # Keep only contextual effects interactions and exclude gender.prop
  filter(Effect == "Interaction", 
         Moderator != "NA (gender.prop)",
         Moderator != "Track Proportion (vg.prop)") %>%
  # Create significance indicator
  mutate(
    significant = p_value < .05,
    # Create error bars
    lower = Estimate - 1.96*SE,
    upper = Estimate + 1.96*SE,
    # Format moderator names for plotting
    Moderator = factor(Moderator)
  )

#'### BFLPE moderation effect magnitudes
ggplot(sem_moderation_plot_data, 
       aes(x = Moderator, y = Estimate, color = Domain)) +
  # Add horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Add error bars
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  # Add points
  geom_point(aes(shape = significant), 
             size = 3, position = position_dodge(width = 0.5)) +
  # Rotate x-axis labels for readability
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  # Labels
  labs(y = "BFLPE Moderation Effect",
       x = "Moderator",
       color = "Domain",
       shape = "p < .05") +
  # Facet by centering
  facet_wrap(~Centering)

#'### Heatmap of moderation effects magnitudes
ggplot(sem_moderation_plot_data, 
       aes(x = Domain, y = Moderator, fill = Estimate)) +
  geom_tile() +
  # Add effect size values
  geom_text(aes(label = sprintf("%.3f", Estimate),
                color = abs(Estimate) > max(abs(Estimate))/2),
            size = 3) +
  # Separate by centering
  facet_wrap(~Centering) +
  # Custom color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(fill = "Moderation\nEffect",
       x = "Domain",
       y = "Moderator")

#'### Summary statistics of moderation effects
moderation_summary <- sem_moderation_plot_data %>%
  group_by(Domain, Centering) %>%
  summarize(
    n_significant = sum(significant),
    mean_effect = mean(Estimate),
    sd_effect = sd(Estimate),
    min_effect = min(Estimate),
    max_effect = max(Estimate),
    strongest_moderator = Moderator[which.max(abs(Estimate))]
  ) %>%
  ungroup()

# Print summary
kable(moderation_summary, 
            caption = "Summary of BFLPE Moderation Effects",
            format = "html") %>%
        kable_styling(bootstrap_options = "striped")


####################################################
####################################################
# Graveyard

# # Generate a plot for each class
# plots <- lapply(class_ids, function(class_id) {
#   class_data <- data_long[data_long$IDclass == class_id, ]
#   
#   p <- ggplot(class_data, aes(x = value, fill = variable)) +
#     geom_histogram(position = "dodge", binwidth = 1, color = "black") +  # Adjust binwidth as necessary
#     labs(title = paste("D8_a and D8_b for Class", class_id),
#          x = "Value",
#          y = "Frequency") +
#     scale_fill_manual(values = c("blue", "green")) +  # Colors for D8_a and D8_b
#     theme_minimal()
#   
#   print(p)  # This will output the plot in the console or viewer
# })
# 
# # If you want to save plots, you can use ggsave here inside the loop or after each plot is created
# 
# ################################################
# ################################################
# #'## Alternative latent structures
# #'
# #'### Unitary factor model
# 
# modelUnitary <- '
# unitaryFactor =~ B1_3 + B1_8 + B1_12 + B1_4 + B1_7 + B1_11 + B1_15 + B1_5 + B1_10 + B1_14 + B1_2 + B1_6 + B1_9 + B1_13 + B2_1 + B2_4 + B2_7 + B2_10 +B2_2 + B2_5 + B2_9 + B2_11 + B2_3 + B2_6 + B2_8'
# 
# fitUnitary <- cfa(model = modelUnitary, data = dat, meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#            estimator = "WLSMVS", test = "Satterthwaite", bootstrap = nboot, sampling.weights = "STUWGT",
#            ordered = names(dat)[1:26])
# 
# # # Specification of the sampling design.
# # # The model applies the sampling weights (vaha6) and accounts for the two-level hierarchical structure of the data (children nested within classes nested within schools).
# design <- svydesign(ids = ~ school + cluster, data = data[!is.na(data$cluster),], weights = ~vaha6)
# fitted.modelUnitary <- lavaan.survey(lavaan.fit = fitUnitary, survey.design = design, estimator = "MLMVS")
# 
# #'#### Model test and approximate fit indices
# (cfa.fitUnitary <- fitmeasures(fitted.modelUnitary, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic")))
# 
# #'#### Mean factor loading
# mean(inspect(fitted.modelUnitary, what="std")$lambda)
# max(inspect(fitted.modelUnitary, what="std")$lambda)
# 
# #'#### $χ^{2}$ model difference test
# anova(fitted.model, fitted.modelUnitary)
# 
# #'#### Diagram
# semPaths(fitted.modelUnitary, style = "mx", edge.label.cex = 0.7,
#          sizeLat = 7, nCharNodes = 0, nDigits = 2, "Standardized",
#          intercepts = FALSE, residuals = FALSE, what = "path",
#          edge.label.position = .5, layout = "circle",
#          node.width = 1.1, color = "white", thresholds = FALSE)
# 
# #'### Higher-order unitary factor model
# 
# modelHigherOrder <- '
# INSMOT =~ C6_B1C + C6_B1H + C6_B1L
# EFFPER =~ C6_B1D + C6_B1G + C6_B1K + C6_B1O
# SELFEF =~ C6_B1A + C6_B1E + C6_B1J + C6_B1N
# CEXP =~ C6_B1B + C6_B1F + C6_B1I + C6_B1M
# INTREA =~ C6_B2E + C6_B2K + C6_B2N
# INTMAT =~ C6_B2A + C6_B2H + C6_B2Q
# COMLRN =~ C6_B2C + C6_B2I + C6_B2M + C6_B2S
# SCVERB =~ C6_B2G + C6_B2D + C6_B2R
# SCMATH =~ C6_B2J + C6_B2L + C6_B2O
# SCACAD =~ C6_B2B + C6_B2F + C6_B2P
# C6_B2L ~~ C6_B2H
# G =~ INSMOT + EFFPER + SELFEF + CEXP + INTREA + INTMAT + COMLRN + SCVERB + SCMATH + SCACAD
# '
# 
# fitHigherOrder <- cfa(model = modelHigherOrder, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                    estimator = "WLSMVS", test = "Satterthwaite", orthogonal = TRUE, bootstrap = nboot,
#                    ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                                "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                                "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                                "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                                "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                                "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                                "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
# # Specification of the sampling design.
# # The model applies the sampling weights (vaha6) and accounts for the two-level hierarchical structure of the data (children nested within classes nested within schools).
# design <- svydesign(ids = ~ school + cluster, data = data[!is.na(data$cluster),], weights = ~vaha6)
# #summary(design)
# fitted.modelHigherOrder <- lavaan.survey(lavaan.fit = fitHigherOrder, survey.design = design, estimator = "MLMVS")
# 
# #'#### Model test and approximate fit indices
# cfa.fitHigherOrder <- fitmeasures(fitted.modelHigherOrder, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# cfa.fitHigherOrder
# 
# #'#### Mean factor loading
# mean(inspect(fitted.modelHigherOrder, what="std")$lambda)
# max(inspect(fitted.modelHigherOrder, what="std")$lambda)
# 
# #'#### $χ^{2}$ model difference test
# #'
# # primary model vs higher-order model
# anova(fitted.model, fitted.modelHigherOrder)
# 
# #'#### Diagram
# semPaths(fitted.modelBifactor, style = "mx", edge.label.cex = 0.7,
#          sizeLat = 7, nCharNodes = 0, nDigits = 2, "Standardized",
#          intercepts = FALSE, residuals = FALSE, what = "path",
#          edge.label.position = .5,
#          node.width = 1.1, color = "white", thresholds = FALSE)
# 
# #'### Higher-order two-factor model
# #'
# modelSecondOrder <- '
# INSMOT =~ C6_B1C + C6_B1H + C6_B1L
# EFFPER =~ C6_B1D + C6_B1G + C6_B1K + C6_B1O
# SELFEF =~ C6_B1A + C6_B1E + C6_B1J + C6_B1N
# CEXP =~ C6_B1B + C6_B1F + C6_B1I + C6_B1M
# INTREA =~ C6_B2E + C6_B2K + C6_B2N
# INTMAT =~ C6_B2A + C6_B2H + C6_B2Q
# COMLRN =~ C6_B2C + C6_B2I + C6_B2M + C6_B2S
# SCVERB =~ C6_B2G + C6_B2D + C6_B2R
# SCMATH =~ C6_B2J + C6_B2L + C6_B2O
# SCACAD =~ C6_B2B + C6_B2F + C6_B2P
# motivation =~ INSMOT + EFFPER + SELFEF + CEXP + INTREA + INTMAT + COMLRN
# selfConcept =~ SCVERB + SCMATH + SCACAD
# motivation ~~ selfConcept
# C6_B2L ~~ C6_B2H
# '
# 
# fitSecondOrder <- cfa(model = modelSecondOrder, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                       estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#                       ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                                   "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                                   "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                                   "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                                   "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                                   "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                                   "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
# # Specification of the sampling design.
# # The model applies the sampling weights (vaha6) and accounts for the two-level hierarchical structure of the data (children nested within classes nested within schools).
# design <- svydesign(ids = ~ school + cluster, data = data[!is.na(data$cluster),], weights = ~vaha6)
# #summary(design)
# fitted.modelSecondOrder <- lavaan.survey(lavaan.fit = fitSecondOrder, survey.design = design, estimator = "MLMVS")
# 
# #'#### Model test and approximate fit indices
# cfa.secondOrder <- fitmeasures(fitted.modelSecondOrder, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# cfa.secondOrder
# 
# #'#### $χ^{2}$ model difference test
# anova(fitted.modelSecondOrder, fitted.model)
# 
# #'### Bifactor model
# 
# modelBifactor <- '
# G =~ C6_B1C + C6_B1H + C6_B1L + C6_B1D + C6_B1G + C6_B1K + C6_B1O + C6_B1A + C6_B1E + C6_B1J + C6_B1N +
# C6_B1B + C6_B1F + C6_B1I + C6_B1M + C6_B2E + C6_B2K + C6_B2N + C6_B2A + C6_B2H + C6_B2Q + C6_B2C + C6_B2I + C6_B2M + C6_B2S +
# C6_B2G + C6_B2D + C6_B2R + C6_B2J + C6_B2L + C6_B2O + C6_B2B + C6_B2F + C6_B2P
# INSMOT =~ C6_B1C + C6_B1H + C6_B1L
# EFFPER =~ C6_B1D + C6_B1G + C6_B1K + C6_B1O
# SELFEF =~ C6_B1A + C6_B1E + C6_B1J + C6_B1N
# CEXP =~ C6_B1B + C6_B1F + C6_B1I + C6_B1M
# INTREA =~ C6_B2E + C6_B2K + C6_B2N
# INTMAT =~ C6_B2A + C6_B2H + C6_B2Q
# COMLRN =~ C6_B2C + C6_B2I + C6_B2M + C6_B2S
# SCVERB =~ C6_B2G + C6_B2D + C6_B2R
# SCMATH =~ C6_B2J + C6_B2L + C6_B2O
# SCACAD =~ C6_B2B + C6_B2F + C6_B2P
# C6_B2L ~~ C6_B2H
# '
# 
# fitBifactor <- cfa(model = modelBifactor, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                    estimator = "WLSMVS", test = "Satterthwaite", orthogonal = TRUE, bootstrap = nboot,
#                    ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                                "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                                "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                                "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                                "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                                "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                                "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
# # Specification of the sampling design.
# # The model applies the sampling weights (vaha6) and accounts for the two-level hierarchical structure of the data (children nested within classes nested within schools).
# design <- svydesign(ids = ~ school + cluster, data = data[!is.na(data$cluster),], weights = ~vaha6)
# #summary(design)
# fitted.modelBifactor <- lavaan.survey(lavaan.fit = fitBifactor, survey.design = design, estimator = "MLMVS")
# 
# #'#### Model test and approximate fit indices
# cfa.fitBifactor <- fitmeasures(fitted.modelBifactor, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# cfa.fitBifactor
# 
# #'#### Mean factor loading
# mean(inspect(fitted.modelBifactor, what="std")$lambda)
# max(inspect(fitted.modelBifactor, what="std")$lambda)
# 
# #'#### $χ^{2}$ model difference test
# #'
# # primary model vs bifactor model
# anova(fitted.model, fitted.modelBifactor)
# # bifactor model vs two-factor higher-order model
# anova(fitted.modelBifactor, fitted.modelSecondOrder)
# 
# #'#### Diagram
# semPaths(fitted.modelBifactor, style = "mx", edge.label.cex = 0.7,
#          sizeLat = 7, nCharNodes = 0, nDigits = 2, "Standardized",
#          intercepts = FALSE, residuals = FALSE, what = "path",
#          edge.label.position = .5,
#          node.width = 1.1, color = "white", thresholds = FALSE)
# 
# 
# #'#### Testing the more parsimonious unitary structure in more homogenous groups
# #'
# 
# # **Basic school students**
# fitUnitaryBasic <- cfa(model = modelUnitary, data = data[!is.na(data$cluster) & data$VG.ZS == "ZS",], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                   estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#                   ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                               "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                               "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                               "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                               "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                               "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                               "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
# fitted.modelUnitaryBasic <- lavaan.survey(lavaan.fit = fitUnitaryBasic, survey.design = design, estimator = "MLMVS")
# 
# # Model test and approximate fit indices
# cfa.fitUnitaryBasic <- fitmeasures(fitted.modelUnitaryBasic, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# cfa.fitUnitaryBasic
# 
# # **8-year gymnasia**
# fitUnitary8yg <- cfa(model = modelUnitary, data = data[!is.na(data$cluster) & data$VG.ZS == "VG",], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                        estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#                        ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                                    "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                                    "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                                    "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                                    "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                                    "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                                    "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
# fitted.modelUnitary8yg <- lavaan.survey(lavaan.fit = fitUnitary8yg, survey.design = design, estimator = "MLMVS")
# 
# # Model test and approximate fit indices
# cfa.fitUnitary8yg <- fitmeasures(fitted.modelUnitary8yg, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# cfa.fitUnitary8yg
# 
# # **SES quintiles**
# cfa.fit.resultsSES <- list(NA)
# for(i in 1:5){
#   fitUnitary <- cfa(model = modelUnitary, data = data[!is.na(data$cluster) & data$SES_quint_child == i,], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                        estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#                        ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                                    "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                                    "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                                    "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                                    "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                                    "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                                    "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# 
#   fitted.modelUnitary <- lavaan.survey(lavaan.fit = fitUnitary, survey.design = design, estimator = "MLMVS")
# 
#   # Model test and approximate fit indices
#   cfa.fit.resultsSES[[i]] <- fitmeasures(fitted.modelUnitary, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# }
# 
# cfa.fit.resultsSES
# 
# #'# Reliability
# #'Internal consistency estimates for individual scales - McDonald's Omega
# #'
# # The scales show very good overall reliability (especially given the number of items per scale; 3-4). These reliabilities are amongst the highest with respect to countries included in Marsh et al. study.
# #'
# #+ warning = FALSE
# INSMOT.rel <- omega(dat[,c("B1_3", "B1_8", "B1_12")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# EFFPER.rel <- omega(dat[,c("B1_4", "B1_7", "B1_11", "B1_15")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# SELFEF.rel <- omega(dat[,c("B1_10", "B1_5", "B1_14")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# CEXP.rel <- omega(dat[,c("B1_2", "B1_6", "B1_9", "B1_13")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# SCVERB.rel <- omega(dat[,c("B2_1", "B2_4", "B2_7", "B2_10")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# SCMATH.rel <- omega(dat[,c("B2_2", "B2_5", "B2_9", "B2_11")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# SCACAD.rel <- omega(dat[,c("B2_3", "B2_6", "B2_8")], poly = FALSE, plot = FALSE, covar = T, fm = "ml")$omega.tot
# 
# mean.rel <- mean(c(INSMOT.rel, EFFPER.rel, SELFEF.rel, CEXP.rel,
#                  SCVERB.rel, SCMATH.rel, SCACAD.rel), na.rm = T)
# sd.rel <- sd(c(INSMOT.rel, EFFPER.rel, SELFEF.rel, CEXP.rel,
#                SCVERB.rel, SCMATH.rel, SCACAD.rel))
# 
# result.rel <- data.frame("Reliability.Omega" = c(INSMOT.rel, EFFPER.rel, SELFEF.rel, CEXP.rel,
#                                                  SCVERB.rel, SCMATH.rel, SCACAD.rel, mean.rel, sd.rel))
# 
# rownames(result.rel) <- c("INSMOT", "EFFPER", "SELFEF", "CEXP", "SCVERB", "SCMATH", "SCACAD", "Mean scale reliability", "SD of scale reliabilities")
# kable(result.rel, "html", digits = 2) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12, position = "left") %>%
#   row_spec(8:9, bold = T, color = "white", background = "blue")
# 
# # #'# Measurement invariance
# # # Invariance with respect to gender
# # #'
# # # The measure shows excellent measurement invariance when fixing loadings and good invariance even when fixing intercepts of items. The instrument thus measures the same constructs equaly well in both genders, i.e., any differences are likely real, not due to how the LVs were measured.
# # inv.gender <- measurementInvariance(model = model, data = data[!is.na(data$C6_F2),], group = "C6_F2",
# #                       estimator = "wlsmv", parameterization = "theta", fit.measures = c("cfi.scaled", "rmsea.scaled"),
# #                       ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                                   "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                                   "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                                   "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                                   "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                                   "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                                   "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# #'
# # #'# Comparing genders on latent means
# # # Constraining loadings and latent intercepts to be equal. Fixing the factor means of the first/reference group (Gender == 1) to zero
# # # while estimating the factor means for the other group. These magnitudes equal the difference between the groups.
# # #'
# # fit.means <- cfa(model = model, data = data[!is.na(data$cluster),], meanstructure = TRUE, std.lv = FALSE, mimic = "Mplus",
# #            estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #            group = "C6_F2", group.equal = c("loadings", "intercepts"),
# #            ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                        "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                        "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                        "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                        "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                        "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                        "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# # #summary(fit.means)
# # design.no.hier <- svydesign(ids = ~1, data = data[!is.na(data$cluster),], weights = ~vaha6)
# # fitted.lat.means.diffs <- lavaan.survey(lavaan.fit = fit.means, survey.design = design.no.hier, estimator = "MLMVS")
# # lat.mean.diffs <- inspect(fitted.lat.means.diffs, what = "est")$`Group 2`$alpha
# # lat.mean.sd <- inspect(fitted.lat.means.diffs, what = "std.err")$`Group 2`$alpha
# # dES.gender <- lat.mean.diffs/(lat.mean.sd * sqrt(table(data.na.rm[!is.na(data.na.rm$vaha6),]$C6_F2)[2]))
# # CLES.gender <- round(d_to_common_language(dES.gender)$`Probability of superiority`*100, 0)
# #'
# # kable(cbind(dES.gender, CLES.gender), "html", digits = 2) %>%
# #   kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12, position = "left")
# #'
# # # All the differences in latent means apart from INSMOT, EFFPER, CEXP, and SCACAD are significant (see summary(fitted.lat.means.diffs, standardized = T). Negative intercept values denote higher mean values for girls and positive values higher mean values for the boys  (factors are scaled inversely).
# # # The girls show markedly higher interest in reading, boys higher interest in math. Girls report spending more effort and being more perseverant, while boys have higher values of self-eficacy and competitive learning. Girls show higher self-concept in language while the opposite is true for math. Results overall consistent with stereotype threat bias.
# # #'
# #'
# # #'# Comparing ZS/VG on latent means
# # # Constraining loadings and latent intercepts to be equal. Fixing the factor means of the first/reference group (ZS.VG == ZS) to zero
# # # while estimating the factor means for the other group (VG). These magnitudes equal the difference between the groups.
# # #'
# # #'Group 1 = ZS, Group 2 = VG
# #'
# # fit.means.VG.ZS <- cfa(model = model, data = data[!is.na(data$cluster),], meanstructure = TRUE, std.lv = FALSE, mimic = "Mplus",
# #                        estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                        group = "VG.ZS", group.equal = c("loadings", "intercepts"),
# #                        ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                                    "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                                    "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                                    "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                                    "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                                    "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                                    "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# # #summary(fit.means)
# # design.no.hier <- svydesign(ids = ~1, data = data[!is.na(data$cluster),], weights = ~vaha6)
# # fitted.lat.means.diffs.ZS.VG <- lavaan.survey(lavaan.fit = fit.means.VG.ZS, survey.design = design.no.hier, estimator = "MLMVS")
# # lat.mean.diffs.ZS.VG <- inspect(fitted.lat.means.diffs.ZS.VG, what = "est")$`Group 2`$alpha
# # lat.mean.diffs.ZS.VG.sd <- inspect(fitted.lat.means.diffs.ZS.VG, what = "std.err")$`Group 2`$alpha
# # dES.ZS.VG <- lat.mean.diffs.ZS.VG/(lat.mean.diffs.ZS.VG.sd * sqrt(table(data.na.rm[!is.na(data.na.rm$vaha6),]$C6_F2)[2]))*-1 #rescaled to make mainstream schools the reference group for the sake of this tabular presentation
# # CLES.ZS.VG <- round(d_to_common_language(dES.ZS.VG)$`Probability of superiority`*100, 0)
# #'
# # kable(cbind(dES.ZS.VG, CLES.ZS.VG), "html", digits = 2) %>%
# #   kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12, position = "left")
# #'
# # # Children at VG (eight-year academies) show more positive values (factors are scaled inversely) for all factors.
# # #'
# #'
# # ##########
# #'
# # #'# Measurement invariance
# # # Invariance with respect to gender
# # #'
# # measurementInvariance(model = model, data = data[!is.na(data$C6_F2),], group = "C6_F2",
# #                       estimator = "wlsmv", parameterization = "theta", fit.measures = c("cfi.scaled", "rmsea.scaled"),
# #                       ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                                   "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                                   "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                                   "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                                   "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                                   "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                                   "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# # # GENDER
# # #step1: configural invariance - estimation
# # config_fit <- cfa(model = model, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                   estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot, group = "C6_F2", cluster = "cluster")
# # survey.config_fit <- lavaan.survey(lavaan.fit = config_fit, survey.design = design.no.hier)
# #'
# # #step2: weak invariance - estimation
# #'
# # weak_fit <- cfa(model = model, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                  estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                  group = "C6_F2", group.equal = "loadings", cluster = "cluster")
# #'
# # survey.weak_fit <- lavaan.survey(lavaan.fit = weak_fit, survey.design = design.no.hier)
# #'
# # #step3: strong invariance - estimation
# # strong_fit <- cfa(model = model, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #     estimator = "MLR", orthogonal = FALSE, bootstrap = nboot,
# #     group = "C6_F2", group.equal = c("loadings", "intercepts"), cluster = "cluster"
# #     )
# #'
# # survey.strong_fit <- lavaan.survey(lavaan.fit = strong_fit, survey.design = design.no.hier)
# #'
# # #comparing nested models
# # anova(survey.config_fit, survey.weak_fit, SB.classic=TRUE) #weak invariance yes
# # anova(survey.weak_fit, survey.strong_fit, SB.classic=TRUE)  #strong invariance no, partial invariance?
# #'
# #'
# # #'# Measurement invariance
# # # Invariance with respect to SES quintiles
# # #'
# # inv.ses <- measurementInvariance(model = model, data = data[!is.na(data$SES_quint_child),], group = "SES_quint_child",
# #                       estimator = "wlsmv", parameterization = "theta", fit.measures = c("cfi.scaled", "rmsea.scaled"),
# #                       ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                                   "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                                   "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                                   "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                                   "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                                   "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                                   "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# # # SES quintiles
# # #step1: configural invariance - estimation
# # config_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$SES_quint_child),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                   estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot, group = "SES_quint_child", cluster = "cluster")
# # survey.config_fit <- lavaan.survey(lavaan.fit = config_fit, survey.design = design.no.hier)
# #'
# # #step2: weak invariance - estimation
# #'
# # weak_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$SES_quint_child),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                 estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                 group = "SES_quint_child", group.equal = "loadings", cluster = "cluster")
# #'
# # survey.weak_fit <- lavaan.survey(lavaan.fit = weak_fit, survey.design = design.no.hier)
# #'
# # #step3: strong invariance - estimation
# # strong_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$SES_quint_child),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                   estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                   group = "SES_quint_child", group.equal = c("loadings", "intercepts"), cluster = "cluster")
# #'
# # survey.strong_fit <- lavaan.survey(lavaan.fit = strong_fit, survey.design = design.no.hier)
# #'
# # #comparing nested models
# # anova(survey.config_fit, survey.weak_fit, SB.classic=TRUE) #weak invariance yes
# # anova(survey.weak_fit, survey.strong_fit, SB.classic=TRUE)  #strong invariance no, partial invariance?
# #'
# #'
# # #'# Measurement invariance
# # # Invariance with respect to type of school
# # #'
# #'
# # inv.school <- measurementInvariance(model = model, data = data[!is.na(data$SES_quint_child),], group = "VG.ZS",
# #                                  estimator = "wlsmv", parameterization = "theta", fit.measures = c("cfi.scaled", "rmsea.scaled"),
# #                                  ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
# #                                              "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
# #                                              "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
# #                                              "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
# #                                              "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
# #                                              "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
# #                                              "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# #'
# #'
# # #step1: configural invariance - estimation
# # config_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$VG.ZS),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                   estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot, group = "VG.ZS", cluster = "cluster")
# # survey.config_fit <- lavaan.survey(lavaan.fit = config_fit, survey.design = design.no.hier)
# #'
# # #step2: weak invariance - estimation
# #'
# # weak_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$VG.ZS),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                 estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                 group = "VG.ZS", group.equal = "loadings", cluster = "cluster")
# #'
# # survey.weak_fit <- lavaan.survey(lavaan.fit = weak_fit, survey.design = design.no.hier)
# #'
# # #step3: strong invariance - estimation
# # strong_fit <- cfa(model = model, data = data[!is.na(data$cluster) & !is.na(data$VG.ZS),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
# #                   estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
# #                   group = "VG.ZS", group.equal = c("loadings", "intercepts"), cluster = "cluster")
# #'
# # survey.strong_fit <- lavaan.survey(lavaan.fit = strong_fit, survey.design = design.no.hier)
# #'
# # #comparing nested models
# # anova(survey.config_fit, survey.weak_fit, SB.classic=TRUE) #weak invariance yes
# # anova(survey.weak_fit, survey.strong_fit, SB.classic=TRUE)  #strong invariance no, partial invariance?
# 
# #'# Predictive validity
# #'
# #'## Achievement measures
# #'Predictive validity of the 10 SAL factors with respect to achievement measures (Language, Math). Fit did not deteriorate due to inclusion of predictive factors of language and math achievement scores.
# 
# model.pv <- '
# INSMOT =~ B1_3 + B1_8 + B1_12
# EFFPER =~ B1_4 + B1_7 + B1_11 + B1_15
# SELFEF =~ B1_10 + B1_5 + B1_14
# CEXP =~ B1_2 + B1_6 + B1_9 + B1_13
# SCVERB =~ B2_1 + B2_4 + B2_7 + B2_10
# SCMATH =~ B2_2 + B2_5 + B2_9 + B2_11
# SCACAD =~ B2_3 + B2_6 + B2_8
# Math_LV =~ IRT_M
# Lang_LV =~ IRT_CJ
# Math_LV ~~ INSMOT + EFFPER + SELFEF + CEXP + SCVERB + SCMATH + SCACAD
# Lang_LV ~~ INSMOT + EFFPER + SELFEF + CEXP + SCVERB + SCMATH + SCACAD
# Lang_LV ~~ Math_LV
# '
# 
# #+ message = FALSE
# fit.pv <- cfa(model = model.pv, data = dat, meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#            estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot, sampling.weights = "STUWGT",
#            ordered = names(dat)[1:26])
# fitted.model.pv <- lavaan.survey(lavaan.fit = fit.pv, survey.design = design, estimator = "MLMVS")
# 
# #'### Model test and approximate fit indices
# fitmeasures(fitted.model.pv, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# 
# #'### Latent correlations
# # Correlations between the SAL factors and achievement measures (Language, Math)
# #'
# # The achievement scores correlate rather strongly and show positive relationship towards each of the SAL factors. Apart from having highest intercorrelations with other SAL factors, SELFEF shows the highest relative predictive power.
# #'
# lv.cor.pv <- round(inspect(fitted.model.pv, "cor.lv"), 2)
# lv.cor.pv[upper.tri(lv.cor.pv)] <- ""
# lv.cor <- as.data.frame(lv.cor.pv)
# 
# kable(lv.cor.pv, "html", digits = 2) %>%
#   kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left")
# 
# #'## Bayesian analysis of predictive power
# #'
# # **For achievement measures**
# #'
# # **Bayes factor in favor of the alternative hypothesis (BF10) and posterior probability for model parameters (given 1:1 prior odds for H0:Ha)**
# #'
# #'Bayes factors show whether there is evidence either for Ha (effect present) or H0 (effect absent), i.e., whether the data are more consistent with Ha, H0, or inconclusive. Posterior probability refers to the probability of the *parameter* not being zero (as oposed to probability of the data under a null).
# #'Frequentist approach without specific procedures (like equivalence testing), on the other hand, cannot provide evidence for H0, by definition (the only possible conclusions are H0 being rejected or failed to be rejected).
# #'These are Bayes Factors based on model selection / information criteria approach as proposed by Wagenmakers, 2007. Each BF represents the relative evidence in the data favoring alternative hypothesis (parameter freely estimated) over the null (the given parameter fixed to 0).
# #'Bayes Factors using BIC approximation implicitly assume unit information prior which makes them rather conservative with regard to the alternative hypothesis.
# #'
# #'Parameters a-u refer to covariances between Math achievement (a-j), Language achievement (k-u) and the 10 SAL factors.
# #'
# # Math ~~ a*INSMOT
# # Math ~~ b*EFFPER
# # Math ~~ c*SELFEF
# # Math ~~ d*CEXP
# # Math ~~ e*INTREA
# # Math ~~ f*INTMAT
# # Math ~~ g*COMLRN
# # Math ~~ h*SCVERB
# # Math ~~ i*SCMATH
# # Math ~~ j*SCACAD
# # Language ~~ k*INSMOT
# # Language ~~ l*EFFPER
# # Language ~~ m*SELFEF
# # Language ~~ n*CEXP
# # Language ~~ o*INTREA
# # Language ~~ p*INTMAT
# # Language ~~ q*COMLRN
# # Language ~~ r*SCVERB
# # Language ~~ s*SCMATH
# # Language ~~ t*SCACAD
# # Math ~~ u*Language
# #'
# f1 <- NULL
# bf1 <- NULL
# posterior1 <- NULL
# for(i in letters[1:21]){
#   m1 <- '
#   INSMOT =~ C6_B1C + C6_B1H + C6_B1L
#   EFFPER =~ C6_B1D + C6_B1G + C6_B1K + C6_B1O
#   SELFEF =~ C6_B1A + C6_B1E + C6_B1J + C6_B1N
#   CEXP =~ C6_B1B + C6_B1F + C6_B1I + C6_B1M
#   INTREA =~ C6_B2E + C6_B2K + C6_B2N
#   INTMAT =~ C6_B2A + C6_B2H + C6_B2Q
#   COMLRN =~ C6_B2C + C6_B2I + C6_B2M + C6_B2S
#   SCVERB =~ C6_B2G + C6_B2D + C6_B2R
#   SCMATH =~ C6_B2J + C6_B2L + C6_B2O
#   SCACAD =~ C6_B2B + C6_B2F + C6_B2P
#   C6_B2L ~~ C6_B2H
#   C6_Math_rel_fac =~ C6_Math_rel
#   C6_lang_rel_fac =~ C6_lang_rel
#   C6_Math_rel_fac ~~ a*INSMOT
#   C6_Math_rel_fac ~~ b*EFFPER
#   C6_Math_rel_fac ~~ c*SELFEF
#   C6_Math_rel_fac ~~ d*CEXP
#   C6_Math_rel_fac ~~ e*INTREA
#   C6_Math_rel_fac ~~ f*INTMAT
#   C6_Math_rel_fac ~~ g*COMLRN
#   C6_Math_rel_fac ~~ h*SCVERB
#   C6_Math_rel_fac ~~ i*SCMATH
#   C6_Math_rel_fac ~~ j*SCACAD
#   C6_lang_rel_fac ~~ k*INSMOT
#   C6_lang_rel_fac ~~ l*EFFPER
#   C6_lang_rel_fac ~~ m*SELFEF
#   C6_lang_rel_fac ~~ n*CEXP
#   C6_lang_rel_fac ~~ o*INTREA
#   C6_lang_rel_fac ~~ p*INTMAT
#   C6_lang_rel_fac ~~ q*COMLRN
#   C6_lang_rel_fac ~~ r*SCVERB
#   C6_lang_rel_fac ~~ s*SCMATH
#   C6_lang_rel_fac ~~ t*SCACAD
#   C6_Math_rel_fac ~~ u*C6_lang_rel_fac
#   '
#   fit.loop <- cfa(model = m1, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#       estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot)
#   f1 <- lavaan.survey(lavaan.fit = fit.loop, survey.design = design.no.hier, estimator = "MLM")
#   f2 <- lavaan.survey(cfa(model = m1, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#                           estimator = "MLR", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#                           constraints = paste(i, '==', '0', sep = "")), survey.design = design.no.hier, estimator = "MLM")
#   bf1[i] <- 1/exp((BIC(f1) - BIC(f2))/2)
#   posterior1[i] <- bf1[i]/(1+bf1[i])
# }
# bayes.out <- data.frame(BF10 = round(bf1, 3), Posterior = round(posterior1, 3))
# kable(bayes.out, "html", digits = 2) %>%
#   kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left")
# 
# # BF10 indicates how much likely is the data under Ha as compared to H0. Table above shows that there is almost 100% posterior probability in favor of most of these effects.
# #'
# 
# #'## Grades
# #'Predictive validity of the 10 SAL factors with respect to grades - Language (averaged Czech and English language grades) and Math. Fit did not deteriorate due to inclusion of predictive factors of language and math achievement scores.
# model.pv.grades <- '
# INSMOT =~ C6_B1C + C6_B1H + C6_B1L
# EFFPER =~ C6_B1D + C6_B1G + C6_B1K + C6_B1O
# SELFEF =~ C6_B1A + C6_B1E + C6_B1J + C6_B1N
# CEXP =~ C6_B1B + C6_B1F + C6_B1I + C6_B1M
# INTREA =~ C6_B2E + C6_B2K + C6_B2N
# INTMAT =~ C6_B2A + C6_B2H + C6_B2Q
# COMLRN =~ C6_B2C + C6_B2I + C6_B2M + C6_B2S
# SCVERB =~ C6_B2G + C6_B2D + C6_B2R
# SCMATH =~ C6_B2J + C6_B2L + C6_B2O
# SCACAD =~ C6_B2B + C6_B2F + C6_B2P
# C6_B2L ~~ C6_B2H
# grade_math_fac =~ grade.math
# grade_lang_fac =~ grade.lang
# grade_math_fac ~~ INSMOT
# grade_math_fac ~~ EFFPER
# grade_math_fac ~~ SELFEF
# grade_math_fac ~~ CEXP
# grade_math_fac ~~ INTREA
# grade_math_fac ~~ INTMAT
# grade_math_fac ~~ COMLRN
# grade_math_fac ~~ SCVERB
# grade_math_fac ~~ SCMATH
# grade_math_fac ~~ SCACAD
# grade_lang_fac ~~ INSMOT
# grade_lang_fac ~~ EFFPER
# grade_lang_fac ~~ SELFEF
# grade_lang_fac ~~ CEXP
# grade_lang_fac ~~ INTREA
# grade_lang_fac ~~ INTMAT
# grade_lang_fac ~~ COMLRN
# grade_lang_fac ~~ SCVERB
# grade_lang_fac ~~ SCMATH
# grade_lang_fac ~~ SCACAD
# grade_math_fac ~~ grade_lang_fac
# '
# 
# #+ message = FALSE
# fit.pv.grades <- cfa(model = model.pv.grades, data = data[!is.na(data$cluster),], meanstructure = FALSE, std.lv = FALSE, mimic = "Mplus",
#               estimator = "WLSMVS", test = "Satterthwaite", orthogonal = FALSE, bootstrap = nboot,
#               ordered = c("C6_B1A", "C6_B1B", "C6_B1C", "C6_B1D", "C6_B1E",
#                           "C6_B1F", "C6_B1G", "C6_B1H", "C6_B1I", "C6_B1J",
#                           "C6_B1K", "C6_B1L", "C6_B1M", "C6_B1N", "C6_B1O",
#                           "C6_B2A", "C6_B2B", "C6_B2C", "C6_B2D", "C6_B2E",
#                           "C6_B2F", "C6_B2G", "C6_B2H", "C6_B2I", "C6_B2J",
#                           "C6_B2K", "C6_B2L", "C6_B2M", "C6_B2N", "C6_B2O",
#                           "C6_B2P", "C6_B2Q", "C6_B2R", "C6_B2S"))
# fitted.model.pv.grades <- lavaan.survey(lavaan.fit = fit.pv.grades, survey.design = design, estimator = "MLMVS")
# 
# #'### Model test and approximate fit indices
# fitmeasures(fitted.model.pv.grades, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "pnfi", "bic"))
# 
# #'### Latent correlations
# # Correlations between the SAL factors and achievement measures (Language, Math)
# #'
# # Grades correlate rather strongly (stronger than achievement measures) and show positive relationship towards each of the SAL factors. SCACAD and SELFEF show the highest relative predictive power.
# #'
# lv.cor.pv.grades <- round(inspect(fitted.model.pv.grades, "cor.lv"), 2)
# lv.cor.pv.grades[upper.tri(lv.cor.pv.grades)] <- ""
# lv.cor.pv.grades <- as.data.frame(lv.cor.pv.grades)
# kable(lv.cor.pv.grades, "html", digits = 2) %>%
#   kable_styling(bootstrap_options = "condensed", full_width = F, font_size = 12, position = "left")
# 
# #'### Intra-class correlations for model variables
# vars <- names(data %>% select(C6_B1A:C6_B2S))
# ICCs <- NA
# for(i in 1:length(vars)){
#   var <- as.numeric(unlist(c(data %>% select(vars = i))))
#   ICCs[i] <- ICCbare(cluster, var, data)
# }
# 
# (iccTable <- data.frame("Variable name" = vars, ICC = as.numeric(round(ICCs, 4))))
# 
# #'#### ICC range for the observed variables
# range(iccTable$ICC)
# 
# #'#### median ICC for the observed variables
# median(iccTable$ICC)
# 
# #'#### Mean value of the design effect (Muthén & Sattora, 1995)
# 1 + (nrow(data)/length(unique(data$cluster)) - 1)*((sum(iccTable$ICC)/nrow(iccTable))/5)

