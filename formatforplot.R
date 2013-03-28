formatforplot <- function(obj, parm.names=names(fixef(obj)), model.name=deparse(substitute(obj)), rnd = 2) {
  require(glmmADMB, quietly=TRUE)
  
  rndtostr <- function(val, dig=rnd) formatC(round(val, rnd), format="f", digits=rnd)
  
   coef <- unname(fixef(obj))
     se <- unname(sqrt(diag(vcov(obj))))
  lower <- coef - 1.959964*se
  upper <- coef + 1.959964*se
  zstat <- abs(coef/se)
   pval <- 2 * pnorm(zstat, lower.tail = FALSE)
  
  ## format p-value to manuscript style w/ categories for <.01 and <.001
  pvalf <- ifelse(pval>=.005, sub("^[0]\\.*", ".", rndtostr(pval)),
                  ifelse(pval<0.005 & pval>=0.0015, "<.01",
                         ifelse(pval<0.0015 & pval>=0.0005, ".001", "<.001")))
  
  ## output formatted results in both numeric and string formats
  out <- data.frame(  model = model.name,
                      param = parm.names,
                       beta = round(coef, rnd),
                         se = round(se, rnd),
                      lower = round(lower, rnd),
                      upper = round(upper, rnd),
                       pval = round(pval, rnd),
                     beta.s = rndtostr(coef, rnd),
                       se.s = rndtostr(se, rnd),
                    lower.s = rndtostr(lower, rnd),
                    upper.s = rndtostr(upper, rnd),
                     pval.s = pvalf,
                    stringsAsFactors=FALSE)
  
  return(out)
}
