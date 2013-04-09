## Utility function that extracts estimates for fitted regression models and outputs
## numeric and formatted string versions suitable for plotting

formatforplot <- function(obj, parm.names=names(coef(obj)), model.name=deparse(substitute(obj)), eform=TRUE, rnd = 2) {
  ## load the appropriate library for reading the fitted regression objected
  obj.class <- class(obj)
  if (obj.class=="lme") require(lme4, quietly=TRUE)
  if (obj.class=="glmmadmb") require(glmmADMB, quietly=TRUE)
  if (obj.class=="hurdle" | obj.class=="zeroinfl") require(pscl, quietly=TRUE)
  
  rndtostr <- function(val, dig=rnd) formatC(round(val, rnd), format="f", digits=rnd)
  
  beta <- unname(coef(obj))
  se <- unname(sqrt(diag(vcov(obj))))
  lower <- beta - 1.959964*se
  upper <- beta + 1.959964*se
  zstat <- abs(beta/se)
  pval <- 2 * pnorm(zstat, lower.tail = FALSE)
  
  ## format p-value to manuscript style w/ categories for <.01 and <.001
  pvalf <- ifelse(pval>=.005, sub("^[0]\\.*", ".", rndtostr(pval)),
                  ifelse(pval<0.005 & pval>=0.0015, "<.01",
                         ifelse(pval<0.0015 & pval>=0.0005, ".001", "<.001")))
  
  
  ## output formatted results in both numeric and string formats
  out <- data.frame(model = model.name,
                    param = parm.names,
                    beta = beta,
                    se = se,
                    lower = lower,
                    upper = upper,
                    pval = pval,
                    beta.s = rndtostr(beta, rnd),
                    se.s = rndtostr(se, rnd),
                    lower.s = rndtostr(lower, rnd),
                    upper.s = rndtostr(upper, rnd),
                    pval.s = pvalf,
                    stringsAsFactors=FALSE)

  ## append exponentiated coeficients, if requested
  if (eform==TRUE) {
    beta.e <- exp(beta)
    lower.e <- exp(lower)
    upper.e <- exp(upper)
    ci.e <- paste("[", rndtostr(lower.e,rnd), ", ", rndtostr(upper.e,rnd), "]", sep="")
    
    out0 <- data.frame(beta.e = beta.e,
                       lower.e = lower.e,
                       upper.e = upper.e,
                       beta.s = rndtostr(beta, rnd),
                       se.s = rndtostr(se, rnd),
                       lower.s = rndtostr(lower, rnd),
                       upper.s = rndtostr(upper, rnd),
                       beta.e.s = rndtostr(beta.e, rnd),
                       ci.e.s = ci.e,
                       stringsAsFactors=FALSE)
    
    ## 
    out <- cbind(out, out0)
  }

  return(out)
}
