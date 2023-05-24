#### stepCPUE<-function(object, scope, r2.change=0.01, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = deviance, steps = 1000, ...)
###
extractCPUE<-function(fit, scale = 0, ...)
{
n <- length(fit$residuals)
edf <- n - fit$df.residual
r.squared<- (fit$null.deviance - fit$deviance) / fit$null.deviance
c(edf, r.squared)
}
###
stepCPUE<-function(object, scope, r2.change=0.01, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = deviance, steps = 1000, ...)

{
cut.string <- function(string)
{
  if(length(string) > 1)
    string[-1] <- paste("\n", string[-1], sep = "")
  string
 }
AIC.drop1 <- function(fit, Terms, scale, trace, ...)
  {
  n <- length(Terms)
  ans <- matrix(nrow = n + 1, ncol = 2)
  dimnames(ans) <- list(c("<none>", paste("-", Terms, sep = "")), c("df", "AIC"))
  ans[1,  ] <- extractCPUE(fit, scale, ...)
  if(n == 0)
    return(ans)
  i <- 2
  for(tt in Terms) {
    if(trace > 1)
      cat("trying -", tt, "\n")
    else cat(".")
    nfit <- update(fit, paste("~ . -", tt))
    ans[i,  ] <- extractCPUE(nfit, scale, ...)
    if (trace > 2) print(ans[i, ])
    i <- i + 1
  }
  ans
}
AIC.add1 <- function(fit, Terms, scale, trace, screen, ...)
{
  n <- length(Terms)
  ans <- matrix(nrow = n + 1, ncol = 2)
  t2 <- if(length(Terms)) paste("+", Terms, sep = "") else NULL
  dimnames(ans) <- list(c("<none>", t2), c("df", "AIC"))
  ans[1,  ] <- extractCPUE(fit, scale, ...)
  if(n == 0)
    return(ans)
  i <- 2
  for(tt in Terms) {
    if(trace > 1)
      cat("trying +", tt, "\n")
    else cat(".")
    nfit <- update(fit, paste("~ . +", tt))
    ans[i,  ] <- extractCPUE(nfit, scale, ...)
    if (trace > 2) print(ans[i, ])
    i <- i + 1
  }
  ans
}
re.arrange <- function(keep)
{
  namr <- names(k1 <- keep[[1]])
  namc <- names(keep)
  nc <- length(keep)
  nr <- length(k1)
  array(unlist(keep, recursive = F), c(nr, nc), list(namr, namc))
}
make.step <- function(models, fit, object)
{
  change <- sapply(models, "[[", "change")
  rd <- sapply(models, "[[", "deviance")
  dd <- c(NA, diff(rd))
  rdf <- sapply(models, "[[", "df.resid")
  ddf <- c(NA, diff(rdf))
  AIC <- sapply(models, "[[", "AIC")
  heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", "\nInitial Model:", deparse(as.vector(formula(object))), 
    "\nFinal Model:", deparse(as.vector(formula(fit))), "\n")
  aod <- data.frame(Step = change, Df = ddf, Deviance = dd, "Resid. Df" = rdf, "Resid. Dev" = rd, "r.squared" = AIC, check.names = F)
  attr(aod, "heading") <- heading
  attr(aod, "class") <- c("anova", "data.frame")
  fit$anova <- aod
  fit
}
if(missing(direction))
  direction <- "both"
else direction <- match.arg(direction)
backward <- direction == "both" | direction == "backward"
forward <- direction == "both" | direction == "forward"
if(missing(scope)) {
  fdrop <- numeric(0)
  fadd <- NULL
}
else {
  if(is.list(scope)) {
    fdrop <- if(!is.null(fdrop <- scope$lower)) attr(terms(update.formula(object, fdrop)), "factor") else numeric(0)
    fadd <- if(!is.null(fadd <- scope$upper)) attr(terms(update.formula(object, fadd)), "factor")
  }
  else {
    fadd <- if(!is.null(fadd <- scope)) attr(terms(update.formula(object, scope)), "factor")
    fdrop <- numeric(0)
  }
}
if(is.null(fadd)) {
  backward <- T
  forward <- F
}
models <- vector("list", steps)
if(!is.null(keep)) {
  keep.list <- vector("list", steps)
  nv <- 1
}
n <- length(object$residuals)
fit <- object
cf <- attributes(coef(object))  #check if any terms have zero df
if(!is.null(cf$singular) && cf$singular > 0) {
  TT <- !match(TL <- attr(object$terms, "term.labels"), names(cf$assign), F)
  if(any(TT)) {
    upd <- eval(parse(text = paste(c(".~.", TL[TT]), collapse = "-")))
    fit <- update(fit, upd)
  }
}
bAIC <- extractCPUE(fit, scale, ...)
edf <- bAIC[1]
bAIC <- bAIC[2]
nm <- 1
Terms <- fit$terms
cat("\n\nTesting for a change in r.squared of less than ", format(round(r2.change,4)),"\n")
models[[nm]] <- list(deviance = bAIC - 2 * edf, df.resid = n - edf, change = "", AIC = bAIC)
if(!is.null(keep))
  keep.list[[nm]] <- keep(fit, bAIC)
AIC <- 0
count.steps<-0
while(steps > 0) {
  steps <- steps - 1
  AIC <- bAIC
  bfit <- fit
  ffac <- attr(Terms, "factor")
  scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
  aod.drop <- NULL
  aod.add <- NULL
  aod<-NULL
  change <- NULL
  if(backward && (ndrop <- length(scope$drop))) {
    aod.drop <- AIC.drop1(fit, scope$drop, scale = scale, trace = trace, ...)  
  }
  if(forward && (nadd <- length(scope$add))) {
    aod.add <- AIC.add1(fit, scope$add, scale = scale, trace = trace, screen = screen, ...)
  }
  if(is.null(aod.drop) && is.null(aod.add)) break
  if(!is.null(aod.drop) && nrow(aod.drop) > 1) {
    o <- rev(order(aod.drop[,"AIC"]))
    if((aod.drop[1,"AIC"]-aod.drop[o[2],"AIC"]) < r2.change) {
      change <- dimnames(aod.drop)[[1]][o[2]]
      cat(paste("\nDrop term",change,"\n"))
      if (is.null(aod.add)) {
        aod<-aod.drop
      } else {
        aod<-rbind(aod.drop,aod.add[-1,])
      }
    }
  }
  if (is.null(aod)) {
    o <- order(-aod.add[, "AIC"])
    if((aod.add[o[1],"AIC"]-aod.add[1,"AIC"]) >= r2.change) {
      change <- dimnames(aod.add)[[1]][o[1]]
      cat(paste("\nAdd term",change,"\n"))
      if (is.null(aod.drop)) {
        aod<-aod.add
      } else {
        aod<-rbind(aod.drop,aod.add[-1,])
      }
    }
  }
  if (is.null(aod)) {
    if(is.null(aod.drop))
      aod<-aod.add
    else if(is.null(aod.add))
      aod<-aod.drop
    else aod<-rbind(aod.add,aod.drop[-1,])
    cat("\n")
    print(data.frame("df"=aod[,1],"r.sqaured"=aod[,2]))
    cat("\n")
    break
    }
  if(trace > 1) {
    print(data.frame("df"=aod[,1],"r.sqaured"=aod[,2]))
  }
  fit <- update(fit, eval(parse(text = paste("~ .", change))))
  Terms <- fit$terms
  bAIC <- extractCPUE(fit, scale, ...)
  edf <- bAIC[1]
  bAIC <- bAIC[2]
  nm <- nm + 1
  edf <- models[[nm]] <- list(deviance = bAIC - 2 * edf, df.resid = n - edf, change = change, AIC = bAIC)
  if(!is.null(keep))
    keep.list[[nm]] <- keep(fit, bAIC)
}
if(!is.null(keep))
  fit$keep <- re.arrange(keep.list[seq(nm)])
fit<-make.step(models = models[seq(nm)], fit, object)
print(fit$anova)
fit
}

# Reset reference year to be first year in regresssion
contr.baseyear1<-function(n, contrasts = T, base = 1)
{
if(is.numeric(n) && length(n) == 1) {
  levs <- 1:n 
} else {
  levs <- n
  n <- length(n)
  }
contr <- array(0, c(n, n), list(levs, levs))
contr[seq(1, n^2, n + 1)] <- 1
if(contrasts) {
  if(n < 2)
  stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
  contr <- contr[,  - base, drop = F]
  }
return(contr)
}

# Reset reference year to be second year in regresssion
contr.baseyear2<-function(n, contrasts = T, base = 2)
{
if(is.numeric(n) && length(n) == 1) {
  levs <- 1:n 
} else {
  levs <- n
  n <- length(n)
  }
contr <- array(0, c(n, n), list(levs, levs))
contr[seq(1, n^2, n + 1)] <- 1
if(contrasts) {
  if(n < 2)
  stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
  contr <- contr[,  - base, drop = F]
  }
return(contr)
}
#
