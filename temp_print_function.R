print_dich_MCMC<-function (x, ...){
  s_fit <- x
  alpha = ToxicR:::.evaluate_alpha(s_fit)
  param_est=paste(round(s_fit$parameter,3), collapse=",   ")
  
  s_fit$eff_size <- coda::effectiveSize(s_fit$mcmc_result$BMD_samples)
  s_fit$geweke_z   <- coda::geweke.diag(coda::as.mcmc(s_fit$mcmc_result$BMD_samples),frac1 = 0.3, frac2 = 0.4)$z
  params<-paste(s_fit$prior$prior$parameters, collapse=",   ")
  cat("Summary of single model fit(MCMC) using ToxicR\n")
  cat(sprintf("Model: %s",s_fit$model),"\n")
  
  cat("\nBMD: ")
  cat(sprintf("%1.2f (%1.2f, %1.2f) %1.1f%% CI\n",
              s_fit$bmd[2],
              s_fit$bmd[1],
              s_fit$bmd[3],
              100*(1-2*alpha)))
  
  
  cat("\nParameters:\n")
  cat(sprintf("  %s \n\n", params))
  
  cat("Parameter estimates:\n")
  cat(sprintf("  %s \n\n", param_est))
  
  
  cat("Covariance Matrix:\n")
  
  for (i in 1:nrow(s_fit$covariance)){
    temp<-paste(round(res$covariance[i,],3), collapse="  ")
    cat(sprintf(" %s \n",temp))
    
  }
  
  cat("Convergence Diagnostics on BMD\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("Effective Sample Size: %1.2f\n\n", s_fit$eff_size) )
  cat(sprintf("Geweke Z-score that mean of first 30%% of \nMCMC chain is different from last 40%%\nZ-Score: %1.3f  P-value %1.3f\n",
              s_fit$geweke_z,2*pnorm(abs(s_fit$geweke_z),lower.tail=F)))
  
}

paste(res$prior$prior$parameters, collapse=",   ")
print_dich_MCMC(res)

res$parameter
res$parameters
summary(res)

res$gof_p_value


res$covariance

res$prior$prior$parameters
paste(res$prior$prior$parameters, collapse=",   ")
res  

alpha3


ToxicR:::.evaluate_alpha(res)
