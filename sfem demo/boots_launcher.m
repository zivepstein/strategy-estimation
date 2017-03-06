% now that we've collected all the likelihoods, do the optimization
    nBoots=1;
    [MLE_estimates, SEs, pvals]=calc_MLE(pisAll,nBoots);
   