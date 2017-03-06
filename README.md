# strategy-estimation
This is the code and papers discussed in <a href="https://docs.google.com/presentation/d/1GLaxG6KUtdUi_56IFJfmO3W3cUM07x2dODMa8mg4Nvo/edit?usp=sharing">this</a> presentation at the MIT Media Lab. 

It contains three folders:

<h2>Papers</h2>
This folder contains the papers talked about in the talk. These papers are 
<ol>
<li>Epstein, Ziv, Alexander Peysakhovich, and David G. Rand. "The Good, the Bad, and the Unflinchingly Selfish: Cooperative Decision-Making can be Predicted with high Accuracy when using only Three Behavioral Types." Proceedings of the 2016 ACM Conference on Economics and Computation. ACM, 2016.
</li>
<li>Poncela-Casasnovas, Julia, et al. "Humans display a reduced set of consistent behavioral phenotypes in dyadic games." Science Advances 2.8 (2016): e1600451.
</li>
<li>Fudenberg, Drew, David G. Rand, and Anna Dreber. "Slow to anger and fast to forgive: Cooperation in an uncertain world." The American Economic Review 102.2 (2012): 720-749.</li>
<li>Bó, Pedro Dal, and Guillaume R. Fréchette. "The evolution of cooperation in infinitely repeated games: Experimental evidence." The American Economic Review 101.1 (2011): 411-429.</li>
<li>
Hartford, Jason S., James R. Wright, and Kevin Leyton-Brown. "Deep learning for predicting human strategic behavior." Advances in Neural Information Processing Systems. 2016.
</li>
</ol>

<h2>Sfem demo</h2>
This folder contains matlab scripts that runs the maximum likelihood estimaton from the Slow to Anger and Fast to Forgive paper. 
`master_MLE_script.m` runs the entire analysis for the s1 data file with 12 participants and 3 strategies. `boots_launcher.m` runs the boostrapped
likelihood calculation and prints the estimated probability of each strategy, as well as the estimation for gamma. 

<h2>gbu regression</h2>
This folder contains the R scripts used in the Good Back Ugly EC paper. `functions.R` contains the functions used to generate the models, and `ec_main_analysis.R` contains the calls to these functions which generates the point estimates and plots found in the paper. 
