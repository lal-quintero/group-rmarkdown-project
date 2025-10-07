## Discussion points
##### For slides and paper

From overview: 
- whisky regionality is important for sales
- whisky is a major export, with a trade value in the [billions](https://wits.worldbank.org/trade/comtrade/en/country/ALL/year/2021/tradeflow/Exports/partner/WLD/product/220830)
- there are many influencing factors in the chemical makeup of whiskies, including the complex distilling process, differing base materials, and aging time
- counterfeiting is a common problem, and poses both economic and human health risk
  - this can be further abstracted to other forms of liquor, as counterfeiting is not a problem unique to whiskies
 
From aims:
- reuse of collected data from the shand research paper
  - trace chemical samples collected from 7 whiskey types
- assessing reproducability of the study's results, and looking at possible further applications of multivariate analytical methods to this method of chemical sampling
  - if successful & widely applicable, this would presents a novel and cost affordable way to differentiate counterfeits, grains and malt whiskies
 
From hypothesis:
- We wanted to use all available data to retain maximum information, so we employed a range of common cluster analysis methods, to determine whether any could produce useful and agreeing groups fro whisky differentiation
- The study was able to successfully differentiate counterfeits from all other whiskies in a limited PC space (using PCs 1-3, encapsulating ~80% of the variation in the data)
  - We attempted to replicate this using k-means, PAM, and agglomerative hierachical clustering
- We wanted to extend the results of the study, o see whether the data presents any compelling groups other than our pre-applied ones
  - To do this, we assessed data structure, and investigated whether groups emerging from cluter analysis agree

Method: \
LDA alone may be used to detect counterfeits, if lower fidelity is desired. \
(results pre cluster analysis identify some traits which may pick out whether a scotch could be counterfeit, or at the very least non-provenance) 

Based on the LDA/clustering analysis, TXRF as a novel means of counterfeit detection appears to be a sound methodology with a reasonable degree of success and accuracy, when care is taken with distance selection, methodology and appropriate selection of number of clusters - varying results and levels of accuracy \
This method could be applied to other types of liquor/alcohol, but would need to undergo the same exploratory analysis 

Points of improvement for further studies: \
Unsure if what we’re picking up on in some \
Grains are the weakest group in terms of identification across the board: grains/blends may be more likely to be misclassified as counterfeit, rather than misclassified as provenance. This could mean some explanatory variables, such as the age of the whisky (as provenance whiskies are long aged, while grain and blends are typically younger in age) \
Regionality may also be something indicated by TXRF results \
Further and more varying data required to look into these ideas. 

TODO:
- [ ] Incorporate overview, aims, and hypothesis response as a structure to the discussion, and answer/respond to each using relevant data points and observations
- [ ] Brief commentary on results and how they relate back to the above
- [ ] Take my handwritten notes from data exploration and observation and incorporate them into the discussion
- [ ] At least one statistic on counterfeit whisky/liquors, in support of the further uses of this technique on counterfeit detection



## Paper sections (for reference and integration)

#### Overview and importance
ie., whisky regionality is important for sales; whisky is one of top exports; many factors influence whiskies chemical makeup from complex distilling process, differing  base materials and aging time; counterfeiting is common and poses both economic and human health risk.

#### Aims
Reassesing/Using data already collected is both cost-saving and effective within acadamia when resources are limited. We used  @shand2017multivariate XTRF collected trace chem sampels from 7 whisky types. We did so to assess reproducability as well further applications of multivariaite analytical methods to this method of chemical sampling. If succesful and widely applicable, this presents a novel and cost affordable way to differentiate counterfeits, grains and malt whiskies.

#### Hypothesis
As we wish to use all available data to retain maximum information, we wish to employ multiple common cluster analysis methods to see if can produce useful and agreeing groups for whisky differentiation. As @shand2017multivariate was able to differentiate counterfeits successfully from all other whiskies within a limited principal component (PC) space (PC1-PC3), we wished to attempt to replicate this using k-means, partitioning about mediods (PAM), and agglomerative hierarchical clustering. 

Further, we wish to see if the data presents us with any compelling groups other than our pre-applied ones, we will aim at assessing data structure and seeing if groups emerging from cluster analysis agree.


## Placeholder discussion points
`Best method, LDA could be used as well to detect counterfeits if lower fidelity is desired. XTRF appears to be a sound method for discriminating malt, grain counterfeits across cluster type though care should be taken with hierarchical clustering in choosing an applicable distance, as results varied widely both in counterfeit discrimination as well as grain/blend and counterfeit discrimination.`

`More data/different data may be needed to truly discern providence. XTRF has been used to sample metals, compounds imparted by wood, etc; aging process may be more distinguishable than region as the main thing separating classes are chemicals ________, likely derived from whether the whisky is grain, malt (of providence) or a counterfeit (likely additives). Interestingly 2 island whiskies (______ and _______) were very distinct from all other samples, as seen in PC space and euclidian clustering`
