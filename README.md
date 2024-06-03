# Not (Officially) in My Backyard: Characterizing Informal Accessory Dwelling Units and Informal Housing Policy with Remote Sensing

## Nathanael Jo, Andrea Vallebueno, Derek Ouyang, Daniel E. Ho

This is the code repository for ["Not (Officially) in My Backyard: Characterizing Informal Accessory Dwelling Units and Informal Housing Policy with Remote Sensing."](https://www.tandfonline.com/doi/full/10.1080/01944363.2024.2345730)

One promising policy approach to addressing housing needs is liberalizing accessory dwelling unit (ADU) development. Yet understanding the impact of such policy efforts is fundamentally constrained by the inability to quantify and characterize unpermitted ADUs, which may expose homeowners and tenants to legal, financial, and safety risks and confound policy evaluations. We address this gap by leveraging computer vision and human annotations to estimate the population of detached ADU constructions in San José, California. We find that informal ADU construction is substantial – approximately three to four informal units for every formal unit – and more likely in more diverse, dense, and overcrowded neighborhoods.

![Methodology Overview](output/paper_figures/flowchart_simplified.png)

```         
@article{doi:10.1080/01944363.2024.2345730,
author = {Nathanael Jo, Andrea Vallebueno, Derek Ouyang and Daniel E. Ho},
title = {Not (Officially) in My Backyard},
journal = {Journal of the American Planning Association},
volume = {0},
number = {0},
pages = {1--16},
year = {2024},
publisher = {Routledge},
doi = {10.1080/01944363.2024.2345730},
URL = {https://doi.org/10.1080/01944363.2024.2345730},
eprint = {https://doi.org/10.1080/01944363.2024.2345730}
}
```

## Repository
This code repository is structured as follows. 

* `src/Sampling`: Scripts used to define the set of residential parcels in the City of San José, stratify by census block
group (CBG) household income and model confidence scores, sample the simple random sample of 5,000 parcels and the
stratified random sample of 15,006 parcels, compute the Neyman allocation, and perform the power analyses described
in Appendices D.3 and D.4.
* `src/Permits`: Scripts used to extract the permits from San José's
[Property & Information Portal](https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch) and to generate
the set of permits used to define the formal population of ADU constructions during 2016-2020.
* `src/Results`: Scripts used to compute the findings presented in the Results and Discussion sections of the main text, 
including the population estimates and permit-matching estimates of the unpermitted proportion of ADU constructions, 
the difference in means analysis across neighborhood- and parcel-level characteristics, and analyses of the City's
complaint data.
* `src/Appendix`: Scripts used to generate the findings presented in the Appendices, namely the analysis of the relationship
between household income and ADU construction (Appendix J) and the analysis of the complaint rate (Appendix I).

## Dataset release
We provide a small building segmentation dataset using small buildings from our study as a resource for future research, excluding 
informal detections. This labeled dataset includes parcel-level remote sensing imagery and accompanying polygons of
detached buildings located within each parcel. Our dataset contains 38,875 images in total, including 2,225 positive
images (images containing one or more permitted small buildings) and 36,650 negative images (images that do not contain a small building). 

## References
City of San José Development Services Permit Center. (2023). Permit and Property Information Portal [Data set]. 
City of San José Development Services Permit Center. Retrieved 2023-06-09, from https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch
