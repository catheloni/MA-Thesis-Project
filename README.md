# MA-Thesis-Project
This repo collects the finished work of my thesis project on climate concern in Austria.
The thesis was submitted in June 2026 as the final prerequisites for completion of the master's degree "Economics Data and Policy" at CEU.

### Project Outline
The project analyzes the impact of the flooding event in September 2024 on climate change worries in Austria. I utilize data from pilot wave 1 (2023) and 2 (2024) from the Austrian Socio-Economic Panel (ASEP), which is not uploaded in this repo due to privacy concerns but can be downloaded free of charge from [AUSSDA](https://aussda.at).

*Please note:* Unfortunately, the project is not reproducible from the code available on this public repo. This is due to two reasons: 
* I had access to additional data that is not publicly available: The municipality code of all ASEP respondents made it possible to identify whether they were likely affected by flooding in their town. Furthermore, data provided by the Lower Austria State Government Office for Fire Departments and Civil Defense kindly supported me with some of their internal flood disaster data.
* Furthermore, all the code in the repo folder "ASEP data preparations" was written on an R browser on an external device with limited options for data transfer. This unfortunately means that not all data was transferred correctly. 


### Folder Structure
```bash
MA-Thesis-Project
├── article_scraping
│   ├── scripts
│   │   ├── 00_scraping_articles.ipynb
│   │   ├── 01_cleaning_articles.ipynb
│   │   ├── 02_llm_categorization.ipynb
│   │   └── 03_town_classification.ipynb
│   │   └── 04_noe_damage_wrangling.ipynb
│   └── output
│       ├── 01_articles_df.csv
│       ├── 02_austria_flood_impact_summary.csv
│       ├── 03_town_sizes.csv
│       ├── 04_flood_count_weighted.csv
│       ├── scraped-articles
│       │   ├── bezirk_flood_articles.csv
│       │   ├── heute_flood_articles.csv
│       │   ├── heute_flood_results.txt
│       │   ├── meinbezirk_urls.txt
│       │   └── orf_flood_articles.csv
│       ├── archive
│       │   ├── districts.txt
│       │   ├── meinbezirk_links.txt
│       │   ├── austria_flood_impact_summary.csv
│       │   └──  towns.txt
│       └── figures
│           └── district_count.png
├── ASEP data preparations
│   ├── data_prep
│   │   ├── 00_did_data_prep.R
│   │   ├── 01_descriptives.R
│   │   ├── 02_attrition_distribution.R
│   │   └── 03_xgboost.R
│   ├── models
│   │   └── 01_lower_at_did.R
│   ├── plots
│   │   ├── 00_ols_modelsummary.png
│   │   └── 01_treatment_effect_model1.png
│   ├── robustness checks
│   │   ├── 01_top_category.R
│   │   ├── 02_binary_outcome.R
│   │   └── 03_envir_model.R
├── Final-Thesis-Draft.pdf
├── LICENSE
└── README.md
```




The folder "article_scraping" (which takes up an disproportionate amount of space...) contains all the scripts and output that were created in the process of classifying the individual survey respondents as treatment or control.
