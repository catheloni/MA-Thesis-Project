# MA-Thesis-Project
This repo represents the ongoing work and progress of my thesis project on climate concern in Austria.
The thesis was submitted in June 2026 as the final prerequisites for completion of the master's degree "Economics Data and Policy" at CEU.

### Project Outline
The project analyzes the impact of the flooding event in September 2024 on climate change worries in Austria. I utilize data from pilot wave 1 (2023) and 2 (2024) from the Austrian Socio-Economic Panel (ASEP), which is not uploaded in this repo due to privacy concerns but can be downloaded free of charge from [AUSSDA](https://aussda.at).



### Folder Structure
```bash
Thesis-Project
├── article_scraping
│   ├── scripts
│   │   ├── 00_scraping_articles.ipynb
│   │   ├── 01_cleaning_articles.ipynb
│   │   ├── 02_llm_categorization.ipynb
│   │   └── 03_town_classification.ipynb
│   └── output
│       ├── 00_articles_df.csv
│       ├── 02_austria_flood_impact_summary.csv
│       ├── 03_town_sizes.csv
│       ├── 03_flood_count_weighted.csv
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
├── notes
│   ├── notes.odt
│   └── IE_W25_S8_DID.pdf
├── presentation-1
│   ├── did_data_prep.R
│   ├── missing_checks.R
│   ├── parallel_trends.R
│   ├── year_comp_check.R
│   ├── presentation-notes.odt
│   └── presi
│       ├── lib.bib
│       ├── presentation_2.pdf
│       └── figures
│          ├── did-image.ipynb
│          ├── parallel-trend.png
│          ├── ...
│          └── town-count-weighted.png
├── draft
│   ├── main.pdf
│   └── figures
├── LICENSE
└── README.md
```




The folder "article_scraping" (which takes up an disproportionate amount of space...) contains all the scripts and output that were created in the process of classifying the individual survey respondents as treatment or control.
