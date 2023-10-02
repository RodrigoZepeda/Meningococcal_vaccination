# Meningococcal disease vaccination model

## Data

Data is downloaded from the [CDC's Wonder website](https://wonder.cdc.gov/nndss/nndss_weekly_tables_menu.asp). They follow the case definitions specified by the [National Notifiable Disease Surveillance System](https://ndc.services.cdc.gov/conditions/meningococcal-disease/). Weekly data is available from 1996 to 2023. 

### Download process

Data is downloaded from the weekly reports using the `download_wonder.R` script. The script `process_wonder.R` generates the different data files. 

```mermaid
graph TD;

    download_wonder.R -->|Downloads data| process_wonder.R;
    
    process_wonder.R -->|Generates weekly totals| meningococcal_total.rds;
    process_wonder.R -->|Weekly totals since 2018| meningococcal_variants.rds;
    process_wonder.R -->|Yearly totals by age\n2016 to 2020| meningococcal_age.rds;
```

## Model

TBA