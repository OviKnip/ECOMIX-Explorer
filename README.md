# ECOMIX-Explorer
Interactive dashboard for ECOMIX data.

This is the web dashboard code for the [ECOMIX](https://water.leeds.ac.uk/fwq-programme/assessing-and-managing-the-impacts-of-mixtures-of-chemicals-on-uk-freshwater-biodiversity/) project, that is part of the [NERC Freshwater Quality Programme](https://water.leeds.ac.uk/event_category/freshwater-quality-programme/).

## Dashboard

The dashboard ([app.R](app.R)) is an R Shiny app that lets users explore hydrological and chemical exposure data for the study catchments, both as they were observed/simulated historically and as they are projected to change under future climate scenarios. It is organised into six tabs:

* **Map** — search for or browse subbasins on an interactive map, with toggleable layers for subbasin/operational catchment boundaries, waterbodies (modelled), measured sites, observed hydrology sites and chemical data coverage.
* **Site Details** — for a selected measured site or modelled water body (switchable via a toggle in the sidebar), view a chemical occurrence/concentration grid and configurable time-series panels for up to four determinands.
* **Data Explorer** — for a selected subbasin (or, in Water body mode, a selected modelled water body), compare climate variables, simulated vs. observed hydrology, and projections (yearly, monthly, distribution/cumulative-frequency, and daily for chemicals) across scenarios and future periods.
* **Spatial Datasets** — view any modelled hydrological, subbasin chemical, or water body chemical variable as a coloured map layer, for a chosen period/percentile or chemical statistic.
* **Data Downloader** — build and preview a custom data extract (variable, data type, spatial layer, format) and download it as a file.
* **Food Web Dynamics** — an embedded external Julia Dash app for exploring food-web dynamics.

Underlying data (HYPE hydrological model output, chemical concentration predictions for both measured sites and modelled water bodies, and supporting GIS layers) lives in [data/](data/), [data-chem/](data-chem/) and [gis-data/](gis-data/), with data preparation scripts in [scripts/](scripts/).

This project will develop a novel assessment framework for assessing the real impacts of chemical pollution in UK rivers. It will identify and manage hotspots of risk, helping to halt the decline in freshwater biodiversity.

The framework will be developed not only to assess currently chemical impacts but also future impacts resulting from climate change, urbanisation and population growth. It will allow mitigation and adaptation approaches to be targeted where they will have the greatest benefit. The framework will also deliver models for assessing the impacts of chemical mixtures and co-stressors on biodiversity.

This project will:

* investigate the most damaging chemicals being emitted into UK freshwaters
* characterise current (2002-2022) and future (2061-2080) chemical exposure and general water quality parameter profiles for the study catchments
* estimate the effects of chemicals on UK-relevant species
* predict the current and future effects of chemical mixtures on biodiversity and ecosystem function
* identify interventions to mitigate the impacts of chemicals on biodiversity now and under future climate and catchment change.

The modelling tools developed during this project will inform the development of better plans for adaptation and mitigation of risks associated with declining water quality now and in the future. Led by Professor Alistair Boxall, University of York, with partners University of Sheffield and Durham University. 

