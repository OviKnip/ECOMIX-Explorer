# ECOMIX-Explorer
Interactive dashboard for ECOMIX data.

The repository now includes a Streamlit implementation in [streamlit_app.py](streamlit_app.py), alongside the original R Shiny code in [app.R](app.R).

This is the web dashboard code for the [ECOMIX](https://water.leeds.ac.uk/fwq-programme/assessing-and-managing-the-impacts-of-mixtures-of-chemicals-on-uk-freshwater-biodiversity/) project, that is part of the [NERC Freshwater Quality Programme](https://water.leeds.ac.uk/event_category/freshwater-quality-programme/).

This project will develop a novel assessment framework for assessing the real impacts of chemical pollution in UK rivers. It will identify and manage hotspots of risk, helping to halt the decline in freshwater biodiversity.

The framework will be developed not only to assess currently chemical impacts but also future impacts resulting from climate change, urbanisation and population growth. It will allow mitigation and adaptation approaches to be targeted where they will have the greatest benefit. The framework will also deliver models for assessing the impacts of chemical mixtures and co-stressors on biodiversity.

This project will:

* investigate the most damaging chemicals being emitted into UK freshwaters
* characterise current (2002-2022) and future (2061-2080) chemical exposure and general water quality parameter profiles for the study catchments
* estimate the effects of chemicals on UK-relevant species
* predict the current and future effects of chemical mixtures on biodiversity and ecosystem function
* identify interventions to mitigate the impacts of chemicals on biodiversity now and under future climate and catchment change.

The modelling tools developed during this project will inform the development of better plans for adaptation and mitigation of risks associated with declining water quality now and in the future. Led by Professor Alistair Boxall, University of York, with partners University of Sheffield and Durham University. 

## Streamlit app

The Streamlit port reproduces the main Shiny workflows:

* subbasin selection on an interactive map
* climate, observations, yearly, monthly, and distribution plots
* spatial dataset mapping
* tabular and spatial downloads
* embedded Julia Dash panel for Food Web Dynamics

### Run locally

Install the Python dependencies:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

Start the Streamlit app:

```bash
streamlit run streamlit_app.py
```

The Food Web Dynamics tab embeds the Julia Dash app at `http://127.0.0.1:8050`, so that service must be running separately if you want the iframe panel to load successfully.

### Original Shiny app

The original R Shiny version remains available in [app.R](app.R).
