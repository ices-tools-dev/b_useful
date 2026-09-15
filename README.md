# B USEFUL Decision Support Tool (DST)

This repo contains the application code for the DST, available on the [ICES website - Data - Assessment Tools](https://ices.dk/data/assessment-tools/Pages/B-USEFUL-DST.aspx)
See https://b-useful.eu/ for details on the b-useful project

repo [b_useful_data](https://github.com/ices-tools-dev/b_useful_data) processes the modelling output data.

data-raw/00_prep_project_data.R copies the processed modelling data and preps remaining texts, themes etc. --> Correct paths for your system.

# Purpose

The app disseminates the outputs of the B-USEFUL project, with the intention to advance the use of marine biodiversity data in Marine Spatial Planning.
B-USEFUL utilises joint species distribution modelling (HMSC) to model individual species as part of a community --> this results in predictions of species distributions at the species level. Utilising the combined output allows us to say something about fish / demersal biodiversity more broadly. The application is organised accordingly, starting with species distributions before moving to biodiversity. Finally, an Interactive Tool provides users with the opportunity to combine biodiversity and spatial filters to understand the model outputs at a finer scale.

# Usage and citation
The B-USEFUL DST follows the [ICES Data Policy](https://www.ices.dk/data/guidelines-and-policy/pages/ices-data-policy.aspx), and makes the B-USEFUL's model outputs available under the [CC BY 4.0 license.](https://creativecommons.org/licenses/by/4.0/)<br>
Please see the [Disclaimer](https://github.com/ices-tools-dev/b_useful_data/blob/main/Disclaimer_B-USEFUL.txt).<br>
Please feel free to download and use the data, providing attribution to the authors.<br>
Please cite both the application and the underlying research data when using the B-USEFUL DST.

### The recommended citation for use of B-USEFUL application is: <br>
B-USEFUL Decision Support Tool, [date accessed]. ICES, Copenhagen, Denmark. https://www.ices.dk/data/assessment-tools/Pages/B-USEFUL-Decision-Support-Tool.aspx

### The recommended citation for use of B-USEFUL data is: <br>
B-USEFUL. Report on temporal trends and spatial patterns of multiple biodiversity indicators. Technical University of Denmark (2025). Martin Lindegren, Manuel Hidalgo, Marcel Montanyes, Federico Maioli, Benjamin Weigel,
Daniel van Denderen, Gleb Tikhnov, Otso Ovaskainen, Baptiste Degueurce, Tim Jimenez,
Francesco Golin, Ingibjörg G. Jónsdóttir, Haseeb Randhawa, Julian Burgos, Walter Zupa,
Antonella Consiglio, Matteo Chiarini, Maria Teresa Spedicato, Patricia Puerta, Alicia Gran,
Shannon Moore, Laurene Pecuchet, Murray Thompson, Lily Greig, Keith Cooper, Georg
Engelhard, Justin Tiago, Marcel Rozemeijer, Sofia Henriques, André Martins, Corina Chaves,
Rita Vasconcelos, Teresa Moura. <br><br>
The data is available as a zipfile bundle per model. Go to releases to access the download link.



##DST Overview
```mermaid
flowchart LR

    A["B-USEFUL<br/>biodiversity research outputs"]
    B["Choose marine region"]
    C["Regional biodiversity &<br/>species model data"]

    D["Species<br/>Distributions"]
    E["Biodiversity<br/>Development"]
    F["Interactive<br/>Spatial Tool"]

    G["Explore species<br/>patterns"]
    H["Explore biodiversity<br/>change & trends"]
    I["Filter biodiversity<br/>and spatial criteria"]

    J["Maps • Comparisons • Statistics"]
    K["Marine Spatial Planning<br/>decision support"]

    A --> B --> C

    C --> D --> G
    C --> E --> H
    C --> F --> I

    G --> J
    H --> J
    I --> J

    J --> K
```

## App Modules and User Flow
```mermaid
flowchart TD

    START["app.R"]
    RUN["run_app()"]
    SHINY["shinyApp()<br/>app_ui + app_server"]

    START --> RUN --> SHINY

    subgraph UI["User interface — app_ui()"]
        NAV["Main navbar"]

        HOME["Home<br/>mod_home_ui()"]
        BACK["Background<br/>mod_story_map_ui()"]
        RESULTS["Model Outputs<br/>regional mod_results_ui()"]
        DOWNLOAD["Data Download<br/>mod_downloads_ui()"]
        RESOURCES["Resources<br/>mod_resources_ui()"]

        NAV --> HOME
        NAV --> BACK
        NAV --> RESULTS
        NAV --> DOWNLOAD
        NAV --> RESOURCES
    end

    SHINY --> NAV

    HOME --> REGION["User selects<br/>case-study region"]

    REGION --> LOCATION["selected_locations<br/>reactiveVal"]

    subgraph SERVER["Server — app_server()"]
        WATCH["Watch region / navbar selection"]
        SELECT["Determine selected<br/>case study"]
        CALL["Call matching<br/>mod_results_server()"]

        WATCH --> SELECT --> CALL
    end

    LOCATION --> WATCH
    RESULTS --> WATCH

    subgraph RESULT["Results module — mod_results_server()"]
        CONFIG["Select regional<br/>data configuration"]

        DATA["Create reactive data sources<br/>diversity • trends • species<br/>spatial data • diagnostics"]

        MODULES{"Results tabs"}

        CONFIG --> DATA --> MODULES
    end

    CALL --> CONFIG

    %% Species distributions
    MODULES --> SPECIES["Species Distributions<br/>mod_species_distributions_server()"]

    %% Biodiversity development
    MODULES --> BIO["Biodiversity Development<br/>mod_time_series_and_trends_server()"]

    subgraph BIODETAIL["Biodiversity Development"]
        ANIMATION["Spatial development<br/>mod_diversity_animation"]
        COMPARE["Compare time periods<br/>mod_wp3_time_comparison"]
        TRENDS["Temporal trends<br/>mod_wp3_trends_server()"]
    end

    BIO --> ANIMATION
    BIO --> COMPARE
    BIO --> TRENDS

    %% Interactive tool
    MODULES --> TOOL["Interactive Tool<br/>mod_interactive_tool_server()"]

    subgraph TOOLDETAIL["Interactive spatial analysis"]
        DIVFILTER["Diversity filters<br/>select biodiversity criteria"]
        SPATIALFILTER["Spatial filters<br/>restrict geographic area"]
        STATS["Diversity statistics<br/>summarise selected area"]
    end

    TOOL --> DIVFILTER
    TOOL --> SPATIALFILTER

    DIVFILTER --> STATS
    SPATIALFILTER --> STATS

    %% Outputs
    SPECIES --> OUTPUT["Reactive maps,<br/>plots & statistics"]

    ANIMATION --> OUTPUT
    COMPARE --> OUTPUT
    TRENDS --> OUTPUT
    STATS --> OUTPUT

    OUTPUT --> USER["Displayed to user"]
```
