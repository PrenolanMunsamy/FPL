Fantasy Premier League Prediction Model System
================

# Fantasy Premier League Prediction Model System

Welcome to the Fantasy Premier League (FPL) Prediction Model System! This project aims to provide a comprehensive solution for predicting FPL points for players in each game week, helping you make informed decisions for your FPL team.

## Table of Contents

-   [Introduction](#introduction)
-   [Data Sources](#data-sources)
-   [Modeling Approach](#modeling-approach)
-   [Weekly Team Selection](#weekly-team-selection)
-   [Future Work](#future-work)
-   [Contributing](#contributing)

## Introduction

The Fantasy Premier League Prediction Model System leverages advanced machine learning techniques to predict the performance of players in upcoming game weeks. This tool is designed to assist FPL managers in optimizing their team selection and transfer strategies, potentially increasing their chances of securing higher points throughout the season.

The project in split into two key stages:
- Model building stage
- Weekly team selection stage

But first, lets talk about the different sets of data that was used.

## Data Sources
- **Historical Player Performance Data:** Sourced from [https://github.com/vaastav/Fantasy-Premier-League/tree/master], this includes detailed statistics on individual player performances for each game week across previous seasons.
- **Fixture Difficulty Ratings:** Provided by [https://github.com/vaastav/Fantasy-Premier-League/tree/master], these ratings indicate the relative difficulty of each player's upcoming matches.
- Additional relevant data to enhance predictions.

## Modeling Approach
- Gameweek, team, fixture difficulty, and player performance data across multiple seasons were aggregated and standardised in format and structure to be worked with in subsequent steps.

- Player names were aligned and corrected between the gameweek and player understat files.

- Corrected player names, fixture difficulty ratings (FDR), team names and IDs, and historical gameweek and player performance data were combined to create the final data. After removing duplicate information, data was preprocessed, joining multiple datasets to create a unified structure. Feature engineering was applied, adding lag and aggregate variables to capture historical performance trends. The final preprocessed dataset was then exported as a CSV file, ready for use in predictive modeling.

- Tidymodels framework was used for the model building. I built separate models for each player position (GK, DEF, MID, FWD). Data was split into training, validation, and test sets based on season and match number. An XGB regression model was trained with predefined hyperparameters, and performance was evaluated using RMSE on the different datasets to make sure we don't overfit/underfit. The final models, along with the model performance summaries were saved and versioned using the vetiver package for easy deployment and reproducibility.

- **Features Included:** 
  - Lagged player performance metrics
  - Team performance metrics
  - Opponent performance metrics
  - Fixture difficulty
  - Other relevant statistics

## Weekly Team Selection
- Upcoming gameweek data was scraped using the official FPL API. Player and fixture information was retrieved, cleaned and merged with team and opponent details for each player.
- Updated and upcoming fixture difficulty ratings (FDR) for teams were calculated based on the next 5 fixtures.
- Team and player data for the current season were processed, cleaned and merged.
- Player names were aligned and corrected between the gameweek and player understat files.
- The initial team for gameweek 1 was selected by doing the following:
 - Sum total points for all players in the previous season
 - Remove players who are no longer in the EPL for the new season
 - Obtain the value for each player in the new season
 - For each position, choose the player with the highest points in the previous season
 - For the remaining 11 players, run a linear optimisation to satisfy all the rules of the FPL game
- For gameweek 2 onwards, process the upcoming week data and use our trained XGB models for each position to predict total points per player for the upcoming week
- Transfer in a player if they have a higher predicted total points value compared to players who are currently in the team while adhering to all FPL rules
- Use the predicted points for all players in our team to decide the formation

## Future Work
- Incorporation of Advanced Statistics: Expanding the dataset to include more detailed player and match statistics.
- Real-Time Data Integration: Developing functionality for real-time data updates to refine predictions.
- Exploration of Additional Modeling Techniques: Investigating other machine learning algorithms and approaches to improve prediction accuracy.

## Contributing
- Prenolan Munsamy