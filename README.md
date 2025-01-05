# CNN-LSTM Model for Predicting Defensive Coverage

This repository contains code and documentation for a Convolutional Neural Network (CNN) combined with a Long Short-Term Memory (LSTM) network designed for the 2025 NFL Big Data Bowl. The model utilizes pre-snap data to predict the defensive coverage a team will employ on a given play.

# Project Overview

Defensive coverage prediction is a challenging task in football analytics, providing valuable insights for teams and analysts. This project leverages modern deep learning techniques to analyze the rich temporal and spatial features of pre-snap data, including player positioning, motion, and contextual game information.

# Objectives

* Develop a robust deep learning model combining CNNs and LSTMs to capture spatial and temporal dependencies in pre-snap data.
* Evaluate the model’s performance against real-world NFL datasets.
* Provide actionable insights that can help offensive coordinators design better game plan
* Provide metrics to better evaluate offensive and defensive coordinators

# Dataset
* Game data: The games.csv contains the teams playing in each game. The key variable is gameId.
* Play data: The plays.csv file contains play-level information from each game. The key variables are gameId and playId.
* Player data: The players.csv file contains player-level information from players that participated in any of the tracking data files. The key variable is nflId.
* Player play data: The player_play.csv file contains player-level stats for each game and play. The key variables are gameId, playId, and nflId.
* Tracking data: Files tracking_week_[week].csv contain player tracking data from week number [week]. The key variables are gameId, playId, and nflId.
# Data Preprocessing

# Feature Engineering:

1. Transform player tracking data into a 5D array with dimensions consisting of plays by frames by defensive players by offensive skill position players by features
2. Removed frames before the offensive line was set, after the snap, and when the defense didn't have 11 players on the field 
3. Include derived features such as defenders x and y positioning (vertical distance from LOS and horizontal distance from ball), defenders x and y positioning relative to each offensive player and defenders oreination relative to the ball
4. Encode coverage types as categorical labels.

# Model Architecture

* 2D Convolutional Layer: Analyzed the interactions between offensive and defensive players.
* Pooling Layer: Combined 30% max pooling and 70% average pooling to process the output of the 2D convolutional layer.
* 1D Convolutional Layer: Focused on analyzing defensive players individually.
* Second Pooling Layer: Again combined 30% max pooling and 70% average pooling.
* Time Distribution Layer: Applies the CNN model to all frames of each play.
* Masking Layer: Instructed the model to ignore zero-padded frames in the frames dimension.
* LSTM Layer: : Enabled the model to analyze all frames pre-snap, capturing both offensive player movements and defensive shifts.
* Dense Layer: Applied a softmax activation function to return probabilities for the eight common coverages a team might run (Cover 0, Cover 1, Cover 2 Man, Cover 2 Zone, Cover 3, Cover 4, Cover 6, and Prevent Defense).

# Results

Accuracy: 68.5%

Script order to run the model: Data.R -> Data_Preprocessing.R -> CNN_LSTM.R
