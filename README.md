# Location-Aware-Cache
Location-Aware Caching Model via Predicting Heterogeneous File Preferences in Mobile Networks

## Introduction
Location-Aware Cache is a implementation of *a*STEAM Project (Next-Generation Information Computing Development Program through the National Research Foundation of Korea (NRF) funded by the Ministry of Science and ICT). The wireless caching aims at deciding which content files need to be cached at users' or helpers' cache storage in terms of maximizing the content utilization (*i.e., the hit ratio*). This model caches content files in each mobile user's device exploiting the heterogeneity in file preferences among mobile users who are moving around different point of interests (*POIs*) in content-centric networks.

## Requirements and Dependencies
- Above development was based on the Rstudio (`64bit`)
- Please import packages (`recommenderlab, reshape2, ggplot2, binaryLogic, dplyr, lsa, proxy, tictoc, RcppAlgos, sparklyr`)

## How to use
* Prepare a data file that contains a set of contents and a set of users who have reacted to some of the contents. (`data format: ['id', 'user', 'content', 'rating']`)
* Input parameters (`file path, # of clusters, # of users, # of time slots, probability of outgoing cluster`)
* Run model