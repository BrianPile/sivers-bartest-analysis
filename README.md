# sivers-bartest-analysis

An R-based analysis for Sivers bar test data

**Instructions**

To use this R project as a template for generating reports, there are a few things that need to be setup after cloning the repository for the first time.

**1. Create a config_info.R file**

This file contains the Sivers lotID, Almae wafer ID, path to the Sivers .xls files, and a list of problematic files that can be removed from the processing and analysis.

Inside the file is just a list. Here is the template to copy/paste into a newly created config_info.R file:

config_info = list( waferID = "71636_30", lotID = "P10515", input_data_path = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/1) Test/1.4) Outsource (dropbox)/Sivers/P10515", problem_files = c( "/Users/brianpile/POET Technologies Dropbox/Brian Pile/1) Test/1.4) Outsource (dropbox)/Sivers/P10515/P10515-7/P10515-7_755&756_82A_GP04_dat.xls" ) )

2.  create the \_quarto.yml file

title: "Sivers Bar Test Report: Lot P10515" subtitle: "EEVEE C-band Wafer 71636-30" author: "Brian Pile" date: "2024-07-03" format: html: toc: true execute: echo: false embed-resources: true
