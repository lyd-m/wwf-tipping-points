# select this environment as your python interpreter in vscode

### DEPENDENCIES --------------------------
# python=3.11

### DIRECTORIES --------------------------
# refinivit/lseg files
import refinitiv.data as rd
from refinitiv.data.errors import RDError
from refinitiv.data.content import search

# data analysis and logging
import pandas as pd
import openpyxl as xl
import time
import datetime
import string
import pytest
from rapidfuzz import fuzz, process

# other
import os  # working directories
import re  # regex

### SET DATA FILES WORKING DIRECTORY -----------------
path = "/Users/ucliipp/Library/CloudStorage/OneDrive-SharedLibraries-UniversityCollegeLondon/CEP-IIPP P4NE grant 2019-2021 - Documents/General/WWF tipping points/Main research/Empirical paper/Company research"
os.chdir(path)

### BEGIN API SESSION ----------------------
# set Refinitiv Data Platform API login key as "app_key_rd" in local env via terminal
app_key_rd = "[define API key in local environment]"
rd.session.desktop.Definition(app_key=app_key_rd)
session = rd.session.desktop.Definition(app_key=app_key_rd).get_session()
rd.open_session()


# test to see if session is open
def rd_session_test():
    test = rd.get_data(universe="MSFT.O", fields=["TR.CommonName"])
    return test


rd_session_test().notna().any().any()  # Should return a non-empty dataframe, will read "True" if passed

### INPUT DATA AND FIELDS ---------------

gfw_logging = pd.read_excel(
    "./Boreal-Canada/Output-data/canada-logging-companies-to-search.xlsx"
)


### EXPLORING SIMILARITY BETWEEN COMPANY NAMES ----------------
# similarity comparison function
def fuzzy_match(row):
    return round(
        fuzz.ratio(row["gfw_name_clean"], row["gfw_next_name_clean"]), 3
    )  # returns similarity %


# clean text function
def clean_text(text):
    # Convert non-string data to string
    text = (
        str(text).strip().lower()
    )  # Strip leading/trailing whitespace and convert to lowercase

    # Remove all special characters except spaces
    text = re.sub(r"[^\w\s]|-", " ", text)

    return text


# add column that includes the entry from below
gfw_logging["gfw_next_name"] = gfw_logging["gfw_name"].shift(-1)

# clean both columns
gfw_logging["gfw_name_clean"] = gfw_logging["gfw_name"].apply(clean_text)
gfw_logging["gfw_next_name_clean"] = gfw_logging["gfw_next_name"].apply(clean_text)

# string comparison
gfw_logging["name_comparison"] = gfw_logging.apply(fuzzy_match, axis=1)
gfw_logging = gfw_logging.sort_values(by="name_comparison", ascending=False)

gfw_logging.to_excel(
    "./Boreal-Canada/Output-data/canada-logging-companies-eda.xlsx", index=False
)

# import consolidated list
logging_companies = pd.read_excel(
    "./Boreal-Canada/Output-data/canada-logging-companies-consolidated.xlsx"
)

## Importing the logging overlap data
boreal_logging = pd.read_csv(
    "./Boreal-Canada/Output-data/canada_logging_overlap_split.csv"
)

# joining consolidated names onto the overlap data
boreal_logging = boreal_logging.merge(
    logging_companies[["gfw_name", "final_name"]],
    left_on="Company",
    right_on="gfw_name",
    how="left",
)

# grouping by company

boreal_logging_grouped = (
    boreal_logging.groupby("final_name", as_index=False)
    .agg({"Boreal_overlap_area_km": "sum"})
    .sort_values(by="Boreal_overlap_area_km", ascending=False)
)

boreal_logging_grouped.to_excel(
    "./Boreal-Canada/Output-data/canada-logging-overlap-grouped.xlsx", index=False
)
