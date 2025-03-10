## PROJECT: FINANCIAL FLOWS TO ECOSYSTEM TIPPING POINTS ##
# AIM: PULLING FINANCIAL FLOWS DATA FROM REFINITIV
# github.com/lyd-m/wwf-tipping-points

### PREREQUSITES -------------------------
# SET UP AND LOGIN TO EIKON DATA API AND REFINITIV DATA PLATFORM ##
# see instructions online here: https://github.com/LSEG-API-Samples/Example.DataLibrary.Python
# download and install anaconda for python
# create a python environmental called "eikon"
# install eikon into this environment
# select this environment as your python interpreter

### DEPENDENCIES --------------------------
# python=3.11 needed for Refinitiv to work

### DIRECTORIES --------------------------
# refinivit/lseg files
import refinitiv.data as rd
from refinitiv.data.errors import RDError
from refinitiv.data.content import search

# data analysis and logging
import pandas as pd
import numpy as np
import openpyxl as xl
import time
import datetime
from rapidfuzz import fuzz, process

# other
import os  # working directories
import re  # regex
import glob


### SIMPLE FUNCTIONS ------------------------
def to_snake_case(str):
    snake_case_string = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", str)
    snake_case_string = snake_case_string.replace(" ", "_").lower()
    return snake_case_string


def to_file_save_format(str):
    file_save_format_string = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", str)
    file_save_format_string = file_save_format_string.replace(" ", "-").lower()
    return file_save_format_string


### SET DATA FILES WORKING DIRECTORY -----------------
path = "/Users/ucliipp/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Documents/programming/main-projects/wwf-tipping-points"
os.chdir(path)

### BEGIN DESKTOP API SESSION ----------------------
# set Refinitiv Data Platform API login key as "app_key_rd" in local env via terminal
# this is necessary due to licence restrictions
# Desktop Refinitiv App needs to be open to do this
# open desktop API session
app_key_rd = "[define API key in local environment]"
rd.session.desktop.Definition(app_key=app_key_rd)
session = rd.session.desktop.Definition(app_key=app_key_rd).get_session()
rd.open_session()


# test to see if session is open
def rd_session_test():
    test = rd.get_data(universe="MSFT.O", fields=["TR.CommonName"])
    return test


rd_session_test().notna().any().any()  # Should return a non-empty dataframe, will read "True" if passed

# max items per request for get_data = 7500

### IMPORT DATA -----------------
asset_classes_flows = ["Bond deals", "Equity deals", "Loan deals"]

raw_flows_files_dict = {
    asset: [
        f
        for f in os.listdir("./intermediate-results/")
        if to_file_save_format(asset) in f
    ]
    for asset in asset_classes_flows
}

ultimate_parents_col_dict = {
    "Loan deals": "All Managers, inc. Int'l Co-Managers, Parent (Full Name)",
    "Bond deals": "All Managers inc Intl Co-Managers Parent",
    "Equity deals": "All Managers inc Intl Co-Managers Parent",
}

ultimate_parents_database = pd.read_excel(
    "./intermediate-results/ultimate_parents_database.xlsx"
)

### FIND ULTIMATE PARENTS, INITIAL MATCHES -------------------

start_exec = time.time()
ups_df = pd.DataFrame()

for asset_class in asset_classes_flows:
    # load in flows data

    file_name = raw_flows_files_dict[asset_class][0]
    deals_df = pd.read_csv(f"./intermediate-results/{file_name}")

    ups = (
        deals_df[ultimate_parents_col_dict[asset_class]]
        .str.split("|")
        .explode()
        .unique()
    )

    # filter out queries that have been searched before to save processing time
    ups_to_search = [
        i for i in ups if i not in ultimate_parents_database["search_query"].values
    ]
    print(f"Searching {len(ups_to_search)} new queries for ultimate parents data")

    retry_max = 5
    retry_count = 1
    for search_query in ups_to_search:
        retry = True
        while retry:
            try:
                search_result = rd.discovery.search(
                    view=rd.discovery.Views.ORGANISATIONS,
                    query=search_query,
                    top=1,  # choose best match
                    select="CommonName, OAPermID, ParentOrganisationName, ParentCompanyOAPermID, UltimateParentOrganisationName, UltimateParentCompanyOAPermID",
                )
                # if no match found, create an empty row for that query with reference data (query, asset class)
                if search_result.empty:
                    search_result = pd.DataFrame(
                        {"search_query": [search_query], "source": [asset_class]}
                    )
                # otherwise store result and add reference data (query, asset class)
                else:
                    search_result["search_query"] = search_query
                    search_result["source"] = asset_class

                # concatenate onto ultimate parents dataframe
                if not ups_df.empty:
                    ups_df = pd.concat(
                        [ups_df, search_result], axis=0, ignore_index=True
                    )
                else:
                    ups_df = search_result  # for first search result need to populate the dataframe

                retry = False  # no exceptino occurred, so set retry to False to exit the loop

            except Exception as e:
                print(f"An error occurred: {e}")
                if retry_count <= retry_max:
                    print(f"Retrying for {search_query}...")
                    retry_count += 1
                    time.sleep(
                        0.1
                    )  # Wait 0.1 second before retrying to prevent overloading the server
                else:
                    print(f"Retry limit reached, no result found for {search_query}.")
                    break
    print(f"Completed searching for ultimate parents for asset class: {asset_class}")
end_exec = time.time()
print(f"This code took {end_exec - start_exec} to run.")

### CHECK QUALITY OF MATCHES -------------------
# use fuzzy matching and checking for gaps to determine which matches to put straight onto the reference data, and which to manually check

# clean names to help with checking


def clean_text(text):
    # Convert non-string data to string
    text = (
        str(text).strip().lower()
    )  # Strip leading/trailing whitespace and convert to lowercase

    # Remove all special characters except spaces
    # text = re.sub(r"[^\w\s]", '', text)
    text = re.sub(r"[^\w\s]|-", " ", text)

    # Replace standalone 'ltd' and 'co' using word boundaries
    # Ensure replacements are done in a non-overlapping manner
    text = re.sub(r"\blimited\b\.?", "ltd", text)
    text = re.sub(r"\bcompany\b\.?", "co", text)
    text = re.sub(r"\bincorporated\b\.?", "inc", text)
    text = re.sub(r"\bcorporation\b\.?", "corp", text)

    return text


# Apply the cleaning function to the 2nd and 3rd columns
ups_df["search_query_clean"] = ups_df["search_query"].apply(clean_text)
ups_df["CommonName_clean"] = ups_df["CommonName"].apply(clean_text)


# function to compute similarity
def fuzzy_match(row):
    return round(
        fuzz.ratio(row["search_query"], row["CommonName"]), 3
    )  # returns similarity %


# compare two columns via fuzzy matching
ups_df["similarity"] = ups_df.apply(fuzzy_match, axis=1)

# check for gaps
columns_to_check = [
    "OAPermID",
    "ParentOrganisationName",
    "ParentCompanyOAPermID",
    "UltimateParentOrganisationName",
    "UltimateParentCompanyOAPermID",
]

# gaps column is "True" if any specified column is empty, False otherwise
ups_df["gaps"] = ups_df[columns_to_check].replace("", np.nan).isna().any(axis=1)


# check for any government organisations
keywords = [
    r"\(government\)",
    "republic of",
    "city of",
    "government of",
    "province of",
    "municipality of",
    "state of",
    "emirate of",
    "canton of",
    "kingdom of",
    "commonwealth of",
    "confederation of",
]

pattern = "|".join(keywords)  # i.e., if contains ANY of the keywords

ups_df["is_government"] = ups_df["UltimateParentOrganisationName"].str.contains(
    pattern, case=False, na=False
)

# flag columns for manual checking
ups_df["manual_check_needed"] = (
    (ups_df["similarity"] < 70)
    | (ups_df["gaps"] == True)
    | (ups_df["is_government"] == True)
).map({True: "TRUE", False: "FALSE"})

## split dataset and add columns that don't need checking to master database
matched_ups = ups_df[ups_df["manual_check_needed"] == "FALSE"]

ultimate_parents_database = pd.concat(
    [
        ultimate_parents_database,
        matched_ups[
            [
                "search_query",
                "CommonName",
                "OAPermID",
                "UltimateParentOrganisationName",
                "UltimateParentCompanyOAPermID",
            ]
        ],
    ],
    axis=0,
    ignore_index=True,
)

# remove any duplicates
ultimate_parents_database = ultimate_parents_database.drop_duplicates()

# overwrite ultimate parents database with new data

ultimate_parents_database.to_excel(
    "./intermediate-results/ultimate_parents_database.xlsx", index=False
)

## send data needing manual checking to a separate file
today = datetime.date.today()
ups_df[ups_df["manual_check_needed"] == "TRUE"].to_csv(
    f"./intermediate-results/ups-manual-checking/to-check/{today}-ups-manual-check.csv"
)
