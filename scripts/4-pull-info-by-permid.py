### PREREQUSITES -------------------------
# SET UP AND LOGIN TO EIKON DATA API AND REFINITIV DATA PLATFORM ##
# see instructions online here: https://github.com/LSEG-API-Samples/Example.DataLibrary.Python
# download and install anaconda for python
# create a python environmental called "eikon"
# install eikon into this environment
# select this environment as your python interpreter in VScode or other IDE

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

# other
import os  # working directories
import re  # regex

### SIMPLE FUNCTIONS ------------------------
def to_snake_case(str):
    snake_case_string = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", str)
    snake_case_string = snake_case_string.replace(" ", "_").lower()
    return snake_case_string

today = time.strftime("%Y%m%d")  # update each time run code

### SET DATA FILES WORKING DIRECTORY -----------------
path = "/Users/ucliipp/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Documents/programming/main-projects/wwf-tipping-points"
os.chdir(path)

### BEGIN API SESSION ----------------------
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

### SEARCH FOR INFO BY PERMID ------------------

## import fields needed ##
flds = pd.read_excel("./input-data/lseg_columns_needed.xlsx")

## import permids ##
# financial institutions
ultimate_parents_ref_data = pd.read_excel(
    "./intermediate-results/ultimate_parents_database.xlsx"
)

permids_finance_df = pd.concat(
    [
        ultimate_parents_ref_data["OAPermID"].rename({"OAPermID":"permid"}),
        ultimate_parents_ref_data["UltimateParentCompanyOAPermID"].rename({"UltimateParentCompanyOAPermID":"permid"}),
    ],
    axis=0,
).drop_duplicates()

permids_finance_list = permids_finance_df.astype("Int64").astype(str).to_list()

# companies
companies_ref_data = pd.read_excel(
    "/Users/ucliipp/Library/CloudStorage/OneDrive-SharedLibraries-UniversityCollegeLondon/CEP-IIPP P4NE grant 2019-2021 - Documents/General/WWF tipping points/Main research/2025 Empirical paper/Company research/companies.xlsx"
, sheet_name="yearly company hierarchies") # update path with relative path in future once companies finalised

permids_companies_df = pd.concat(
    [
        companies_ref_data["legal_entity_permid"].rename({"legal_entity_permid":"permid"}),
        companies_ref_data["legal_entity_ultimate_parent_permid"].rename({"legal_entity_ultimate_parent_permid":"permid"}),
    ],
    axis=0,
).drop_duplicates()

permids_companies_list = permids_companies_df.astype("Int64").astype(str).to_list()

## search with rd.get_data ##

# financial institutions

flds_finance_info = flds.loc[flds["Asset class"].str.contains("Fundamentals - Finance",na=False), "LSEG field name"].tolist()

info_by_permid_finance = rd.get_data(
    universe=permids_finance_list,
    fields=flds_finance_info,
    parameters={"Scale": "6"},
)

# clean column names
info_by_permid_finance.columns = [to_snake_case(col) for col in info_by_permid_finance.columns]

## tidying up columns
info_by_permid_finance = info_by_permid_finance.rename({"Instrument":"permid"})

### adding flag for government owned
ultimate_parents_ref_data_long = (
    ultimate_parents_ref_data.melt(id_vars=["UltimateParentOrganisationName"], value_vars=["OAPermID", "UltimateParentCompanyOAPermID"],var_name="oa_permid_type",value_name="permid")
    .drop(columns=['oa_permid_type'])
    .drop_duplicates(subset=['permid'])
) # i.e., the manually assigned and checked ultimate parents for each permid

ultimate_parents_ref_data_long.columns = [to_snake_case(col) for col in ultimate_parents_ref_data_long.columns]

info_by_permid_finance = info_by_permid_finance.merge(
    ultimate_parents_ref_data_long, on="permid", how="left"
)

#organization_ultimate_parent is the name from info_by_permid_finance, ultimate_parent_organisation_name is the name from ultimate_parents_ref_data

# fill in missing organization_ultimate_parent names with ultimate_parent_organisation_name
info_by_permid_finance["organization_ultimate_parent"] = info_by_permid_finance["organization_ultimate_parent"].fillna(info_by_permid_finance["ultimate_parent_organisation_name"])

# drop ultimate parent organisation name
info_by_permid_finance = info_by_permid_finance.drop(columns=["ultimate_parent_organisation_name"])

government_keywords = r"\(government\)|republic of|city of|government of|province of|municipality of|state of|emirate of|canton of|kingdom of|commonwealth of|confederation of"

info_by_permid_finance["government_ultimate_parent"] = info_by_permid_finance["organization_ultimate_parent"].str.contains(government_keywords, case=False, na=False)

# Handle missing values explicitly
info_by_permid_finance['government_ultimate_parent'] = info_by_permid_finance['government_ultimate_parent'].where(~info_by_permid_finance['organization_ultimate_parent'].isna(), other=pd.NA)

info_by_permid_finance.to_csv(
    f"./intermediate-results/financial_institutions_info_by_permid.csv"
)


# companies

flds_companies_info = flds.loc[flds["Asset class"].str.contains("Fundamentals - Companies",na=False), "LSEG field name"].tolist()

info_by_permid_companies = rd.get_data(
    universe=permids_companies_list,
    fields=flds_finance_info,
    parameters={"Scale": "6"},
)

info_by_permid_companies.to_csv(
    f"./intermediate-results/companies_info_by_permid.csv"
)