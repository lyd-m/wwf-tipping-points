## PROJECT: FINANCIAL FLOWS TO ECOSYSTEM TIPPING POINTS ##
# PULLING FINANCIAL FLOWS DATA FROM REFINITIV
# github.com/lyd-m/wwf-tipping-points

### PREREQUSITES -------------------------
# SET UP AND LOGIN TO EIKON DATA API AND REFINITIV DATA PLATFORM ##
# see instructions online here: https://github.com/LSEG-API-Samples/Example.DataLibrary.Python
# download and install anaconda for python
# create a python environmental called "eikon"
# install eikon into this environment
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
import string
import pytest

# other
import os #working directories
import re #regex

### SIMPLE FUNCTIONS ------------------------

def to_snake_case(str): 
    snake_case_string = re.sub(r'([a-z0-9])([A-Z])', r'\1_\2', str)
    snake_case_string = snake_case_string.replace(" ", "_").lower()
    return snake_case_string

### SET DATA FILES WORKING DIRECTORY -----------------
path = "/Users/ucliipp/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Documents/programming/main-projects/wwf-tipping-points"
os.chdir(path)

### BEGIN API SESSION ----------------------
# set Refinitiv Data Platform API login key as "app_key_rd" in local env via terminal
# this is necessary due to licence restrictions
# Desktop Refinitiv App needs to be open to do this
# open desktop API session
app_key_rd = '[define API key in local environment]'
rd.session.desktop.Definition(app_key = app_key_rd)
session = rd.session.desktop.Definition(app_key = app_key_rd).get_session()
rd.open_session()

# test to see if session is open
def rd_session_test():
    test = rd.get_data(
    universe='MSFT.O',
    fields = ['TR.CommonName']
    )
    return test

rd_session_test().notna().any().any() # Should return a non-empty dataframe, will read "True" if passed

### INPUT DATA AND FIELDS ------------------------
MAX_ITEMS_PER_REQUEST_FOR_GET_DATA = 7500

# test data for checking code working
permids_test = ["5086635324", "4295903463"]
years_test = [str(year) for year in range(2014, 2025)]  # Convert years to strings
df_companies_permids_test = pd.DataFrame(
    [(perm, year) for perm in permids_test for year in years_test],
    columns=["permid", "year"]
)

# companies to pull financial data for
df_companies = pd.read_excel("/Users/ucliipp/Library/CloudStorage/OneDrive-SharedLibraries-UniversityCollegeLondon/CEP-IIPP P4NE grant 2019-2021 - Documents/General/WWF tipping points/Main research/Empirical paper/Company research/companies.xlsx", sheet_name="yearly company hierarchies")

# collect all permids into one longer df
df_companies_permids = pd.concat([df_companies.loc[:,["company","year","legal_entity_permid"]],
                                  df_companies.loc[:,["company","year","legal_entity_ultimate_parent_permid"]]])

# pivot even longer to only have one column of permids, preserving hierarchy
df_companies_permids = pd.melt(df_companies_permids,
                               id_vars = ['company','year'],var_name='hierarchy_level',value_name='permid')

# drop nas to finalise list
df_companies_permids = df_companies_permids.dropna(subset=['permid']).reset_index()

# make sure they're strings, needed for searching in Refinitiv
df_companies_permids['permid'] = df_companies_permids['permid'].apply(lambda x: str(int(x)) if x == x else "")
df_companies_permids['year'] = df_companies_permids['year'].astype(str)

# years to pull data for 
yrs = list(range(2014,2025))
yrs = list(map(str, yrs))

# set fields with data items to pull for each asset class
flds = pd.read_excel("./input-data/lseg_columns_needed.xlsx")

asset_classes_flows = ["Bond deals", "Equity deals", "Loans"] # seem to need to do capital markets first otherwise it breaks...
asset_classes_stocks = ["Equity holdings", "Bond holdings"]
fundamentals_types = ["Fundamentals - Companies", "Fundamentals - Finance"]

flds_dict_flows = {
    asset: flds.loc[flds["Asset class"].str.contains(asset, na=False), "LSEG field name"].tolist()
    for asset in asset_classes_flows
}

flds_dict_stocks = {
    asset: flds.loc[flds["Asset class"].str.contains(asset,na=False), "LSEG field name"].tolist()
    for asset in asset_classes_stocks
}

flds_dict_fundamentals = {
    type: flds.loc[flds["Asset class"].str.contains(type,na=False), "LSEG field name"].tolist()
    for type in fundamentals_types
}

### PULLING DEALS DATA ------------------------

# function to dynamically create global variables to create output dataframes
def create_variable(name, value):
    globals()[name] = value

# allow for multiple retries in case server times out
retry_max = 5
retry_count = 1

# strings that define the universes for each asset class
universes_dict_flows = {
    "Loans": "SCREEN(U(IN(DEALS)/*UNV:DEALSLOAN*/),",
    "Bond deals": "SCREEN(U(IN(DEALS)/*UNV:DEALSBOND*/),TR.NIisECM=False,",
    "Equity deals": "SCREEN(U(IN(DEALS)/*UNV:DEALSEQ*/),TR.NIisECM=True,"
}

participants_dict_flows = {
    "Loans": "IN(TR.LNParticipant(LNPartRole=LNB,LNBIP,LNBUP),",
    "Bond deals": "IN(TR.NIParticipant(NIDealPartRole=IS,ISIP,ISUP),",
    "Equity deals": "IN(TR.NIParticipant(NIDealPartRole=IS,ISIP,ISUP),"
}

deals_status_dict = {
    "Loans": 'IN(TR.LNStatusOfLoan,"5","4","C"))',
    "Bond deals": 'IN(TR.NITransactionStatus,"LIVE"))',
    "Equity deals": 'IN(TR.NITransactionStatus,"LIVE"))'
}

# pull flows data for all three asset classes
start_exec = time.time()
for asset_class in asset_classes_flows:

    # create empty dataframe to store data for the deals for this asset class
    df = pd.DataFrame()

    # separating out pulls into years to account for changes in hierarchies
    # can change df_companies_permids to df_companies_permids_test (small df) to troubleshoot
    for yr in yrs:
        permids_companies_this_yr = df_companies_permids.loc[df_companies_permids["year"]==yr, "permid"].tolist()
        
        # debugging: check there are permids being searched
        print(f"Year: {yr}, PermIDs found: {len(permids_companies_this_yr)}")
        print(f"Processing {asset_class} for year {yr} with {len(permids_companies_this_yr)} permIDs")

        date_start = yr + "0101"
        date_end = yr + "1231"
        dates_dict_flows = {
            "Loans": f"),BETWEEN(TR.LNTrancheClosingDate,{date_start},{date_end}),",
            "Bond deals": f"),BETWEEN(TR.NIIssueDate,{date_start},{date_end}),",
            "Equity deals": f"),BETWEEN(TR.NIIssueDate,{date_start},{date_end}),"
            }

    # construct refinitiv search for this asset class
        for permid in permids_companies_this_yr:
            retry_count = 1 # reset retry count for each permid
            retry = True

            while retry:
                try: # set up RDP query for this asset class, permid, and year
                    query = universes_dict_flows[asset_class] + participants_dict_flows[asset_class] + permid + dates_dict_flows[asset_class] + deals_status_dict[asset_class]
                
                    # debugging: check query makes sense
                    if retry_count == 1:
                        print(f"Sample query for {asset_class} ({yr}): {query}")

                    # pull data
                    current_df = rd.get_data(
                        universe=[query], 
                        fields = flds_dict_flows[asset_class])

                    # Check if the dataframe is empty
                    if current_df.empty:
                        print(f"No data found for {asset_class} ({yr}) with permid {permid}. Continuing without joining on...")
                    else:
                        # Reset index and remove any duplicates to prevent errors
                        current_df = current_df.drop_duplicates()

                        # Tag results with the queried company permID, asset class for tractability
                        current_df['queried_company_permid'] = permid
                        current_df['asset_class'] = asset_class

                        # Add results to existing df
                        df = pd.concat([df, current_df.reset_index(drop=True)], ignore_index=True, axis=0)

                    # exit loop unless an error occurred, otherwise try again
                    retry = False

                except Exception as e:
                    print(f"An error occurred with {permid} ({yr}): {e}")

                    if retry_count <= retry_max:
                        print("Retrying...")
                        retry_count +=1
                        time.sleep(0.01) # Wait for 0.01 seconds before retrying
                    else:
                        print(f"Retry limit reached, skipping {permid} ({yr})")
                        retry = False # exit retry loop
                        break

        # create global variable including all results for this asset class
    
    df_asset_class_name = "df_" + to_snake_case(asset_class)
    create_variable(df_asset_class_name, df)
    print(f"Completed processing for asset class: {asset_class}")

end_exec = time.time()
print(f'This code tool {end_exec - start_exec} to run')

print(df_loans)
print(df_bond_deals)
print(df_equity_deals)



