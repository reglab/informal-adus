"""
Retrieves the permit corresponding to a Detached ADU (as specified by a folder number) from
San Jose's Permit and Property Information portal.
https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch
"""

import argparse
import pandas as pd
import os
from tqdm import tqdm

from src.Permits.SJPermitPortal import query_permit

tqdm.pandas()


def main(args):

    # Load data frame of SJ detached ADU permits 
    dADU = pd.read_csv(args.detached_list, dtype={'APN': str})
    dADU.rename(columns={'APN': 'Geocoded APN'}, inplace=True)

    # Filter for permits issued 2015-2020
    dADU['Year Issued'] = dADU['DATE'].str[:4].astype(int)
    dADU = dADU.loc[dADU['Year Issued'] < 2021]

    columns = [
        'FOLDERNUMBER', 'Geocoded APN', 'ISSUEDATE', 'SUBDESC', 'WORKDESC',
        'PROP_ADDRESS', 'DATE', 'Year Issued',
        'Folder name', 'Folder Type', 'Sub Category', 'Work Type', 'Description',
        'Status', 'Issue Date', 'Expiry Date', 'Final Date', 'Permit Address',
        'Permit APN', 'Rough Frame Approval Date']

    if os.path.exists(os.path.join(args.output_dir, 'Adu_Detach_Scraped.csv')):
        dADU_scraped = pd.read_csv(
            os.path.join(args.output_dir, 'Adu_Detach_Scraped.csv'),
            dtype={'Permit APN': str, 'Geocoded APN': str}
        )
    else:
        dADU_scraped = pd.DataFrame()

    for i in tqdm(range(len(dADU))):
        adu = dADU.iloc[i]

        if 'Geocoded APN' in dADU_scraped.columns and adu['FOLDERNUMBER'] in dADU_scraped['FOLDERNUMBER'].unique():
            continue

        adu_permits = query_permit(foldernum=adu['FOLDERNUMBER'], headless=True)

        if len(adu_permits) == 0:
            # None values for permit
            adu_df = adu.copy()
            for key in columns:
                if key not in adu.keys():
                    adu_df[key] = None
            adu_df = pd.DataFrame(adu_df).transpose()[columns]
        else:
            adu_df = pd.DataFrame(pd.concat([adu, adu_permits.iloc[0]])).transpose()[columns]

        # Append and save
        dADU_scraped = pd.concat([dADU_scraped, adu_df])
        dADU_scraped.to_csv(os.path.join(args.output_dir, 'Adu_Detach_Scraped.csv'), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--detached_list", type=str)
    parser.add_argument("--output_dir", type=str)

    args = parser.parse_args()
    args.detached_list = '../../data/raw/Permits/Adu_Detach.csv'
    args.output_dir = '../../data/processed/Permits'
    os.makedirs(args.output_dir, exist_ok=True)

    main(args)
