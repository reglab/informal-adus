"""
Verifies whether a permit corresponding to a Detached ADU  (or a more liberal
definition of a permit) exists for a parcel from San Jose's Permit and 
Property Information portal.
https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch

Objective:
    - Return the number of detached ADU permits that exist for an APN
"""

import argparse
import pandas as pd
import os
from tqdm import tqdm

from src.Permits.SJPermitPortal import query_property


def main(args):

    # Load data on annotated construction events from our sample of 15,000 parcels
    batch3 = pd.read_csv(args.input_list, dtype={'apn': str})
    batch3.rename(columns={'apn': 'APN'}, inplace=True)

    # Subset to events in which either the building in 2016 or the building in 2020
    # was larger than or equal to a sqft threshold.
    sqm_threshold = args.sqft_threshold / 10.764
    batch3 = batch3[(batch3['2016_a'] >= sqm_threshold) | (batch3['2020_a'] >= sqm_threshold)]

    # Aggregate at the APN level
    batch3_apn = batch3.groupby('APN')[args.c_event].sum().reset_index()

    # Subset to APNs that have non-zero construction events
    batch3_apn = batch3_apn.loc[batch3_apn[args.c_event] > 0]

    # Set up output file
    if os.path.exists(args.out_file):
        batch3_scraped = pd.read_csv(args.out_file, dtype={'APN': str})
    else:
        batch3_scraped = pd.DataFrame()

    for i in tqdm(range(len(batch3_apn))):
        parcel = batch3_apn.iloc[i]

        if 'APN' in batch3_scraped.columns and parcel['APN'] in batch3_scraped['APN'].unique():
            continue

        # Collect permits that match a detached ADU based on the parcel APN
        permits = query_property(
            apn=parcel['APN'], address=None, headless=True, foldernum=None, all_permits=args.all_permits)

        # Concatenate
        if args.all_permits:
            parcel_df = permits.copy()
            parcel_df['APN'] = parcel['APN']

            if len(parcel_df) == 0 and len(batch3_scraped) > 1:
                parcel_df = {col_name: [None] for col_name in list(batch3_scraped.columns)}
                parcel_df['APN'] = [parcel['APN']]
                parcel_df = pd.DataFrame.from_dict(parcel_df)

        else:
            parcel_df = pd.DataFrame.from_dict(
                {'APN': parcel['APN'], args.c_event: [parcel[args.c_event]], 'num_permits': [len(permits)]})

        if len(batch3_scraped) > 0:
            assert list(parcel_df.columns) == list(batch3_scraped.columns)
        batch3_scraped = pd.concat([batch3_scraped, parcel_df])

        # Save final version
        batch3_scraped.to_csv(args.out_file, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_list", type=str)
    parser.add_argument("--output_dir", type=str)
    parser.add_argument(
        "--c_event", type=str, default='change_area1.2',
        choices=['construction', 'change', 'change_area1.2', 'change_area1.3'])
    parser.add_argument("--all_permits", type=bool, default=False)
    parser.add_argument("--sqft_threshold", type=float, default=120)
    parser.add_argument("--out_file", type=str)

    args = parser.parse_args()
    # Data frame of annotated construction events
    args.input_list = '../../data/raw/processed_buildings.csv'
    args.output_dir = '../../data/processed/Permits'

    os.makedirs(args.output_dir, exist_ok=True)
    if args.all_permits:
        args.out_file = os.path.join(args.output_dir, f"b3_apns_allp_{args.c_event.replace('.', '')}_{args.sqft_threshold}.csv")
    else:
        args.out_file = os.path.join(args.output_dir, f"b3_apns_p_{args.c_event.replace('.', '')}_{args.sqft_threshold}.csv")

    main(args)
