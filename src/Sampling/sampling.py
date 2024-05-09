#
# Generates the initial (simple random sample) sample of 5,000 parcels and the stratified random sample of 15,006 parcels
#

from tqdm import tqdm
import pandas as pd
import geopandas as gpd
import os
import numpy as np

tqdm.pandas()


def main():
    # Load data
    # * SJ Residential parcels (assigned to CBGs)
    sj_parcels_res = gpd.read_file(os.path.join('sj-parcels-res-cbgs'))
    assert sj_parcels_res.duplicated('APN').sum() == 0

    # * SJ Census block groups (including income data)
    cbg_income_2016_SJ = gpd.read_file(os.path.join('Strata', 'cbg-income-strata'))

    # * SJ Permits
    permits = pd.read_csv(os.path.join('outputs/Permit-Matching/outputs/all_permits.csv'))
    permits['geometry_permit'] = gpd.GeoSeries.from_wkt(permits['geometry_permit'])

    # * Parcel confidences for all parcels (based on model confidence metric)
    parcel_conf_df = pd.read_csv(
        os.path.join('Confidences_construction', 'parcel-confidence.csv'),
        dtype={'APN': str, 'GEOID': str})

    # * Simple Random Sample (SRS) Area coverage
    CF_area_coverage = gpd.read_file(os.path.join('Outcomes', 'B1-100', 'CF_area_coverage'))

    # * SRS Estimates (small building constructions)
    new_sbuild_2020 = pd.read_csv(
        os.path.join('Outcomes', 'B1-100', 'permit-matching-apns-CF-min11-GT1.csv'),
        dtype={'APN': str})

    # Add income strata information to parcel data
    sj_parcels_res = pd.merge(
        sj_parcels_res, cbg_income_2016_SJ[['GEOID', 'strata_inc']],
        how='left', validate='many_to_one')

    # 1. Simple Random Sample of 5,000 parcels (Sample 3.5% of parcels) ===========================
    percentage_of_samples = 0.035
    iteration1 = pd.DataFrame()
    for strata_inc in tqdm(sj_parcels_res['strata_inc'].unique()):
        parcels_to_review = sj_parcels_res.loc[sj_parcels_res['strata_inc'] == strata_inc]['APN'].unique()

        # Sample parcels
        np.random.seed(42)
        strata_inc_N = len(parcels_to_review)
        parcels_to_review = np.random.choice(
            parcels_to_review, size=int(strata_inc_N * percentage_of_samples), replace=False)

        for i, parcel_apn in enumerate(parcels_to_review):
            parcel_dict = {'APN': [parcel_apn], 'strata_inc': [strata_inc]}
            iteration1 = pd.concat([iteration1, pd.DataFrame.from_dict(parcel_dict)])

    # 2. Stratified Random Sample: Neyman allocation ==============================
    # Number of samples in the stratified sample per the power analysis (see Appendix D.3 for details)
    N_iter3 = 15005

    neyman = sj_parcels_res.copy()

    # Add parcel confidence data and generate income-confidence bins
    neyman = pd.merge(neyman, parcel_conf_df[['APN', 'confidence']], how='left', validate='one_to_one')

    print('[INFO] Dropping {} parcels without confidence data'.format(neyman['confidence'].isna().sum()))
    neyman = neyman.loc[~neyman['confidence'].isna()]

    neyman['Bin'] = neyman.apply(
        lambda row: '{}-{}'.format(row['strata_inc'], row['confidence']), axis=1)

    print('[INFO] We have {} income strata, {} confidence bins and {} final bins for the Neyman alloc'.format(
        len(neyman['strata_inc'].unique()),
        len(neyman['confidence'].unique()),
        len(neyman['Bin'].unique())
    ))

    # Compute Yij (number of new small buildings in a parcel)
    # * Add all parcels from the SRS
    new_sbuild_2020 = pd.merge(
        CF_area_coverage[['APN']],
        new_sbuild_2020[['APN', 'count_new_sbuild']], how='left', validate='one_to_one')
    new_sbuild_2020['count_new_sbuild'] = new_sbuild_2020['count_new_sbuild'].fillna(0)

    neyman = pd.merge(
        neyman, new_sbuild_2020[['APN', 'count_new_sbuild']], how='left', validate='one_to_one')
    neyman['Yij'] = neyman['count_new_sbuild']

    # Check how many SRS parcels there are in each of the 150 bins (ie how many
    # parcels are being used to estimate Sj for a bin)
    nparcels_b1 = neyman.notna().groupby(neyman['Bin'])['Yij'].sum().reset_index()

    # Find S_j, N_j
    Neyman_alloc = neyman.groupby('Bin').agg(
        {'APN': 'count', 'Yij': 'std'}).reset_index()
    Neyman_alloc.rename(columns={'APN': 'N_j', 'Yij': 'S_j'}, inplace=True)

    # For bins that do not have B1 observations, we use the mean Sj
    empty_bins = Neyman_alloc['S_j'].isna().sum()
    print('[INFO] {} bins do not have B1 parcels we can use to estimate Sj:'.format(
        empty_bins))
    if empty_bins > 0:
        print(Neyman_alloc.loc[Neyman_alloc['S_j'].isna()])
        median_Sj = Neyman_alloc['S_j'].median()
        Neyman_alloc['S_j'] = Neyman_alloc['S_j'].apply(lambda sj: median_Sj if pd.isnull(sj) else sj)

    # Smoothing: as many parcels did not have new buildings, we set S_j = min(S_j, min S_i),
    # where S_i > 0 so that each bin has a non-zero probability of being sampled from.
    min_Sj = Neyman_alloc.loc[Neyman_alloc['S_j'] > 0]['S_j'].min()
    Neyman_alloc['S_j'] = Neyman_alloc['S_j'].apply(
        lambda sj: sj if sj > 1e-5 else min_Sj)

    Neyman_denom = (Neyman_alloc['N_j'] * Neyman_alloc['S_j']).sum()
    Neyman_alloc['weight'] = Neyman_alloc.apply(
        lambda row: row['N_j'] * row['S_j'] / Neyman_denom, axis=1)

    # Check that sum of Neyman weights equals 1
    assert Neyman_alloc['weight'].sum() - 1 < 1e-5

    # Determine number of samples per bin
    Neyman_alloc['n_j'] = N_iter3 * Neyman_alloc['weight']
    Neyman_alloc['n_j'] = Neyman_alloc['n_j'].apply(lambda nj: round(nj, 0))

    print('[INFO] Number of parcels to sample: {}'.format(int(Neyman_alloc['n_j'].sum())))

    # Sample n_j samples for each bin
    np.random.seed(42)

    iteration3_parcels = []
    for final_bin in Neyman_alloc['Bin'].unique():
        n_j = Neyman_alloc.loc[Neyman_alloc['Bin'] == final_bin].iloc[0]['n_j']
        parcels_bin = neyman.loc[neyman['Bin'] == final_bin]['APN'].unique()
        try:
            sampled_parcels_bin = np.random.choice(parcels_bin, size=int(n_j), replace=False)
        except ValueError:
            sampled_parcels_bin = np.random.choice(parcels_bin, size=len(parcels_bin), replace=False)
            print('Sampling below Neyman alloc for bin {}: Num parcels: {}; nj: {}'.format(
                final_bin, len(parcels_bin), n_j))
        iteration3_parcels.extend(sampled_parcels_bin.tolist())

    iteration3 = neyman.copy()
    iteration3 = iteration3.loc[iteration3['APN'].isin(iteration3_parcels)]
    print('[INFO] Sampling {} parcels'.format(len(iteration3)))

    # Make a full database of parcels in the stratified sample
    db = neyman.copy()
    # * Add median income
    db = db.merge(cbg_income_2016_SJ[['GEOID', 'median_inc']], validate='many_to_one')
    # * Add WLS weights
    db = db.merge(Neyman_alloc[['Bin', 'N_j', 'n_j', 'weight']], validate='many_to_one', how='left')
    db['true_weight'] = db['N_j'] / Neyman_alloc['N_j'].sum()
    db['Neyman_weight'] = db['weight']


if __name__ == '__main__':
    main()
