#
# Defines the stratification of census block groups by 2016 household median income.
#

import seaborn as sns
import geopandas as gpd
import pandas as pd
import os
from tqdm import tqdm

tqdm.pandas()


def main():
    # Paths
    INPUT_FP = os.path.join('outputs', 'Population-Estimates', 'inputs')
    OUTPUT_FP = os.path.join('outputs', 'Population-Estimates', 'outputs')

    # * San Jose residential parcels
    SJ_RES_PARCELS_FP = os.path.join('san_jose_suppl', 'san_jose_parcels_res.geojson')

    # Load data
    # * SJ CBGs
    cbg = gpd.read_file(os.path.join('shapefiles', 'tl_2016_06_bg'))
    cbg_scc = cbg.loc[cbg['COUNTYFP'] == '085']

    # ** Get San Jose city SHP
    scc_cities = gpd.read_file(os.path.join('shapefiles', 'City_Limits'))
    sj_city = scc_cities.loc[scc_cities['NAME'] == 'SAN JOSE']

    # ** Get CBGs in SJ city
    cbg_scc = cbg_scc.to_crs(sj_city.crs)
    cbg_sj_2016 = gpd.clip(cbg_scc, sj_city)

    # * Residential parcels
    sj_parcels_res = gpd.read_file(SJ_RES_PARCELS_FP)
    sj_parcels_res = sj_parcels_res[sj_parcels_res['APN'].notna()]

    # * CBG income (includes interpolation for 15 CBGs with missing income data)
    cbg_income_2016 = gpd.read_file(os.path.join(INPUT_FP, 'cbg_income_2016'))

    # * Parcel confidences
    parcel_conf_df = pd.read_csv(
        os.path.join(OUTPUT_FP, 'Confidences_construction', 'parcel-confidence.csv'), dtype={'APN': str})

    # 1. Income-level stratification
    # Shapefiles include 643 CBGs as of 2016 for San Jose. ACS has non-missing data for 628 CBGs; interpolated data
    # has data for 13 out of these 15 CBGs. The other 2 CBGs include a county jail containing 7 residential parcels
    # and a residential CBG containing only 24 residential parcels. We drop these two CBGs.

    # Filter for SJ CBGs
    cbg_income_2016_SJ = cbg_income_2016.loc[cbg_income_2016['GEOID'].isin(
        cbg_sj_2016['GEOID'].unique())].copy()

    # Check missing CBGs
    cbg_income_2016_SJ.info()
    missing = cbg_income_2016_SJ.loc[cbg_income_2016_SJ['median_inc'].isna()]

    # Stratify
    N_INCOME_BINS = 50
    cbg_income_2016_SJ['strata_income'] = pd.qcut(
        cbg_income_2016_SJ['median_inc'], q=N_INCOME_BINS,
        labels=['IS_{}'.format(i) for i in range(N_INCOME_BINS)])

    # Add strata for the missing CBGs
    cbg_income_2016_SJ['strata_income'] = cbg_income_2016_SJ['strata_income'].fillna('IS_MISSING')

    cbg_income_2016_SJ.to_file(os.path.join(OUTPUT_FP, 'Strata', 'cbg-income-strata'))

    # 2. Assign parcels to CBGs
    sj_parcels = sj_parcels_res[['APN', 'geometry']].copy()

    # Drop duplicate parcels
    sj_parcels.drop_duplicates(subset=['APN'], inplace=True)
    print('Number of parcels: {}'.format(len(sj_parcels)))

    # Filter parcels by area. We use an area threshold of 5,000 square meters as this
    # provides a good trade-off between removing some non-SFRs in this area interval
    # such as churches and apartment complexes, and starting to capture large SFRs
    sj_parcels['area'] = sj_parcels.to_crs('EPSG:26910').geometry.area

    AREA_THRESHOLD = 5000
    sj_parcels = sj_parcels.loc[sj_parcels['area'] <= AREA_THRESHOLD]
    print('Number of parcels: {}'.format(len(sj_parcels)))

    # Assign to CBGs
    cbg_income_2016_SJ.reset_index(inplace=True, drop=True)
    sj_parcels = sj_parcels.sjoin(cbg_income_2016_SJ[['GEOID', 'geometry']], how='left', predicate='intersects')

    sj_parcels['CBG_iou'] = sj_parcels.progress_apply(
        lambda row: 0 if pd.isnull(row['index_right']) else row['geometry'].buffer(0).intersection(
            cbg_income_2016_SJ.iloc[int(row['index_right'])]['geometry'].buffer(0)).area / row['geometry'].buffer(
            0).area,
        axis=1)

    # Handle multiple matches
    sj_parcels.sort_values('CBG_iou', ascending=False, inplace=True)
    sj_parcels.drop_duplicates(subset=['APN'], keep='first', inplace=True)

    sj_parcels.drop('index_right', axis=1, inplace=True)
    print('Number of parcels: {}'.format(len(sj_parcels)))

    # Save
    sj_parcels[['APN', 'GEOID', 'geometry']].to_file(os.path.join(OUTPUT_FP, 'sj-parcels-res-cbgs'))


if __name__ == '__main__':
    main()
