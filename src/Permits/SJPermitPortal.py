"""
Scrapes permit data given an APN or property address from SJ's Permit
and Property Information portal.
https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch
"""

from bs4 import BeautifulSoup
import copy
import pandas as pd
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException, ElementClickInterceptedException
from selenium.webdriver.support.select import Select
from selenium.webdriver.common.by import By
from time import sleep
from tqdm import tqdm
from webdriver_manager.chrome import ChromeDriverManager
import re
from selenium.webdriver.common.keys import Keys

# Scraping functions
def set_up_driver(headless=True):
    # Set up a headless Chrome browser
    options = webdriver.ChromeOptions()
    if headless:
        options.add_argument("headless")
    options.add_argument('log-level=2')

    # Set up driver
    driver = webdriver.Chrome(ChromeDriverManager().install(), options=options)
    driver.maximize_window()
    url = 'https://portal.sanjoseca.gov/deployed/sfjsp?interviewID=PublicPropertySearch'
    driver.get(url)
    sleep(2)
    return driver


def scrape_table(driver, num_table):
    table_bs = BeautifulSoup(driver.page_source, features="html.parser")
    table = table_bs.find_all('table')[num_table]
    table = [[cell.text for cell in row.find_all(["th", "td"])] for row in table.find_all("tr")]
    table = pd.DataFrame(table[1:], columns=table[0])
    table = table.replace(r'\n', '', regex=True)
    table = table.replace(r'\t', '', regex=True)
    return table


# PROPERTIES ============
def search_property(apn=None, address=None, headless=True):
    """

    :param headless: bool
    :param apn: str
    :param address: dict e.g. {'number': 13112, 'street': 'BROWN'}
    :return:
    """
    driver = set_up_driver(headless)

    # Search by APN
    if apn:
        apn_box = driver.find_element(By.ID, "d_1594045115084")
        apn_box.send_keys(apn)
        sleep(1)

        # Search
        driver.find_element(By.NAME, 'd_1594045115086').click()

    if address:
        house_num_box = driver.find_element(By.ID, "d_1594045115076")
        house_num_box.send_keys(address['number'])

        street_box = driver.find_element(By.ID, "d_1594045115078")
        street_box.send_keys(address['street'])
        sleep(1)

        # Search
        driver.find_element(By.NAME, 'd_1594045115085').click()

    sleep(2)
    return driver


def verify_properties(driver):
    # First we check whether there were results for our search
    try:
        # Expand all entries
        driver.find_element(By.XPATH, "//select[@name='DataTables_Table_0_length']/option[text()='All']").click()
        sleep(2)
    except NoSuchElementException:
        return None, 0, [], []

    # Public Property Search Results page
    # Need to extract table of how many property entries exist
    properties = scrape_table(driver=driver, num_table=0)
    num_properties = properties.shape[0]

    if num_properties > 1:
        print('[WARNING] Multiple matches for this property.')

    # Fetch APNs and addresses
    apns = properties['APN Number'].to_list()
    addresses = properties['Street Address'].to_list()

    return driver, num_properties, apns, addresses


def get_public_property_detail(driver, property_num=1):
    # At this stage we click on the first property that shows up
    driver.find_element(By.NAME, f'd_1533175742209[{property_num}]').click()
    sleep(5)
    return driver


def get_property_permits(driver):
    # View Public permit search results
    driver.find_element(By.NAME, f'd_1533175742204').click()
    sleep(2)

    # Expand all entries
    driver.find_element(By.XPATH, "//select[@name='DataTables_Table_0_length']/option[text()='All']").click()
    sleep(2)

    # Scrape permits
    permits = scrape_table(driver=driver, num_table=0)
    return driver, permits


def adu_permits(permits, foldernum):

    adus = permits.loc[
        (permits['Permit Type'] == '1 & 2 Family Detached Dwellings') &
        (permits['Work Type'] == 'New Construction') &
        ((permits['Folder Name'].str.contains('2')) |
         (permits['Folder Name'].str.upper().str.contains('SECOND')))].copy()

    if foldernum:
        adus = permits.loc[permits['Permit #'].str[:-3] == f"20{foldernum}"].copy()

    return adus


def query_property(apn=None, address=None, headless=True, foldernum=None, all_permits=False):
    property_permits = pd.DataFrame()
    drivers = {}

    driver = search_property(apn, address, headless=headless)
    driver, num_properties, apns, addresses = verify_properties(driver)

    if num_properties == 0:
        print('[WARNING] No properties available for this search')
        return pd.DataFrame()

    # Get first property
    driver = get_public_property_detail(driver, property_num=1)
    driver, permits = get_property_permits(driver)
    drivers[1] = driver

    if permits.shape[0] == 0:
        print('[WARNING] No permits available for this property')
        return pd.DataFrame()

    permits['driver'] = 1
    permits['Permit_APN'] = apns[0]
    permits['Permit_Address'] = addresses[0]
    property_permits = pd.concat([property_permits, permits])

    # Get permits for other properties (note that Permit Number is one-indexed
    if num_properties > 1:
        for property_num in range(2, num_properties + 1):
            driver = search_property(apn, address)
            driver, _, _, _ = verify_properties(driver)
            driver = get_public_property_detail(driver, property_num=property_num)
            driver, permits = get_property_permits(driver)

            permits['driver'] = num_properties
            permits['Permit_APN'] = apns[property_num - 1]
            permits['Permit_Address'] = addresses[property_num - 1]
            property_permits = pd.concat([property_permits, permits])
            drivers[property_num] = driver

    if all_permits:
        return property_permits

    # Get detached ADU permits
    adus = adu_permits(property_permits, foldernum)

    return adus


def clean_address(address):
    DIRS = ['N', 'S', 'E', 'W']
    ROADS = ['WY', 'AV', 'LN', 'DR', 'CT', 'RD', 'ST']

    # Note that we have to remove single digits from the street name
    # or else the portal does not return a result. (Whitespace is OK)
    result = re.search(r"(\d+)\s([\w\s]+)", address)
    number, street = result.groups()

    # Remove single digits
    street = re.sub(r"\b\d+\b", "", street)

    # Remove strings not permitted by portal
    for dir in DIRS:
        street = re.sub(rf"\b{dir}\b", "", street)
    for road in ROADS:
        street = re.sub(rf"\b{road}\b", "", street)

    return {'number': number, 'street': street}


# PERMITS ===================
def number_of_permits(driver):
    # Permit search results
    try:
        # Expand all entries
        driver.find_element(By.XPATH, "//select[@name='DataTables_Table_0_length']/option[text()='All']").click()
        sleep(2)
    except NoSuchElementException:
        return 0

    # Check number of permits (there should only be one match)
    permits = scrape_table(driver=driver, num_table=0)
    num_permits = permits.shape[0]

    if num_permits > 1:
        print('[WARNING] Multiple matches for this folder number.')

    return num_permits


def specific_permit_specs(driver, num_subpermit):
    # Go to subpermit page
    try:
        driver.find_element(By.NAME, f'd_1585161890839[{num_subpermit}]').click()
        sleep(3)
    except ElementClickInterceptedException:
        sleep(15)
        driver.find_element(By.NAME, f'd_1585161890839[{num_subpermit}]').click()
        sleep(3)

    subpermit_table = scrape_table(driver=driver, num_table=0)
    approval = subpermit_table.loc[subpermit_table['Result'] == 'Approved']
    approval = str(list(approval['Date'].unique()))
    specs = {'Approval': approval}

    # Return to permit page
    driver.find_element(By.NAME, f'd_1592405017455').click()
    sleep(5)

    return specs


def permit_specs(driver, num_permit):
    driver.find_element(By.NAME, f'd_1584559089314[{num_permit}]').click()
    sleep(3)

    fname = driver.find_element(By.ID, 'div_d_1585161890749').text
    ftype = driver.find_element(By.ID, 'div_d_1585161890752').text
    sub_cat = driver.find_element(By.ID, 'div_d_1585161890753').text
    wtype = driver.find_element(By.ID, 'div_d_1585161890754').text
    desc_box = driver.find_element(By.NAME, f'd_1585161890766').text
    status = driver.find_element(By.ID, f'div_d_1585161890755').text
    issue_d = driver.find_element(By.ID, f'div_d_1585161890757').text
    expiry_d = driver.find_element(By.ID, f'div_d_1603983111512').text
    final_d = driver.find_element(By.ID, f'div_d_1671123289158').text

    # Property details
    driver.find_element(By.NAME, 'd_1594821755326[1]').click()
    sleep(3)
    address = driver.find_element(By.ID, 'div_d_1594821755358').text
    apn = driver.find_element(By.ID, 'div_d_1594821755353').text
    driver.find_element(By.NAME, 'd_1659361738987').click()
    sleep(3)

    # Specific permits
    # Expand all entries
    driver.find_element(By.XPATH, "//select[@name='DataTables_Table_2_length']/option[text()='All']").click()
    sleep(2)
    subpermits = scrape_table(driver=driver, num_table=2)

    # Rough frame
    rf_subpermits = subpermits.loc[subpermits['Process Name'].str.upper().str.contains('ROUGH FRAME')]
    if len(rf_subpermits) > 0:
        rf_id = rf_subpermits.iloc[0].name + 1
        rf_specs = specific_permit_specs(driver=driver, num_subpermit=rf_id)
        rf_approval = rf_specs['Approval']
    else:
        rf_approval = None

    # Final dict containing all permit specs
    specs_dict = {
        'Folder name': [fname],
        'Folder Type': [ftype],
        'Sub Category': [sub_cat],
        'Work Type': [wtype],
        'Description': [desc_box],
        'Status': [status],
        'Issue Date': [issue_d],
        'Expiry Date': [expiry_d],
        'Final Date': [final_d],
        'Permit Address': [address],
        'Permit APN': [apn],
        'Rough Frame Approval Date': [rf_approval]}
    return specs_dict


# Query a permit
def query_permit(foldernum, headless=True):
    driver = set_up_driver(headless)

    # Search
    permit_box = driver.find_element(By.ID, "d_1594640876845")
    permit_box.send_keys(f"20{foldernum}")
    sleep(1)
    driver.find_element(By.NAME, 'd_1594640876846').click()
    sleep(3)

    # Check number of permits and go to permit details page
    num_permits = number_of_permits(driver)
    if num_permits == 0:
        return pd.DataFrame()

    # Get permit specs
    specs_dict = permit_specs(driver, num_permit=1)
    return pd.DataFrame.from_dict(specs_dict)
