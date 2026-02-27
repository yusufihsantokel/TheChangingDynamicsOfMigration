# _author_ = 'sixueyang'
# _data_ = 2024/7/4 16:15
import os
import pandas as pd
from collections import defaultdict


country_map = {
    "CANADA": "Canada",
    "MEXICO": "Mexico",
    "CHINA": "China",
    "COLOMBIA": "Colombia",
    "INDIA": "India",
    "RUSSIA": "Russian Federation",
    "VENEZUELA": "Venezuela, RB",
    "UKRAINE": "Ukraine",
    "EL SALVADOR": "El Salvador",
    "CUBA": "Cuba",
    "ECUADOR": "Ecuador",
    "GUATEMALA": "Guatemala",
    "HAITI": "Haiti",
    "PHILIPPINES": "Philippines",
    "HONDURAS": "Honduras",
    "NICARAGUA": "Nicaragua",
    "ROMANIA": "Romania",
    "BRAZIL": "Brazil",
    "MYANMAR (BURMA)": "Myanmar",
    "PERU": "Peru",
    "TURKEY": "Turkiye"
}





if __name__ == '__main__':
    out_name = ['Country Name','Indicator Name','2021','2022','2023']
    file_dir = r'F:\01_project\other\张一七\20240701\00.data\汇总'
    file_list = [os.path.join(file_dir,i) for i in os.listdir(file_dir)]
    df_list = [pd.read_csv(i, skiprows=4) for i in file_list]
    data = pd.concat(df_list,axis=0).dropna(axis=1, how='all').reset_index(drop=True)
    ana_data = data[data['Country Name'].isin(country_map.values())]
    ana_data = ana_data.loc[:,out_name]
    # print()
    long_data = pd.melt(ana_data, id_vars=['Country Name', 'Indicator Name'], var_name='Year', value_name='Value')
    long_data.to_csv('all.xls',sep='\t',index=False)
    print(ana_data.columns)

