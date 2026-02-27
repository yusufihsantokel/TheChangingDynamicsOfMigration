# _author_ = 'sixueyang'
# _data_ = 2024/7/4 20:12
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


def trun_data(file):
    data = pd.read_csv(file, encoding='utf-8')
    # print(data)
    sort_data = defaultdict(list)
    con_group = data.groupby(['Fiscal Year', 'Citizenship'])
    for years, years_else in con_group:
        year, count = years
        for i in country_map.keys():
            if i in count or count in i:
                count = country_map[i]
                break
        sort_data['Year'].append(year.split(' ')[0])
        sort_data['Country'].append(count)
        sort_data['Number'].append(years_else['Encounter Count'].sum())
    df = pd.DataFrame(sort_data)
    return df


if __name__ == '__main__':
    people_data = r'F:\01_project\other\张一七\20240701\cbp_nationwide-encounters-fy21-fy24-may-aor.csv'
    feature_data = r'F:\01_project\other\张一七\20240701\00.data\all.xls'
    people_df = trun_data(people_data)
    feature_data = pd.read_csv(feature_data,sep='\t')
    feature_data.columns = ['Country','Indicator','Year','Value']
    people_df['Year'] = people_df['Year'].astype(int)
    feature_data['Year'] = feature_data['Year'].astype(int)
    merged_df = pd.merge(people_df, feature_data, on=['Country','Year'], how='left').dropna()
    merged_df.to_csv('merge.csv',index=False)
    # print(people_df.columns)
    # print(merged_df.columns)


