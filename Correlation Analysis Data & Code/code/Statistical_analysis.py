
import statsmodels.api as sm
from statsmodels.formula.api import ols
import pandas as pd
from scipy.stats import spearmanr
from collections import defaultdict



if __name__ == '__main__':
    data_file = r'merge.csv'
    data2_file = r'merge_2.csv'
    data = pd.read_csv(data_file)
    f_group = data.groupby('Indicator')
    with open('相关性分析.xls','w') as sp_obj:
        head = ['Indicator','相关性系数','p']
        print('\t'.join(head),file=sp_obj)
        for e,eelse in f_group:
            correlation, p_value = spearmanr(eelse['Number'], eelse['Value'])
            out_content = [e,correlation, p_value]
            print('\t'.join(map(str,out_content)),file=sp_obj)
        print(data.columns)


    # # 多因素方差分析
    data2_df = pd.read_csv(data2_file)
    print(data2_df.columns)

    dependent_vars = list(data2_df.columns)[2]
    independent_vars = list(data2_df.columns)[3:]

    X = data2_df[independent_vars]
    X = sm.add_constant(X)  # 添加常数项

    # 创建因变量矩阵
    Y = data2_df[dependent_vars]

    # 使用OLS进行多元回归分析
    model = sm.OLS(Y, X).fit()

    # 打印回归分析结果
    print(model.summary())
    # sort_data = defaultdict(list)
    # f_group = data.groupby('Indicator')
    # for e, eelse in f_group:
    #     print(eelse)

