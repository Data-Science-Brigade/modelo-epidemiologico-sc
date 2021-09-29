# imports
import numpy as np
import pandas as pd
import sys

# date in format "year-month-day"
date = sys.argv[1]
lim_date = pd.to_datetime(date)
lim_date_str = str(lim_date).split()[0].replace('-','_')

# read data
df_raw = pd.read_csv('boavista_covid_dados_abertos.csv', sep=';', error_bad_lines=False)


# deaths and cases =====================================================
cols = ['codigo_ibge_municipio', 'regional_saude', 'municipio', 'obito',
        'data_coleta', 'data_publicacao']
df = df_raw[cols]

df.loc[:,'data_publicacao'] = pd.to_datetime(df['data_publicacao'], errors='coerce').dt.date
df.loc[:,'data_ocorrencia'] = pd.to_datetime(df['data_coleta'], errors='coerce').dt.date

df = df[df.data_publicacao.notnull()]
df = df[df.data_ocorrencia.notnull()].query('data_ocorrencia <= @lim_date')

df['casos'] = [1 if x == 'NAO' else 0 for x in df.obito.values]
df['obitos'] = [0 if x == 'NAO' else 1 for x in df.obito.values]


cols = ['regional_saude','codigo_ibge_municipio', 'municipio',
       'data_ocorrencia', 'casos', 'obitos']
cols_names = ['nom_regional', 'cod_municipio_ibge', 'nom_municipio', 
              'data_ocorrencia', 'casos', 'obitos']
df2 = df[cols].copy()
df2.columns = cols_names

df_final = df2.groupby(['nom_regional', 'cod_municipio_ibge', 'nom_municipio', 'data_ocorrencia'])        ['casos', 'obitos'].sum().reset_index()


# save deaths
df_final.to_csv(lim_date_str + "_compilado.csv", index=False)

# onset to death =================================================
cols = ['data_inicio_sintomas', 'data_coleta', 'data_obito']
df_od = df_raw[cols].copy()

df_od.loc[:,'data_inicio_sintomas'] = pd.to_datetime(df_od['data_inicio_sintomas'], errors='coerce').dt.date
df_od.loc[:,'data_coleta'] = pd.to_datetime(df_od['data_coleta'], errors='coerce').dt.date
df_od.loc[:,'data_obito'] = pd.to_datetime(df_od['data_obito'], errors='coerce').dt.date
df_od = df_od[df_od.data_obito.notnull()].query('data_obito <= @lim_date')

df_od['data_sintomas_ou_hospital'] = df_od[['data_inicio_sintomas','data_coleta']].bfill(axis=1).iloc[:, 0]

df_od_final = df_od[['data_sintomas_ou_hospital', 'data_obito']]
df_od_final.columns = ['dat_sintomas_ou_hospital', 'dat_obito']
df_od_final.loc[:,'data_onset_death'] = (df_od_final.dat_obito- df_od_final.dat_sintomas_ou_hospital).dt.days

# save onset to death
df_od_final.to_csv(lim_date_str + "_onset_to_death.csv", index=False)