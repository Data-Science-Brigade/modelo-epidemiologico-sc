import pandas as pd

df = pd.read_csv("deaths_unclean.csv")

# Get only the most common regions
g = df.groupby(["nom_municipio","nom_regional"]).count()
cities_regions = {c:("",0) for c in df["nom_municipio"].unique()}
for c,r in g.index:
  count = g.loc[(c,r)]["data_ocorrencia"]
  if cities_regions[c][1] < count:
    cities_regions[c] = (r,count)

print(cities_regions)
for c in cities_regions:
  df.loc[df["nom_municipio"]==c,"nom_regional"] = cities_regions[c][0]

# Replace 2002 with 2020 since it is a common typo
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2002-","2020-")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2022-","2020-")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2200-","2020-")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2202-","2020-")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2220-","2020-")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2222-","2020-")

df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2021-10","2020-10")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2021-11","2020-11")
df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2121-","2021-")

df.loc[df["data_ocorrencia"].str.startswith("201"),"data_ocorrencia"] = "2020-01-01"
df.loc[df["data_ocorrencia"].str.startswith("200"),"data_ocorrencia"] = "2020-01-01"
df.loc[df["data_ocorrencia"].str.startswith("199"),"data_ocorrencia"] = "2020-01-01"
df.loc[df["data_ocorrencia"].str.startswith("5"),"data_ocorrencia"] = "2020-01-01"


df.to_csv("deaths.csv")
