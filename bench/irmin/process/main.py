import json
import pandas as pd

data_fd = open('_build/default/bench/irmin/data/latest.json', 'r')
data = json.load(data_fd)['results']
data_fd.close()

df = pd.json_normalize(data)
print(df)
