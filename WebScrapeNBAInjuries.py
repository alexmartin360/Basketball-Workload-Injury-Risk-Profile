from nbainjuries import injury
from datetime import datetime

json_output = injury.get_reportdata(datetime(year=2025, month=4, day=25, hour=17, minute=30)) 
df_output = injury.get_reportdata(datetime(year=2025, month=4, day=25, hour=17, minute=30), return_df=True)

print(df_output)