"""
Fetch monthly exchange rates from WRDS (comp.g_exrt_mth) for all
currencies in unique_curcd.csv and convert them to USD.

Output: cleaned_data/exchange_rates_to_usd.csv
  Columns: datadate, {CCY}_USD for each non-USD currency
"""

import os
import pandas as pd
import wrds
from dotenv import load_dotenv

load_dotenv()

DROPBOX_PATH = os.getenv("DATA_FOLDER")

###################
# Connect to WRDS #
###################
conn = wrds.Connection()

###################################
# Load currency list              #
###################################
curcds = pd.read_csv(f"{DROPBOX_PATH}/raw_data/compustat/unique_curcd.csv")["curcd"]
print(f"Currencies to fetch: {curcds.tolist()}")

###################################
# Fetch exchange rates            #
###################################
def fetch_exchange_rate(conn, _from: str, _to: str = "USD") -> pd.DataFrame:
    """
    Return a monthly time series of _from/_to exchange rates.

    comp.g_exrt_mth stores each currency's rate relative to GBP
    (fromcurm = GBP, tocurm = target currency).
    To get CCY/USD we compute: (CCY/GBP) / (USD/GBP).
    """
    def query_vs_gbp(currency: str) -> pd.DataFrame:
        df = conn.raw_sql(
            f"""
            SELECT tocurm, fromcurm, datadate, exratm
            FROM comp.g_exrt_mth
            WHERE tocurm = '{currency}'
            """,
            date_cols=["datadate"],
        )
        return df.rename(columns={"exratm": f"{currency}_gbp"})

    from_gbp = query_vs_gbp(_from)
    to_gbp   = query_vs_gbp(_to)

    merged = pd.merge(
        from_gbp[["datadate", f"{_from}_gbp"]],
        to_gbp[["datadate", f"{_to}_gbp"]],
        on="datadate",
        how="inner",
    )
    merged[f"{_from}_{_to}"] = merged[f"{_from}_gbp"] / merged[f"{_to}_gbp"]
    return merged[["datadate", f"{_from}_{_to}"]]


exchange_rates_df = pd.DataFrame()

for curcd in curcds:
    if curcd == "USD":
        continue
    rate = fetch_exchange_rate(conn, _from=curcd, _to="USD")
    if exchange_rates_df.empty:
        exchange_rates_df = rate
    else:
        exchange_rates_df = pd.merge(exchange_rates_df, rate, on="datadate", how="outer")

conn.close()

###################################
# Save                            #
###################################
out_path = f"{DROPBOX_PATH}/cleaned_data/exchange_rates_to_usd.csv"
exchange_rates_df.to_csv(out_path, index=False)
print(f"Saved {len(exchange_rates_df)} rows × {len(exchange_rates_df.columns)} columns -> {out_path}")
