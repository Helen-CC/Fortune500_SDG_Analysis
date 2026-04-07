"""
Pull firm characteristics (fundamentals annual) from Compustat
for companies in company_reference_master.xlsx.

Some gvkeys are only available in Compustat Global (comp.g_funda),
so we query both Compustat North America (comp.funda) and Global.
"""

import os
import pandas as pd
import wrds
from dotenv import load_dotenv

load_dotenv()

DROPBOX_PATH = os.getenv("DATA_FOLDER")
COMPANY_REF_PATH = f"{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"
OUT_DIR = f"{DROPBOX_PATH}/raw_data/compustat"
TODAY = pd.Timestamp.today().strftime("%Y-%m-%d")

###################
# Connect to WRDS #
###################
conn = wrds.Connection()

###########################
# Load gvkeys from master #
###########################
master = pd.read_excel(COMPANY_REF_PATH, sheet_name="master")
# Compustat stores gvkey as zero-padded 6-char varchar (e.g. '011259')
gvkeys = master["gvkey"].dropna().astype(int).unique().tolist()
gvkey_list = ", ".join(f"'{g:06d}'" for g in gvkeys)
print(f"Total unique gvkeys: {len(gvkeys)}")

# Variables to fetch
# at     : total assets
# sale   : sales/turnover (net) — may differ from revt for some firms
# revt   : total revenue
# emp    : employees (thousands)
# ni     : net income (nicon in g_funda)
# ib     : income before extraordinary items
# oiadp  : operating income after depreciation
# ceq    : common/ordinary equity total (book value)
# pstk   : preferred stock total
# prcc_f : fiscal-year-end closing price (NA only; absent in g_funda)
# csho   : common shares outstanding (csho in funda; cshoi in g_funda)
# dltt   : long-term debt total
# dlc    : debt in current liabilities
# fic    : country of incorporation (ISO code)
# costat : active/inactive company status
# (ROA = ni/at, ROS = ni/revt, Tobin's Q ≈ (prcc_f*csho + dltt + dlc) / at)

# comp.funda (North America)
VARS_US = """gvkey, datadate, fyear, conm, curcd, fic, costat,
             at, sale, revt, emp,
             ni, ib, oiadp,
             ceq, pstk, prcc_f, csho, dltt, dlc,
             naicsh AS naics, sich AS sic"""

# comp.g_funda (Global):
#   - net income = nicon (aliased as ni)
#   - shares outstanding = cshoi (aliased as csho)
#   - prcc_f does not exist → NULL placeholder
VARS_GLOBAL = """gvkey, datadate, fyear, conm, curcd, fic, costat,
                 at, sale, revt, emp,
                 nicon AS ni, ib, oiadp,
                 ceq, pstk, NULL::numeric AS prcc_f, cshoi AS csho, dltt, dlc,
                 naicsh AS naics, sich AS sic"""

######################################
# Query Compustat North America      #
# comp.funda                         #
######################################
print("\nQuerying Compustat North America (comp.funda)...")
funda_us = conn.raw_sql(
    f"""
    SELECT {VARS_US}
    FROM comp.funda
    WHERE gvkey IN ({gvkey_list})
      AND datafmt = 'STD'
      AND consol  = 'C'
      AND fyear  >= 2015
    ORDER BY gvkey, fyear
    """,
    date_cols=["datadate"],
)
print(f"  Rows fetched: {len(funda_us)}")
print(f"  Unique gvkeys: {funda_us['gvkey'].nunique()}")

######################################
# Query Compustat Global             #
# comp.g_funda                       #
######################################
print("\nQuerying Compustat Global (comp.g_funda)...")
funda_global = conn.raw_sql(
    f"""
    SELECT {VARS_GLOBAL}
    FROM comp.g_funda
    WHERE gvkey IN ({gvkey_list})
      AND datafmt = 'HIST_STD'
      AND consol  = 'C'
      AND fyear  >= 2015
    ORDER BY gvkey, fyear
    """,
    date_cols=["datadate"],
)
print(f"  Rows fetched: {len(funda_global)}")
print(f"  Unique gvkeys: {funda_global['gvkey'].nunique()}")

##############################################
# Derive computed metrics and save to CSV    #
##############################################
for label, df in [("us", funda_us), ("global", funda_global)]:
    df = df.copy()

    # Derived financial ratios
    df["roa"] = df["ni"] / df["at"]
    df["ros"] = df["ni"] / df["revt"]
    df["tobin_q"] = (df["prcc_f"] * df["csho"] + df["dltt"].fillna(0) + df["dlc"].fillna(0)) / df["at"]

    out_path = f"{OUT_DIR}/{TODAY}_comp_funda_{label}.csv"
    df.to_csv(out_path, index=False)
    print(f"\nSaved {label} data -> {out_path}")
    print(df.head(3))

conn.close()
print("\nDone.")
