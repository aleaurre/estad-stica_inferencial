# Analisis inicial del dataset de horarios de lineas de omnibus en Montevideo
# Lectura del Dataset

import argparse
import os
import re
import sys
import pandas as pd
from ydata_profiling import ProfileReport, load as load_profiler

"""
Uso:
  python perfilar.py --csv datos.csv --meta metadatos.txt --out reporte.html

El TXT de metadatos puede ser en UNO de estos formatos:

A) INI-like (recomendado):
    [edad]
    dtype = int
    description = Edad del usuario
    missing = NA, -1
    categories =        # (opcional, para variables categóricas)
    datetime_format =   # (opcional, p.ej. %Y-%m-%d)

    [sexo]
    dtype = category
    description = Sexo auto-reportado
    categories = M,F

B) Delimitado (cabecera requerida). Separador ; o , o tab:
    column;dtype;description;missing;categories;datetime_format
    edad;int;Edad del usuario;NA|-1;; 
    sexo;category;Sexo; ;M|F; 
"""

# ---------- parseo de metadatos ----------
def _clean(s):
    return None if s is None else str(s).strip()

def _split_items(s):
    s = _clean(s)
    if not s:
        return []
    # Permite separar por coma o | (p.ej. "NA, -1" o "M|F")
    parts = re.split(r"[|,]", s)
    return [p.strip() for p in parts if p.strip() != ""]

def parse_metadata_ini(path):
    # Parser INI minimalista sin depender de configparser (para mantenerlo portable)
    meta = {}
    current = None
    with open(path, "r", encoding="utf-8") as f:
        for raw in f:
            line = raw.strip()
            if not line or line.startswith("#") or line.startswith(";"):
                continue
            if line.startswith("[") and line.endswith("]"):
                current = line[1:-1].strip()
                meta[current] = {}
            elif "=" in line and current is not None:
                k, v = line.split("=", 1)
                meta[current][_clean(k)] = _clean(v)
            else:
                # línea desconocida -> ignorar
                pass

    # Normalizar campos
    normalized = {}
    for col, info in meta.items():
        normalized[col] = {
            "dtype": _clean(info.get("dtype")),
            "description": _clean(info.get("description")),
            "missing": _split_items(info.get("missing")),
            "categories": _split_items(info.get("categories")),
            "datetime_format": _clean(info.get("datetime_format")),
        }
    return normalized

def parse_metadata_table(path):
    import csv
    # Detectar delimitador ; , o \t
    with open(path, "r", encoding="utf-8") as f:
        sample = f.read(4096)
    delim = ";" if ";" in sample else ("\t" if "\t" in sample else ",")

    fields = ["column","dtype","description","missing","categories","datetime_format"]
    meta = {}
    with open(path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f, delimiter=delim)
        # validar cabeceras mínimas
        lower = [c.strip().lower() for c in reader.fieldnames or []]
        for req in ("column","dtype"):
            if req not in lower:
                raise ValueError(f"Falta la columna '{req}' en el metadata TXT.")
        for row in reader:
            row_norm = { (k or "").strip().lower(): _clean(v) for k,v in row.items() }
            col = row_norm.get("column")
            if not col: 
                continue
            meta[col] = {
                "dtype": row_norm.get("dtype"),
                "description": row_norm.get("description"),
                "missing": _split_items(row_norm.get("missing")),
                "categories": _split_items(row_norm.get("categories")),
                "datetime_format": row_norm.get("datetime_format"),
            }
    return meta

def parse_metadata(path):
    # Heurística: si hay líneas [seccion] -> INI; si no, asumimos tabla
    with open(path, "r", encoding="utf-8") as f:
        txt = f.read()
    if re.search(r"^\s*\[.+?\]\s*$", txt, flags=re.M):
        return parse_metadata_ini(path)
    else:
        return parse_metadata_table(path)

# ---------- mapeo de dtypes ----------
_PANDAS_DTYPES = {
    "int": "Int64",       # enteros con NA seguros
    "integer": "Int64",
    "float": "float64",
    "double": "float64",
    "str": "string",
    "string": "string",
    "category": "category",
    "bool": "boolean",    # boolean con NA
    "datetime": "datetime64[ns]",  # se parsea aparte si hay formato
    "date": "datetime64[ns]",
}

def build_pandas_dtype_map(meta):
    dtypes = {}
    datetime_cols = {}
    categories = {}
    na_values = set()

    for col, info in meta.items():
        dtype = (info.get("dtype") or "").lower()
        if dtype in ("datetime", "date"):
            datetime_cols[col] = info.get("datetime_format")
        elif dtype:
            dtypes[col] = _PANDAS_DTYPES.get(dtype, "string")

        if info.get("categories"):
            categories[col] = info["categories"]

        for mv in info.get("missing") or []:
            na_values.add(mv)

    return dtypes, datetime_cols, categories, (na_values if na_values else None)

# ---------- aplicar metadatos ----------
def apply_metadata(df, dtypes, datetime_cols, categories):
    # categorías primero (si existen)
    for col, cats in categories.items():
        if col in df.columns:
            df[col] = pd.Categorical(df[col], categories=cats)

    # castear tipos explícitos (excepto datetimes)
    for col, dt in dtypes.items():
        if col in df.columns and not (dt.startswith("datetime")):
            try:
                if dt == "Int64":
                    df[col] = pd.to_numeric(df[col], errors="coerce").astype("Int64")
                elif dt == "float64":
                    df[col] = pd.to_numeric(df[col], errors="coerce").astype("float64")
                elif dt == "boolean":
                    # Normalizar booleanos comunes
                    df[col] = (df[col]
                               .astype("string")
                               .str.strip()
                               .str.lower()
                               .replace({"true":"true","false":"false","1":"true","0":"false","si":"true","no":"false","sí":"true"}))
                    df[col] = df[col].map({"true": True, "false": False})
                elif dt == "string":
                    df[col] = df[col].astype("string")
                elif dt == "category":
                    df[col] = df[col].astype("category")
                else:
                    df[col] = df[col].astype(dt)
            except Exception:
                # fallback suave
                df[col] = pd.to_numeric(df[col], errors="ignore")

    # parsear fechas
    for col, fmt in datetime_cols.items():
        if col in df.columns:
            if fmt:
                df[col] = pd.to_datetime(df[col], format=fmt, errors="coerce")
            else:
                df[col] = pd.to_datetime(df[col], errors="coerce")

    return df

# ---------- main ----------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", required=True, help="Ruta al CSV de datos.")
    ap.add_argument("--meta", required=True, help="Ruta al TXT de metadatos.")
    ap.add_argument("--out", default="reporte.html", help="Ruta de salida del HTML.")
    ap.add_argument("--sep", default=None, help="Separador CSV (auto si no se provee).")
    ap.add_argument("--encoding", default="utf-8", help="Encoding del CSV.")
    ap.add_argument("--decimal", default=None, help="Separador decimal (p.ej. ',').")
    ap.add_argument("--low_memory", action="store_true", help="Pasar low_memory=True a read_csv.")
    args = ap.parse_args()

    if not os.path.exists(args.csv):
        sys.stderr.write(f"No existe el CSV: {args.csv}\n")
        sys.exit(1)
    if not os.path.exists(args.meta):
        sys.stderr.write(f"No existe el TXT de metadatos: {args.meta}\n")
        sys.exit(1)

    meta = parse_metadata(args.meta)
    dtypes, dt_cols, cats, na_values = build_pandas_dtype_map(meta)

    # Intento de autodetección de separador si no se indicó
    sep = args.sep
    if sep is None:
        with open(args.csv, "r", encoding=args.encoding, errors="ignore") as f:
            sample = f.read(4096)
        if sample.count(";") > sample.count(",") and sample.count(";") > sample.count("\t"):
            sep = ";"
        elif sample.count("\t") > sample.count(","):
            sep = "\t"
        else:
            sep = ","

    df = pd.read_csv(
        args.csv,
        sep=sep,
        encoding=args.encoding,
        dtype=None,             # dejamos que pandas lea crudo; casteamos luego
        na_values=na_values,
        decimal=args.decimal,
        low_memory=args.low_memory,
    )

    # aplicar metadatos
    df = apply_metadata(df, dtypes, dt_cols, cats)

    # Generar reporte
    ProfileReport = load_profiler()
    profile = ProfileReport(
        df,
        title="Data Profiling Report",
        explorative=True,
        correlations={"pearson": True, "spearman": True, "phi_k": True, "cramers": True},
        dataset={
            "description": "Perfil automático generado a partir de CSV + metadatos.",
            "copyright_holder": "",
            "creator": "perfilar.py",
        },
    )
    profile.to_file(args.out)
    print(f"✅ Reporte generado en: {args.out}")

if __name__ == "__main__":
    main()
