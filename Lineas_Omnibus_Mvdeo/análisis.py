from pathlib import Path
import webbrowser
import os
import sys
import pandas as pd
from ydata_profiling import ProfileReport

# --- Carga de datos
csv_path = r"C:\Users\alexi\OneDrive - Universidad Católica del Uruguay\Desktop\estad-stica_inferencial\Lineas_Omnibus_Mvdeo\horarios_metropolitanos_dnt.csv"
df = pd.read_csv(csv_path, sep=';', encoding='latin-1')
df.columns = df.columns.str.strip()

# --- Carpeta de salida junto al script
BASE_DIR = Path(__file__).parent if "__file__" in globals() else Path.cwd()
out_dir = BASE_DIR / "outputs"
out_dir.mkdir(exist_ok=True)
out_file = out_dir / "reporte_analisis_exploratorio.html"

# --- Generar reporte
profile = ProfileReport(df, title="Reporte de Análisis Exploratorio de Datos", explorative=True)
profile.to_file(out_file)

# --- Verificación y apertura
if out_file.exists():
    print(f"Reporte generado en: {out_file.resolve()}")
    try:
        # En Windows esto suele ser lo más directo
        os.startfile(out_file)          # type: ignore[attr-defined]
    except Exception:
        webbrowser.open_new_tab(out_file.resolve().as_uri())
else:
    print("⚠️ No se encontró el HTML en la ruta esperada.")
    print("Ruta esperada:", out_file.resolve())
    sys.exit(1)


# --- Notas
# Pegar la ruta en el navegador si no se abre automáticamente.
# Si el archivo no se genera, revisar permisos de escritura en la carpeta.
# Revisar que el CSV se haya leído correctamente (encoding, separador, etc.).
