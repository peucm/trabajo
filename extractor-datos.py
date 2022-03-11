#
# Script para extraer datos del servicio REST de carburantes
# de MINETUR (Ministerio de Industria, Comercio y Turismo)
#
import requests
import datetime
import json
from os.path import exists

PRICES_URL = "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestresHist/"
REGIONS_URL = "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/Listados/ComunidadesAutonomas/"
DATAPOINTS = [ "Precio Biodiesel", "Precio Bioetanol", "Precio Gas Natural Comprimido", "Precio Gas Natural Licuado",
               "Precio Gases licuados del petróleo", "Precio Gasoleo A", "Precio Gasoleo B", "Precio Gasoleo Premium", 
               "Precio Gasolina 95 E10", "Precio Gasolina 95 E5", "Precio Gasolina E5 Premium", "Precio Gasolina 98 E10", 
               "Precio Gasolina 98 E5", "Precio Hidrogeno"
             ]
MONTH_NAMES = [ "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre" ]

def query(url):
  """
  Realiza una petición a una URL dada
  y devuelve el resultado en JSON.
  """
  headers = { 'Content-Type': 'application/json' }
  response = requests.get(url = url, headers = headers)

  if response.status_code == 200:
    return response.json()

  raise RuntimeError("Respuesta de la petición a " + url + ": " + str(response.status_code) + "; " + response.text)

def calc_day_avg(region_names, date):
  """
  Calcula el precio medio para una fecha dada
  de todos los combustibles en toda España,
  agrupados por comunidad autónoma.
  """
  # Realizamos la petición de los datos. La estructura
  # de la respuesta puede visualizarse en esta dirección:
  # https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/help/operations/PreciosEESSTerrestresHist#response-json
  response = query(PRICES_URL + date.strftime("%d-%m-%Y"))

  if not "ListaEESSPrecio" in response:
    raise RuntimeError("Respuesta inválida de la API, no existe ListaEESSPrecio; " + response)

  # Diccionario con los diferentes precios de las estaciones de servicio,
  # agrupados por comunidad autónoma y tipo de combustible:
  # { 'CCAA1': { 'COMB1': 1.25, 'COMB2' : ... }, 'CCAA2': ... }
  dp_per_region = { }

  # Diccionario con el número de entradas de cada tipo de combustible,
  # agrupados por comunidad autónoma. Utilizado para calcular la media
  # de manera iterativa
  count_per_region = { }

  for station in response["ListaEESSPrecio"]:
    # Buscamos todos los combustibles que nos interesan
    for point in DATAPOINTS:
      if point in station:
        try:
          price = float(station[point].replace(',', '.'))
        except ValueError:
          # La API devuelve valores vacíos si no hay datos, continuamos
          continue

        reg_id = station["IDCCAA"]
        reg_name = region_names[reg_id]

        if not reg_name in dp_per_region:
          # Primera entrada de esta comunidad autónoma
          dp_per_region[reg_name] = { }
          count_per_region[reg_name] = { }

        datapoints = dp_per_region[reg_name]
        count_per_datapoint = count_per_region[reg_name]

        if point not in datapoints:
          # Primera entrada de este tipo de combustible
          datapoints[point] = 0
          count_per_datapoint[point] = 0

        # Calculamos la media de manera iterativa
        count = count_per_datapoint[point]
        prev_mean = datapoints[point]

        new_mean = (prev_mean * count + price) / (count + 1)

        count_per_datapoint[point] += 1
        datapoints[point] = new_mean

  return dp_per_region

def get_region_names():
  """
  Obtiene el diccionario de comunidades
  autónomas y sus respectivos identificadores
  utilizados por el servicio de MINETUR.
  """
  response = query(REGIONS_URL)
  region_names = { }

  for region in response:
    reg_id = region["IDCCAA"]
    reg_name = region["CCAA"]
    region_names[reg_id] = reg_name

  return region_names

def calc_period_avg(region_names, start_date, end_date):
  """
  Calcula la media del precio de los
  diferentes tipos de combustible en
  un periodo dado. Ambas fechas (start_date
  y end_date) son inclusivas.
  """
  delta = datetime.timedelta(days = 1)

  # Diccionario con los valores medios en el periodo dado para
  # cada tipo de combustible, agrupados por comunidad autónoma:
  # { 'CCAA1': { 'COMB1': 1.25, 'COMB2' : ... }, 'CCAA2': ... }
  avg_per_region = { }

  # Diccionario con el número de entradas en el periodo dado para
  # cada tipo de combustible, agrupados por comunidad autónoma
  count_per_region = { }

  while start_date <= end_date:
    print("Obteniendo datos de " + start_date.strftime("%d-%m-%Y") + "...")
    
    # Obtenemos los datos medios de ese día
    date_avg_per_reg = calc_day_avg(region_names, start_date)

    # Iteramos por cada una de las regiones de los datos
    for reg_name in date_avg_per_reg:
      datapoints = date_avg_per_reg[reg_name]

      if not reg_name in avg_per_region:
        # Primer dato de la comunidad autónoma
        avg_per_region[reg_name] = { }
        count_per_region[reg_name] = { }

      period_avg = avg_per_region[reg_name]
      period_count = count_per_region[reg_name]

      for point in datapoints:
        avg_value = datapoints[point]
        
        if not point in period_avg:
          # Primer dato de este tipo de combustible
          period_avg[point] = 0
          period_count[point] = 0

        # Calculamos la media iterativamente
        count = period_count[point]
        prev_mean = period_avg[point]
        new_mean = (prev_mean * count + avg_value) / (count + 1)

        period_count[point] += 1
        period_avg[point] = new_mean

    start_date += delta

  return avg_per_region


# Obtenemos el nombre de las distintas comunidades autónomas
region_names = get_region_names()

# Generamos un reporte para cada año desde 2011 hasta el presente
date_now = datetime.date.today()
for year in range(2011, date_now.year + 1):
  file_name = "avg_price_monthly_report_" + str(year) + ".json"

  if exists(file_name):
    print("Ignorando año " + str(year) + " porque ya hay un registro sobre él.")
  else:
    print("Generando un reporte del año " + str(year) + "...")
    result = { }
    i = 1
    
    # Si es el año actual, solo llegamos hasta el mes anterior
    if year == date_now.year:
      max = date_now.month - 1
    else:
      max = 12

    while i <= max:
      month = MONTH_NAMES[i - 1]
      print("========== " + month + " ==========")
      start_date = datetime.date(year = year, month = i, day = 1)
      delta = datetime.timedelta(days = 1)
      
      # Si es diciembre, la fecha siguiente es el año que viene
      if i == 12:
        end_date = datetime.date(year = year + 1, month = 1, day = 1) - delta
      else:
        end_date = datetime.date(year = year, month = i + 1, day = 1) - delta

      # Obtenemos la media del mes actual y la guardamos
      result[month] = calc_period_avg(region_names, start_date, end_date)
      i += 1

    # Guardamos el resultado en un archivo
    json_result = json.dumps(result, indent = 4, ensure_ascii = False)

    with open(file_name, "w", encoding = 'utf-8') as outfile:
        outfile.write(json_result)
