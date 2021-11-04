import urllib.request
from bs4 import BeautifulSoup
import numpy as np

url = "https://weather.com/weather/hourbyhour/l/" + \
    "ae92ba08dec71ff144fa5ee2552626ea71a4d097c7e91bd7b0efc8a6ba553495"

try:
    page = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    html = urllib.request.urlopen(page).read()

    soup__object = BeautifulSoup(html, 'html.parser')
    table = soup__object.find(
        "div", {"class": "HourlyForecast--DisclosureList--3CdxR"})

    # weather description
    descriptions = table.find_all(
        'span', {'class': "DetailsSummary--extendedData--365A_"})
    descriptions = [d.get_text() for d in descriptions]
    output_description = descriptions[0]  # coming hour

    # temperature
    temps = table.find_all(
        'span', {'class': "DetailsSummary--tempValue--1K4ka"})
    temps = [t.get_text() for t in temps]
    output_temp = temps[0]  # coming hour

    # chance of rain + warning
    rain_chance = table.find_all(
        'div', {'data-testid': "Precip"})
    rain_chance = [
        rc.find("span", {'data-testid': 'PercentageValue'}).get_text()
        for rc in rain_chance]

    msg = output_description + ' ' + output_temp

    rain_chance_num = np.array([float(r.replace('%', ''))/100.
                                for r in rain_chance])

    output_chance = np.max(rain_chance_num) * 100

    if output_chance > 20.:
        msg += ' <fc=#169490>rain coming</fc>'

    # heavy wind & warning
    wind_speed = table.find_all(
        'span', {'data-testid': 'Wind'})
    wind_speed = [float(w.get_text().split(' ')[1]) for w in wind_speed]

    max_speed = np.max(wind_speed)

    if max_speed > 18:
        msg += ' <fc=#169490>heavy wind coming</fc>'

    print(msg)

except:

    print('weather error :(')
