import urllib.request
from bs4 import BeautifulSoup

try:
    url = "https://www.iqair.com/us/usa/california/albany"
    html = urllib.request.urlopen(url).read()
    soup__object = BeautifulSoup(html, 'html.parser')
    aqi_val = soup__object.find(
        "p", {"class": "aqi-value__value"}).get_text().strip()
    aqi_desc = soup__object.find(
        "span", {"class": "aqi-status__text"}).get_text().strip()

    print("AQI: " + str(aqi_val) + " (" + aqi_desc + ")")

except urllib.error.URLError:
    print("-")
