#!/usr/bin/python3
import lxml.etree
import requests


url = 'https://www.yr.no/place/Norway/Oslo/Oslo/Oslo/forecast_hour_by_hour.xml'

icons = {'02d': u'\uf00c', '42d': u'\uf0b2', '20n': u'\uf01d', '24d': u'\uf00e',
         '48': u'', '02n': u'\uf081', '22': u'\uf01d', '26n': u'\uf06a',
         '28d': u'\uf06b', '21n': u'\uf06d', '08n': u'\uf01b', '26d': u'\uf068',
         '01d': u'\uf00d', '08d': u'\uf01b', '29n': u'\uf06d', '04': u'\uf041',
         '23': u'\uf01d', '25n': u'\uf01e', '47': u'\uf0b5', '29d': u'\uf06b',
         '09': u'\uf01a', '27n': u'\uf01d', '46': u'\uf01c', '43d': u'\uf0b2',
         '50': u'\uf01b', '06n': u'\uf01d', '44n': u'\uf02a', '41d': u'\uf019',
         '05n': u'\uf029', '24n': u'\uf02c', '03n': u'\uf086', '05d': u'\uf009',
         '20d': u'\uf01d', '03d': u'\uf002', '42n': u'\uf0b4', '07d': u'\uf0b2',
         '40d': u'\uf00b', '25d': u'\uf01e', '01n': u'\uf02e', '45n': u'\uf01b',
         '07n': u'\uf0b4', '45d': u'\uf01b', '11': u'\uf01d', '10': u'\uf019',
         '13': u'\uf01b', '12': u'\uf0b5', '15': u'\uf014', '14': u'\uf01d',
         '28n': u'\uf06d', '33': u'\uf01d', '32': u'\uf01d', '31': u'\uf01d',
         '30': u'\uf01d', '44d': u'\uf00a', '21d': u'\uf06b', '34': u'\uf01d',
         '43n': u'\uf0b4', '40n': u'\uf02b', '49': u'\uf01b', '27d': u'\uf01d',
         '41n': u'\uf019', '06d': u'\uf01d'}

response = requests.get(url)
root = lxml.etree.fromstring(response.content)
new = root.xpath('forecast/tabular/time')[0]
symbol = icons[new.xpath('symbol')[0].attrib['var'].replace('mf/', '').split('.')[0]]
degrees = new.xpath('temperature')[0].attrib['value']
name = new.xpath('symbol')[0].attrib['name']

print(u'%{T5}' + symbol + u'%{T-}  %{T1}' + degrees + '° C%{T-}')
