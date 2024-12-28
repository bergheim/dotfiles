#!/bin/bash

# TODO: add metalerts. see https://api.met.no/weatherapi/metalerts/1.1/documentation
FORECAST_URL="https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=${LAT}&lon=${LONG}"
FORECAST_FILE="$HOME/.config/polybar/forecast.json"

if test -e "$FORECAST_FILE"; then
    IF_MOD_DATE=$(date -u -r $FORECAST_FILE)
    IF_MOD_HEADER="'If-Modified-Since: $IF_MOD_DATE'"
else
    IF_MOD_HEADER=""
fi
curl --fail --silent --header "${IF_MOD_HEADER}" "${FORECAST_URL}" --output "${FORECAST_FILE}"

TEMPERATURE=$(jq ".properties.timeseries[0] | .data.instant.details.air_temperature" <$FORECAST_FILE)
INDICATOR=$(jq ".properties.timeseries[0] | .data.next_1_hours.summary.symbol_code" <$FORECAST_FILE)

TEMPERATURE=$(sed -e 's/^"//' -e 's/"$//' <<<"$TEMPERATURE")
INDICATOR=$(sed -e 's/^"//' -e 's/"$//' <<<"$INDICATOR")

declare -A ICON_MAP
# converted from yr-icons
ICON_MAP=(
    ["clearsky_day"]=$'\ue800'
    ["clearsky_night"]=$'\ue801'
    ["clearsky_polartwilight"]=$'\ue802'
    ["cloudy"]=$'\ue803'
    ["fair_day"]=$'\ue804'
    ["fair_night"]=$'\ue805'
    ["fair_polartwilight"]=$'\ue806'
    ["fog"]=$'\ue807'
    ["heavyrain"]=$'\ue808'
    ["heavyrainandthunder"]=$'\ue809'
    ["heavyrainshowers_day"]=$'\ue80a'
    ["heavyrainshowers_night"]=$'\ue80b'
    ["heavyrainshowers_polartwilight"]=$'\ue80c'
    ["heavyrainshowersandthunder_day"]=$'\ue80d'
    ["heavyrainshowersandthunder_night"]=$'\ue80e'
    ["heavyrainshowersandthunder_polartwilight"]=$'\ue80f'
    ["heavysleet"]=$'\ue810'
    ["heavysleetandthunder"]=$'\ue811'
    ["heavysleetshowers_day"]=$'\ue812'
    ["heavysleetshowers_night"]=$'\ue813'
    ["heavysleetshowers_polartwilight"]=$'\ue814'
    ["heavysleetshowersandthunder_day"]=$'\ue815'
    ["heavysleetshowersandthunder_night"]=$'\ue816'
    ["heavysleetshowersandthunder_polartwilight"]=$'\ue817'
    ["heavysnow"]=$'\ue818'
    ["heavysnowandthunder"]=$'\ue819'
    ["heavysnowshowers_day"]=$'\ue81a'
    ["heavysnowshowers_night"]=$'\ue81b'
    ["heavysnowshowers_polartwilight"]=$'\ue81c'
    ["heavysnowshowersandthunder_day"]=$'\ue81d'
    ["heavysnowshowersandthunder_night"]=$'\ue81e'
    ["heavysnowshowersandthunder_polartwilight"]=$'\ue81f'
    ["lightrain"]=$'\ue820'
    ["lightrainandthunder"]=$'\ue821'
    ["lightrainshowers_day"]=$'\ue822'
    ["lightrainshowers_night"]=$'\ue823'
    ["lightrainshowers_polartwilight"]=$'\ue824'
    ["lightrainshowersandthunder_day"]=$'\ue825'
    ["lightrainshowersandthunder_night"]=$'\ue826'
    ["lightrainshowersandthunder_polartwilight"]=$'\ue827'
    ["lightsleet"]=$'\ue828'
    ["lightsleetandthunder"]=$'\ue829'
    ["lightsleetshowers_day"]=$'\ue82a'
    ["lightsleetshowers_night"]=$'\ue82b'
    ["lightsleetshowers_polartwilight"]=$'\ue82c'
    ["lightsnow"]=$'\ue82d'
    ["lightsnowandthunder"]=$'\ue82e'
    ["lightsnowshowers_day"]=$'\ue82f'
    ["lightsnowshowers_night"]=$'\ue830'
    ["lightsnowshowers_polartwilight"]=$'\ue831'
    ["lightssleetshowersandthunder_day"]=$'\ue832'
    ["lightssleetshowersandthunder_night"]=$'\ue833'
    ["lightssleetshowersandthunder_polartwilight"]=$'\ue834'
    ["lightssnowshowersandthunder_day"]=$'\ue835'
    ["lightssnowshowersandthunder_night"]=$'\ue836'
    ["lightssnowshowersandthunder_polartwilight"]=$'\ue837'
    ["partlycloudy_day"]=$'\ue838'
    ["partlycloudy_night"]=$'\ue839'
    ["partlycloudy_polartwilight"]=$'\ue83a'
    ["rain"]=$'\ue83b'
    ["rainandthunder"]=$'\ue83c'
    ["rainshowers_day"]=$'\ue83d'
    ["rainshowers_night"]=$'\ue83e'
    ["rainshowers_polartwilight"]=$'\ue83f'
    ["rainshowersandthunder_day"]=$'\ue840'
    ["rainshowersandthunder_night"]=$'\ue841'
    ["rainshowersandthunder_polartwilight"]=$'\ue842'
    ["sleet"]=$'\ue843'
    ["sleetandthunder"]=$'\ue844'
    ["sleetshowers_day"]=$'\ue845'
    ["sleetshowers_night"]=$'\ue846'
    ["sleetshowers_polartwilight"]=$'\ue847'
    ["sleetshowersandthunder_day"]=$'\ue848'
    ["sleetshowersandthunder_night"]=$'\ue849'
    ["sleetshowersandthunder_polartwilight"]=$'\ue84a'
    ["snow"]=$'\ue84b'
    ["snowandthunder"]=$'\ue84c'
    ["snowshowers_day"]=$'\ue84d'
    ["snowshowers_night"]=$'\ue84e'
    ["snowshowers_polartwilight"]=$'\ue84f'
    ["snowshowersandthunder_day"]=$'\ue850'
    ["snowshowersandthunder_night"]=$'\ue851'
    ["snowshowersandthunder_polartwilight"]=$'\ue852'
)

TEXT=$(sed -e 's/^"//' -e 's/"$//' <<<"$TEXT")

if [[ -v ICON_MAP[$INDICATOR] ]]; then
    echo -e "${ICON_MAP[$INDICATOR]} ${INDICATOR//_/ } $TEMPERATURE℃"
else
    echo -e "${INDICATOR//_/ } $TEMPERATURE℃"
fi
