DB_FILE=Metrics.sqlite
METRICS_DIR=mainnet/data/metrics

echo loading selected_inputs

sqlite3 -separator ';' $DB_FILE \
   '.import '$METRICS_DIR'/selected_inputs.csv selectedInputs'
