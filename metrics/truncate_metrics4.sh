# use this script to delete all records from the metrics tables

DB_FILE=Metrics.sqlite

echo $DB_FILE 'delete from appendFullBlock4;'
sqlite3 $DB_FILE 'delete from appendFullBlock4;'

echo $DB_FILE 'delete from applyTransactions4;'
sqlite3 $DB_FILE 'delete from applyTransactions4;'

echo $DB_FILE 'delete from createUtxoState4;'
sqlite3 $DB_FILE 'delete from createUtxoState4;'

echo $DB_FILE 'delete from validateTxStateful4;'
sqlite3 $DB_FILE 'delete from validateTxStateful4;'

echo $DB_FILE 'delete from verifyScript4;'
sqlite3 $DB_FILE 'delete from verifyScript4;'
