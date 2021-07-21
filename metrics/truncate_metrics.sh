# use this script to delete all records from the metrics tables

DB_FILE=Metrics.sqlite

echo $DB_FILE 'delete from appendFullBlock;'
sqlite3 $DB_FILE 'delete from appendFullBlock;'

echo $DB_FILE 'delete from applyTransactions;'
sqlite3 $DB_FILE 'delete from applyTransactions;'

echo $DB_FILE 'delete from createUtxoState;'
sqlite3 $DB_FILE 'delete from createUtxoState;'

echo $DB_FILE 'delete from validateTxStateful;'
sqlite3 $DB_FILE 'delete from validateTxStateful;'

echo $DB_FILE 'delete from verifyScript;'
sqlite3 $DB_FILE 'delete from verifyScript;'
