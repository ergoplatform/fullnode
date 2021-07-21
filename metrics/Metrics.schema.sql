CREATE TABLE IF NOT EXISTS "appendFullBlock4"
(
    blockId varchar not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    cost decimal,
    time decimal
);
CREATE TABLE IF NOT EXISTS "createUtxoState4"
(
    blockId varchar not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    cost decimal,
    time decimal
);
CREATE TABLE IF NOT EXISTS "verifyScript4"
(
    blockId varchar not null,
    txId varchar not null,
    boxIndex int,
    cost decimal,
    time decimal
);
CREATE UNIQUE INDEX appendFullBlock4_id_uindex
    on appendFullBlock4 (blockId);
CREATE UNIQUE INDEX createUtxoState4_id_uindex
    on createUtxoState4 (blockId);
CREATE TABLE verifyScript
(
    blockId varchar not null,
    txId varchar not null,
    boxIndex int,
    cost decimal,
    time decimal
);
CREATE INDEX appendFullBlock4_height_index
    on appendFullBlock4 (height);
CREATE INDEX createUtxoState4_height_index
    on createUtxoState4 (height);
CREATE INDEX verifyScript_blockId_txId_index
    on verifyScript (blockId, txId);
CREATE INDEX verifyScript4_blockId_txId_index
    on verifyScript4 (blockId, txId);
CREATE INDEX verifyScript_blockId_index
    on verifyScript (blockId);
CREATE INDEX verifyScript4_blockId_index
    on verifyScript4 (blockId);
CREATE TABLE IF NOT EXISTS "selectedInputs"
(
    txId varchar not null,
    boxIndex int not null
);
CREATE INDEX selectedInputs_txId_inputIndex_index
    on selectedInputs (txId, boxIndex);
CREATE TABLE IF NOT EXISTS "validateTxStateful"
(
    blockId varchar(64) not null,
    txId varchar not null,
    cost decimal,
    time decimal,
    constraint validateTxStateful_pk
        primary key (blockId, txId)
);
CREATE INDEX validateTxStateful_blockId_index
    on validateTxStateful (blockId);
CREATE INDEX validateTxStateful_txId_index
    on validateTxStateful (txId);
CREATE TABLE IF NOT EXISTS "validateTxStateful4"
(
    blockId varchar(64) not null,
    txId varchar not null,
    cost decimal,
    time decimal,
    constraint validateTxStateful_pk
        primary key (blockId, txId)
);
CREATE INDEX validateTxStateful4_blockId_index
    on validateTxStateful4 (blockId);
CREATE TABLE IF NOT EXISTS "applyTransactions4"
(
    blockId varchar(64) not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    cost decimal,
    time decimal
);
CREATE INDEX blocks4_height_uindex
    on applyTransactions4 (height);
CREATE UNIQUE INDEX blocks4_id_uindex
    on applyTransactions4 (blockId);
CREATE INDEX verifyScript_blockId_txId_boxIndex_index
    on verifyScript (blockId, txId, boxIndex);
CREATE INDEX verifyScript4_blockId_txId_boxIndex_index
    on verifyScript4 (blockId, txId, boxIndex);
CREATE TABLE IF NOT EXISTS "appendFullBlock"
(
    blockId varchar not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    maxCost decimal,
    cost decimal,
    time decimal
);
CREATE INDEX appendFullBlock_height_index
    on appendFullBlock (height);
CREATE UNIQUE INDEX appendFullBlock_id_uindex
    on appendFullBlock (blockId);
CREATE TABLE IF NOT EXISTS "applyTransactions"
(
    blockId varchar(64) not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    maxCost decimal,
    cost decimal,
    time decimal
);
CREATE INDEX blocks_height_uindex
    on applyTransactions (height);
CREATE UNIQUE INDEX blocks_id_uindex
    on applyTransactions (blockId);
CREATE TABLE IF NOT EXISTS "createUtxoState"
(
    blockId varchar not null
        constraint blocks_pk
            primary key,
    height int not null,
    tx_num int,
    maxCost decimal,
    cost decimal,
    time decimal
);
CREATE INDEX createUtxoState_height_index
    on createUtxoState (height);
CREATE UNIQUE INDEX createUtxoState_id_uindex
    on createUtxoState (blockId);