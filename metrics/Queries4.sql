---------------------------------------------------------
-- Validation Queries for v4.0

-- appendFullBlock4
select count(*)
from appendFullBlock4;
-- applyTransactions4
select count(*)
from applyTransactions4;
-- createUtxoState4
select count(*)
from createUtxoState4;
-- validateTxStateful4
select count(*)
from validateTxStateful4;
-- verifyScript4
select count(*)
from verifyScript4;

select *
from appendFullBlock4
where blockId = 'blockId';
select *
from applyTransactions4
where blockId = 'blockId';
select *
from createUtxoState4
where blockId = 'blockId';
select *
from validateTxStateful4
where blockId = 'blockId';
select *
from verifyScript4
where blockId = 'blockId';

-- delete from appendFullBlock4 where blockId = 'blockId';
-- delete from applyTransactions4 where blockId = 'blockId';
-- delete from createUtxoState4 where blockId = 'blockId';
-- delete from validateTxStateful4 where blockId = 'blockId';
-- delete from verifyScript4 where blockId = 'blockId';


-- suspicious costs
select b.blockId, b.tx_num, b.cost, sum(tx.cost) as sum_tx_costs
from applyTransactions4 as b
         join validateTxStateful4 tx on b.blockId = tx.blockId
where b.tx_num > 1
  and b.cost < 30000
group by b.blockId
limit 20;

-- tx with leading 000
select *
from validateTxStateful4
where txId like '000%';

-- tx with negative cost
select count(*)
from validateTxStateful4
where cost < 0;

-- tx by blockId
select *
from validateTxStateful4
where blockId = '1507f24b800e9d8dbed4842946717729e829ffba1a628d0e9df3b6be8f239147';

-- invalid tx_num
select b.blockId, t.tx_count as c, b.tx_num as n
from (select blockId, count(*) as tx_count
      from validateTxStateful4
      group by blockId) as t
         join applyTransactions4 as b on b.blockId = t.blockId
where c != n;

-- invalid tx cost
select b.blockId, t.sum_costs as sum_tx_costs, b.cost as block_cost
from (select blockId, sum(cost) as sum_costs
      from validateTxStateful4
      group by blockId) as t
         join applyTransactions4 as b on b.blockId = t.blockId
where sum_tx_costs != block_cost;

-- invalid script cost
select count(*)
from (select tx.blockId, tx.txId, script_count, t.sum_script_costs, tx.cost as tx_cost
      from (select blockId, txId, sum(cost) as sum_script_costs, count(*) as script_count
            from verifyScript4
            group by blockId, txId) as t
               join validateTxStateful4 as tx on tx.blockId = t.blockId and tx.txId = t.txId
      where sum_script_costs > tx_cost
      order by tx.blockId);

-- invalid script cost or time
select tx.blockId,
       tx.txId,
       t.script_count,
       t.sum_script_costs,
       tx.cost as tx_cost,
       t.sum_script_time,
       tx.time as tx_time
from (select blockId,
             txId,
             sum(cost) as sum_script_costs,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript4
      group by blockId, txId) as t
         join validateTxStateful4 as tx on tx.blockId = t.blockId and tx.txId = t.txId
where sum_script_costs > tx_cost
   or sum_script_time > tx_time
order by script_count desc;

-- v4.0: top 100 block costs vs total validation times
select b1.blockId,
       b1.height,
       b1.tx_num,
       b2.cost,
       b1.time / 1000                       as t1_us,
       b2.time / 1000                       as t2_us,
       b3.time / 1000                       as t3_us,
       (b1.time + b2.time + b3.time) / 1000 as time_us
from appendFullBlock4 as b1
         join applyTransactions4 as b2 on b1.blockId = b2.blockId
         join createUtxoState4 b3 on b1.blockId = b3.blockId
order by time_us desc
limit 100;

-- count blocks where script time is less than 50%
select count(*)
from (select b1.blockId,
             b1.height,
             b1.tx_num,
             b2.cost,
             b1.time / 1000                       as t1_us,
             b2.time / 1000                       as t2_us,
             b3.time / 1000                       as t3_us,
             (b1.time + b2.time + b3.time) / 1000 as time_us
      from appendFullBlock4 as b1
               join applyTransactions4 as b2 on b1.blockId = b2.blockId
               join createUtxoState4 b3 on b1.blockId = b3.blockId
      where t3_us >= t2_us
      order by b2.cost desc);


