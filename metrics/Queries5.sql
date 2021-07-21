---------------------------------------------------------
-- Validation Queries for v5.0

select * from
(select count(*) as appendFullBlock
from appendFullBlock),
(select count(*) as applyTransactions
from applyTransactions),
(select count(*) as createUtxoState
from createUtxoState),
(select count(*) as validateTxStateful
from validateTxStateful),
(select count(*) as verifyScript
from verifyScript)

-- The recoreded metrics files contain a header row. That row can be found using the following
-- queries.
select *
from appendFullBlock
where blockId = 'blockId';
select *
from applyTransactions
where blockId = 'blockId';
select *
from createUtxoState
where blockId = 'blockId';
select *
from validateTxStateful
where blockId = 'blockId';
select *
from verifyScript
where blockId = 'blockId';

-- Uncomment these commands to remove header rows after metrics are imported from csv files.
-- delete from appendFullBlock where blockId = 'blockId';
-- delete from applyTransactions where blockId = 'blockId';
-- delete from createUtxoState where blockId = 'blockId';
-- delete from validateTxStateful where blockId = 'blockId';
-- delete from verifyScript where blockId = 'blockId';


-- count tx with negative cost
select count(*)
from validateTxStateful
where cost < 0;

-- invalid tx_num
select b.blockId, t.tx_count as c, b.tx_num as n
from (select blockId, count(*) as tx_count
      from validateTxStateful
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
where c != n;

-- invalid tx cost
select b.blockId, t.sum_costs as sum_tx_costs, b.cost as block_cost
from (select blockId, sum(cost) as sum_costs
      from validateTxStateful
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
where sum_tx_costs != block_cost;

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
      from verifyScript
      group by blockId, txId) as t
         join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId
where sum_script_costs > tx_cost
   or sum_script_time > tx_time
order by script_count desc;

-- top tx_time vs script_time ratio
-- how much tx time larger than script verification time
select tx.blockId,
       tx.txId,
       t.script_count,
       tx.time / 1000                                   as tx_time_us,
       t.sum_script_time / 1000                         as script_time_us,
       (tx.time - t.sum_script_time) / 1000             as time_diff_us,
       round(tx.time * 10 / t.sum_script_time * 0.1, 1) as time_ratio
from (select blockId,
             txId,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript
      group by blockId, txId) as t
         join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId
order by time_ratio desc;

-- count tx by tx_time vs script_time ratio
-- same as above but the transactions are grouped by integer ratio
select t.time_ratio / 10,
       count(*),
       round(avg(t.tx_time_us), 1) as tx_time_us,
       round(avg(t.script_time_us), 1) as script_time_us,
       round(avg(t.script_count), 1) as script_count
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10 desc;

-- small ratio: count tx by tx_time vs script_time
select round(t.time_ratio * 0.1, 1),
       count(*),
       round(avg(t.tx_time_us), 1) as tx_time_us,
       round(avg(t.script_time_us), 1) as script_time_us,
       round(avg(t.script_count), 1) as script_count
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio desc;

-- Count transaction where script validation <= 80% of time
select count(*)
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
where t.time_ratio >= 12;


-- top tx block_time vs script_time ratio
select b.blockId,
       t.script_count,
       b.time / 1000                       as block_time_us,
       t.sum_script_time / 1000            as script_time_us,
       (b.time - t.sum_script_time) / 1000 as time_diff,
       b.time * 10 / t.sum_script_time     as time_ratio
from (select blockId,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
order by time_ratio desc;

-- count blocks by block_time vs script_time ratio
select t.time_ratio / 10 as time_ratio,
       count(*),
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.tx_time_us), 0) as avg_tx_time_us,
       round(avg(t.tx_count), 1)   as avg_tx_count
from (select b.blockId,
             tx.tx_count,
             b.time / 1000                        as block_time_us,
             tx.sum_tx_time / 1000            as tx_time_us,
             (b.time - tx.sum_tx_time) / 1000 as time_diff_us,
             b.time * 10 / tx.sum_tx_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_tx_time,
                   count(*)  as tx_count
            from validateTxStateful
            group by blockId) as tx
               join applyTransactions as b on b.blockId = tx.blockId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10;


-- count blocks by block_time vs script_time ratio
select t.time_ratio / 10 as time_ratio,
       count(*),
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.script_time_us), 0) as avg_script_time_us,
       round(avg(t.script_count), 1)   as avg_script_count
from (select b.blockId,
             t.script_count,
             b.time / 1000                       as block_time_us,
             t.sum_script_time / 1000            as script_time_us,
             (b.time - t.sum_script_time) / 1000 as time_diff,
             b.time * 10 / t.sum_script_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10 desc;

-- small ratio: count blocks by block_time vs script_time
select t.time_ratio,
       count(*),
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.script_time_us), 0) as avg_script_time_us,
       round(avg(t.script_count), 1)   as avg_script_count
from (select b.blockId,
             t.script_count,
             b.time / 1000                       as block_time_us,
             t.sum_script_time / 1000            as script_time_us,
             (b.time - t.sum_script_time) / 1000 as time_diff,
             b.time * 10 / t.sum_script_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio desc;

-----------------------------------------------------------
-- comparing stages of block validation
-- v5.0: top 1000 block costs vs total validation times
select b1.blockId,
       b1.height,
       b1.tx_num,
       b2.cost,
       b1.time / 1000                       as t1_us,
       b2.time / 1000                       as t2_us,
       b3.time / 1000                       as t3_us,
       (b1.time + b2.time + b3.time) / 1000 as time_us
from appendFullBlock as b1
         join applyTransactions as b2 on b1.blockId = b2.blockId
         join createUtxoState b3 on b1.blockId = b3.blockId
order by time_us desc
limit 1000;

-- group and count blocks by (total time / `applyTransactions` time) ratio
select time_us / t2_us as ratio, count(*)
from (select b1.blockId,
             b1.height,
             b1.tx_num,
             b2.cost,
             b1.time / 1000                       as t1_us,
             b2.time / 1000                       as t2_us,
             b3.time / 1000                       as t3_us,
             (b1.time + b2.time + b3.time) / 1000 as time_us
      from appendFullBlock as b1
               join applyTransactions as b2 on b1.blockId = b2.blockId
               join createUtxoState b3 on b1.blockId = b3.blockId)
group by ratio
order by ratio;

select round(time_us * 10 / t2_us * 0.1, 1) as ratio, count(*)
from (select b1.blockId,
             b1.height,
             b1.tx_num,
             b2.cost,
             b1.time / 1000                       as t1_us,
             b2.time / 1000                       as t2_us,
             b3.time / 1000                       as t3_us,
             (b1.time + b2.time + b3.time) / 1000 as time_us
      from appendFullBlock as b1
               join applyTransactions as b2 on b1.blockId = b2.blockId
               join createUtxoState b3 on b1.blockId = b3.blockId)
group by ratio
order by ratio desc;

select b1.blockId,
       b1.height,
       b1.tx_num,
       b2.cost,
       b1.time / 1000                       as t1_us,
       b2.time / 1000                       as t2_us,
       b3.time / 1000                       as t3_us,
       (b1.time + b2.time + b3.time) / 1000 as time_us
from appendFullBlock as b1
         join applyTransactions as b2 on b1.blockId = b2.blockId
         join createUtxoState b3 on b1.blockId = b3.blockId
where time_us * 0.3 >= t3_us
order by b2.cost desc;

-- new vs old
select at5.blockId, at5.time as t5, at4.time as t4
from applyTransactions as at5
         join applyTransactions4 as at4 on at5.blockId = at4.blockId
where at5.time > at4.time
