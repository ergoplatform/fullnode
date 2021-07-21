--------------------------------------
--   Cross version validation

-- validate appendFullBlock tables
select *
from appendFullBlock a1
         join appendFullBlock4 a2 on a1.blockId = a2.blockId
where a1.height != a2.height
   or a1.tx_num != a2.tx_num;

-- validate applyTransactions tables
select *
from applyTransactions a1
         join applyTransactions4 a2 on a1.blockId = a2.blockId
where a1.height != a2.height
   or a1.tx_num != a2.tx_num;

-- validate createUtxoState tables
select *
from createUtxoState a1
         join createUtxoState4 a2 on a1.blockId = a2.blockId
where a1.height != a2.height
   or a1.tx_num != a2.tx_num;

---------------------------------------
-- Cross Version reports

-- comment this count(*) line and parentheses to get records
select count(*) from (
-- tx costs and times by blocks
select t5.blockId,
       t5.height,
       t5.tx_num,

       t4.cost                            as cost4,
       t5.cost                            as cost5,
       round(t4.cost * 10 / t5.cost * 0.1, 1) as cost_ratio,

       t4.tx_time_us                      as time4_us,
       t5.tx_time_us                      as time5_us,
       round(t4.tx_time_us * 10 / t5.tx_time_us * 0.1, 1) as time_ratio
from (select b.blockId,
             b.height,
             b.tx_num,
             t.cost + b.tx_num * 9000 as cost,
             t.tx_time / 1000 as tx_time_us
      from (select blockId, sum(cost) as cost, sum(time) as tx_time
            from validateTxStateful
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t5

         join (select blockId,
                      cost,
                      tx_time / 1000 as tx_time_us
               from (select blockId, sum(cost) as cost, sum(time) as tx_time
                     from validateTxStateful4
                     group by blockId)) as t4 on t5.blockId = t4.blockId
where time_ratio < 1
order by time_ratio asc
    )
;

-- tx costs and times by blocks (compare)
select count(*) from (
select t5.blockId,
       t5.height,
       t5.tx_num,

       t4.cost                                            as cost4,
       t5.cost                                            as cost5,
       round(t4.cost * 10 / t5.cost * 0.1, 1)             as cost_ratio,

       t4.tx_time_us                                      as time4_us,
       t5.tx_time_us                                      as time5_us,
       round(t4.tx_time_us * 10 / t5.tx_time_us * 0.1, 1) as time_ratio,
       round(t4.cost / t4.tx_time_us * 0.1, 1)            as cost_time_ratio
from (select b.blockId,
             b.height,
             b.tx_num,
             (t.cost + b.tx_num * 9000) * 2 as cost,
             t.tx_time / 1000         as tx_time_us

      from (select blockId, sum(cost) as cost, sum(time) as tx_time
            from validateTxStateful
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t5

         join (select blockId,
                      cost,
                      tx_time / 1000 as tx_time_us
               from (select blockId, sum(cost) as cost, sum(time) as tx_time
                     from validateTxStateful4
                     group by blockId)) as t4 on t5.blockId = t4.blockId
-- where cost_ratio > time_ratio and time_ratio >= 1
--   and cost_time_ratio < 1
--where time_ratio >= 1 and time_ratio <= 1.1
order by cost4 desc
);

-- count blocks by tx_time speedup
select t.time_ratio / 10,
       count(*),
       avg(t.tx_num),
       avg(t.time4_us),
       avg(t.time5_us)
from (select t5.tx_num,
             t4.tx_time_us                      as time4_us,
             t5.tx_time_us                      as time5_us,
             t4.tx_time_us * 10 / t5.tx_time_us as time_ratio
      from (select b.blockId,
                   b.tx_num,
                   t.tx_time / 1000 as tx_time_us
            from (select blockId, sum(time) as tx_time
                  from validateTxStateful
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            tx_time / 1000 as tx_time_us
                     from (select blockId, sum(time) as tx_time
                           from validateTxStateful4
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10 asc;

-- small ratio: count blocks by tx_time speedup
select t.time_ratio,
       count(*),
       round(avg(t.tx_num), 1)   as avg_tx_num,
       round(avg(t.time4_us), 1) as avg_t4_us,
       round(avg(t.time5_us), 1) as avg_t5_us
from (select t5.tx_num,
             t4.tx_time_us                      as time4_us,
             t5.tx_time_us                      as time5_us,
             t4.tx_time_us * 10 / t5.tx_time_us as time_ratio
      from (select b.blockId,
                   b.tx_num,
                   b.cost           as cost,
                   t.tx_time / 1000 as tx_time_us
            from (select blockId, sum(time) as tx_time
                  from validateTxStateful
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            cost,
                            tx_time / 1000 as tx_time_us
                     from (select blockId, sum(cost) as cost, sum(time) as tx_time
                           from validateTxStateful4
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio asc;

-- v4 vs v5 script costs and times by blocks
select t5.blockId,
       t5.height,
       t5.tx_num,

       t4.script_cost                             as cost4,
       t5.script_cost                             as cost5,
       t4.script_cost * 10 / t5.script_cost       as cost_ratio,

       t4.script_time_us                          as time4_us,
       t5.script_time_us                          as time5_us,
       t4.script_time_us * 10 / t5.script_time_us as time_ratio
from (select b.blockId,
             b.height,
             b.tx_num,
             b.time / 1000        as block_time_us,
             t.script_cost,
             t.script_time / 1000 as script_time_us,
             t.script_count
      from (select blockId,
                   sum(cost) as script_cost,
                   sum(time) as script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t5

         join (select blockId,
                      script_cost,
                      script_time / 1000 as script_time_us
               from (select blockId,
                            sum(cost) as script_cost,
                            sum(time) as script_time
                     from verifyScript4
                     group by blockId)) as t4 on t5.blockId = t4.blockId
-- where time_ratio <= 10
order by time_ratio asc;

-- count blocks by script_time speedup
select t.time_ratio / 10,
       count(*),
       round(avg(t.script_count), 1) as avg_script_count,
       round(avg(t.time4_us), 1)     as avg_t4_us,
       round(avg(t.time5_us), 1)     as avg_t5_us
from (select t5.script_count,
             t4.script_time_us                          as time4_us,
             t5.script_time_us                          as time5_us,
             t4.script_time_us * 10 / t5.script_time_us as time_ratio
      from (select b.blockId,
                   b.cost               as cost,
                   t.script_time / 1000 as script_time_us,
                   t.script_count
            from (select blockId,
                         sum(time) as script_time,
                         count(*)  as script_count
                  from verifyScript
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            script_time / 1000 as script_time_us
                     from (select blockId, sum(time) as script_time
                           from verifyScript4
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
-- where t.time_ratio <= 19
group by t.time_ratio / 10
order by t.time_ratio / 10 asc;

-- count blocks by script_time speedup (selected)
select t.time_ratio / 10,
       count(*),
       round(avg(t.script_count), 1) as avg_script_count,
       round(avg(t.time4_us), 1)     as avg_t4_us,
       round(avg(t.time5_us), 1)     as avg_t5_us
from (select t5.script_count,
             t4.script_time_us                          as time4_us,
             t5.script_time_us                          as time5_us,
             t4.script_time_us * 10 / t5.script_time_us as time_ratio
      from (select b.blockId,
                   b.cost               as cost,
                   t.script_time / 1000 as script_time_us,
                   t.script_count
            from (select blockId,
                         sum(time) as script_time,
                         count(*)  as script_count
                  from verifyScript as s
                           join selectedInputs as i on s.txId = i.txId and s.boxIndex = i.boxIndex
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            script_time / 1000 as script_time_us
                     from (select blockId, sum(time) as script_time
                           from verifyScript4 as s
                                    join selectedInputs as i
                                         on s.txId = i.txId and s.boxIndex = i.boxIndex
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
-- where t.time_ratio <= 19
group by t.time_ratio / 10
order by t.time_ratio / 10 asc;

-- small ratio: count blocks by script_time speedup
select round(t.time_ratio * 0.1, 1)  as speedup_ratio,
       count(*)                      as num_blocks,
       round(avg(t.script_count), 1) as avg_script_count,
       round(avg(t.time4_us), 1)     as avg_t4_us,
       round(avg(t.time5_us), 1)     as avg_t5_us
from (select t5.script_count,
             t4.script_time_us                          as time4_us,
             t5.script_time_us                          as time5_us,
             t4.script_time_us * 10 / t5.script_time_us as time_ratio
      from (select b.blockId,
                   b.cost               as cost,
                   t.script_time / 1000 as script_time_us,
                   t.script_count
            from (select blockId,
                         sum(time) as script_time,
                         count(*)  as script_count
                  from verifyScript
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            script_time / 1000 as script_time_us
                     from (select blockId, sum(time) as script_time
                           from verifyScript4
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
where t.time_ratio >= 10
  and t.time_ratio <= 40
group by t.time_ratio
order by t.time_ratio asc;

-- small ratio: count blocks by script_time speedup (selected)
select round(t.time_ratio * 0.1, 1)  as speedup_ratio,
       count(*)                      as num_blocks,
       round(avg(t.script_count), 1) as avg_script_count,
       round(avg(t.time4_us), 1)     as avg_t4_us,
       round(avg(t.time5_us), 1)     as avg_t5_us
from (select t5.script_count,
             t4.script_time_us                          as time4_us,
             t5.script_time_us                          as time5_us,
             t4.script_time_us * 10 / t5.script_time_us as time_ratio
      from (select b.blockId,
                   b.cost               as cost,
                   t.script_time / 1000 as script_time_us,
                   t.script_count
            from (select blockId,
                         sum(time) as script_time,
                         count(*)  as script_count
                  from verifyScript as s
                           join selectedInputs as i on s.txId = i.txId and s.boxIndex = i.boxIndex
                  group by blockId) as t
                     join applyTransactions as b on b.blockId = t.blockId) as t5

               join (select blockId,
                            script_time / 1000 as script_time_us
                     from (select blockId, sum(time) as script_time
                           from verifyScript4 as s
                                    join selectedInputs as i
                                         on s.txId = i.txId and s.boxIndex = i.boxIndex
                           group by blockId)) as t4 on t5.blockId = t4.blockId) as t
where t.time_ratio >= 10
  and t.time_ratio <= 40
group by t.time_ratio
order by t.time_ratio asc;

select count(*) from selectedInputs;

-- count script rows
select *, total_rows5 - total_rows4 as v5_v4_rows_diff from
(select count(*) as total_rows5
from verifyScript as s),
(select count(*) as total_rows4
from verifyScript4 as s),
(select count(*) as common_rows
from verifyScript as t5
         join verifyScript4 t4
              on t5.blockId = t4.blockId
                  and t5.txId = t4.txId
                  and t5.boxIndex = t4.boxIndex);

-- compare total script time
select times.total_time4 / 1000                                   as total_time4_us,
       times.total_time5 / 1000                                   as total_time5_us,
       (times.total_time4 - times.total_time5) / 1000             as total_diff_us,
       round((1 - round(times.total_time5 * 100 / times.total_time4 * 0.01, 1)) * 100, 1) as percent_of_reduction
from (select sum(t5.time) as total_time5,
             sum(t4.time) as total_time4
      from verifyScript as t5
               join verifyScript4 t4
                    on t5.blockId = t4.blockId
                        and t5.txId = t4.txId
                        and t5.boxIndex = t4.boxIndex) as times;

-- count v4 script which are not selected
select count(*) as total_rows4
from verifyScript4 as s
where not exists(
        select *
        from selectedInputs as i
        where s.txId = i.txId
          and s.boxIndex = i.boxIndex);

-- sum of all script times (!selected)
select total_time4_us,
       total_time_us,
       total_time4_us - total_time_us                      as total_diff_us,
       round(total_time4_us * 10 / total_time_us * 0.1, 1) as ratio
from (select total_time4 / 1000 as total_time4_us
      from (select sum(time) as total_time4
            from verifyScript4 as s
            where not exists(
                    select *
                    from selectedInputs
                    where s.txId = txId
                      and s.boxIndex = boxIndex))),
     (select total_time / 1000 as total_time_us
      from (select sum(time) as total_time
            from verifyScript as s
            where not exists(
                    select *
                    from selectedInputs
                    where s.txId = txId
                      and s.boxIndex = boxIndex)));

-- count of all scripts (selected)
select count(*) as total_count4
from verifyScript4 as s
         join selectedInputs as i on s.txId = i.txId and s.boxIndex = i.boxIndex;

-- sum of all script times (selected)
select total_time4_us,
       total_time_us,
       total_rows,
       total_time4_us - total_time_us                      as total_diff_us,
       round(total_time4_us * 10 / total_time_us * 0.1, 1) as ratio
from (select total_time4 / 1000 as total_time4_us,
             total_time / 1000  as total_time_us,
             total_rows
      from (select sum(s4.time) as total_time4,
                   sum(s.time)  as total_time,
                   count(*)     as total_rows
            from verifyScript4 as s4
                     join verifyScript as s
                          on s4.blockId = s.blockId and s4.txId = s.txId and
                             s4.boxIndex = s.boxIndex
                     join selectedInputs as i on s4.txId = i.txId and s4.boxIndex = i.boxIndex))

