# Performance And Costing Analysis of Ergo Node

## Introduction
This notebook uses `Metrics.sqlite` database collected during full node syncronization.
See `README.md` for detail of database schema.



## Setup
If necessary uncomment and run the following commands to setup necessary packages.



```python
#!pip install ipython-sql
#!pip install jupyter_contrib_nbextensions
#!jupyter contrib nbextension install --user
!jupyter nbextension enable python-markdown / main

```

    Please specify one nbextension/package at a time



```python
# open sqlite connection to perform queries
import pandas as pd
import sqlite3
conn = sqlite3.connect("../Metrics.sqlite")

```

## v5.0 Validation
- make sure validateTxStateful recorded for each transaction of the block (should be empty)


```python
pd.read_sql_query(f"""
    -- invalid tx_num
    select b.blockId, t.tx_count as c, b.tx_num as n
    from (select blockId, count(*) as tx_count
          from validateTxStateful
          group by blockId) as t
             join applyTransactions as b on b.blockId = t.blockId
    where c != n;
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>blockId</th>
      <th>c</th>
      <th>n</th>
    </tr>
  </thead>
  <tbody>
  </tbody>
</table>
</div>



- make sure recored block cost = sum of recorded tx costs (should be empty)


```python
pd.read_sql_query(f"""
    -- invalid tx cost
    select b.blockId, t.sum_costs as sum_tx_costs, b.cost as block_cost
    from (select blockId, sum(cost) as sum_costs
          from validateTxStateful
          group by blockId) as t
             join applyTransactions as b on b.blockId = t.blockId
    where sum_tx_costs != block_cost;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>blockId</th>
      <th>sum_tx_costs</th>
      <th>block_cost</th>
    </tr>
  </thead>
  <tbody>
  </tbody>
</table>
</div>



- now many transactions have negative cost (should be empty)


```python
pd.read_sql_query(f"""
-- count tx with negative cost
select count(*)
from validateTxStateful
where cost < 0;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
    </tr>
  </tbody>
</table>
</div>



## v5.0 Analysis
### Block Validation Time Analysis
In this section we compare script validation time against block validation time.

#### Sizes of the recorded tables


```python
pd.read_sql_query(f"""
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
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>appendFullBlock</th>
      <th>applyTransactions</th>
      <th>createUtxoState</th>
      <th>validateTxStateful</th>
      <th>verifyScript</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>501900</td>
      <td>501900</td>
      <td>501900</td>
      <td>1391500</td>
      <td>4195700</td>
    </tr>
  </tbody>
</table>
</div>



#### Comparing Stages of Block Validation
First we look at how much time of the total block validation is spent in applyTransactions.
We group and count blocks by (total time / `applyTransactions` time) ratio.
We see that `applyTransactions` is < 50% for roughly 80% of the blocks.
For more ~40% of the blocks:
1) the ratio is above 3, which means script validation is < 33%
2) further analysis shows that time is spent in creating UtxoState (after applyTransaction)
**Thus, creating UtxoState after application of transactions requires profiling and optimization.**


```python
pd.read_sql_query(f"""
select time_us / t2_us as time_ratio, count(*) as block_count
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
group by time_ratio
order by time_ratio;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>block_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>71161</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>176815</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>190927</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>26227</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>21516</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>61</th>
      <td>201</td>
      <td>1</td>
    </tr>
    <tr>
      <th>62</th>
      <td>220</td>
      <td>1</td>
    </tr>
    <tr>
      <th>63</th>
      <td>224</td>
      <td>1</td>
    </tr>
    <tr>
      <th>64</th>
      <td>226</td>
      <td>1</td>
    </tr>
    <tr>
      <th>65</th>
      <td>237</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
<p>66 rows × 2 columns</p>
</div>



#### Comparing applyTransaction of block with validateTxStateful
Here we further drill down to applyTransactions part of block validation.
Specifically, for each block we compare the time of `UtxoState.applyTransactions` with total time
of `ErgoTransaction.validateStateful` taken for all transactions in the block.
The blocks are grouped by the ratio between times.
We can see that for > 70% blocks the ration is above 2, which suggests that
**`UtxoState.applyTransactions` method need optimizations.**


```python
pd.read_sql_query(f"""
select t.time_ratio / 10 as time_ratio,
       count(*) as block_count,
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
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>block_count</th>
      <th>avg_block_time_us</th>
      <th>avg_tx_time_us</th>
      <th>avg_tx_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>156281</td>
      <td>18827.0</td>
      <td>15793.0</td>
      <td>6.7</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>191267</td>
      <td>1246.0</td>
      <td>445.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>153325</td>
      <td>1209.0</td>
      <td>393.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>287</td>
      <td>5385.0</td>
      <td>1210.0</td>
      <td>1.5</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>135</td>
      <td>4985.0</td>
      <td>902.0</td>
      <td>1.3</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>70</th>
      <td>79</td>
      <td>1</td>
      <td>32010.0</td>
      <td>402.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>71</th>
      <td>81</td>
      <td>1</td>
      <td>34432.0</td>
      <td>420.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>72</th>
      <td>84</td>
      <td>1</td>
      <td>37181.0</td>
      <td>438.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>73</th>
      <td>90</td>
      <td>1</td>
      <td>47346.0</td>
      <td>525.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>74</th>
      <td>624</td>
      <td>1</td>
      <td>288505.0</td>
      <td>461.0</td>
      <td>1.0</td>
    </tr>
  </tbody>
</table>
<p>75 rows × 5 columns</p>
</div>



#### Tx/Script validation time ratio
Further down to validation call stack, we observe how much `validateStateful` time larger than
script verification time.
The ratio is computed for each transaction and then the transactions are grouped by integer ratio.
Comparing times in `verifyScript` and `validateTxStateful` metrics.
We see that for most transactions, `verifyScript` is > 50% of `validateStateful`.


```python
pd.read_sql_query(f"""
select t.time_ratio / 10 as time_ratio,
       count(*) as tx_count,
       round(avg(t.tx_time_us), 1) as avg_tx_time_us,
       round(avg(t.script_time_us), 1) as avg_script_time_us,
       round(avg(t.script_count), 1) as avg_script_count
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
order by t.time_ratio / 10;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>tx_count</th>
      <th>avg_tx_time_us</th>
      <th>avg_script_time_us</th>
      <th>avg_script_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>1391051</td>
      <td>1874.3</td>
      <td>1828.9</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>143</td>
      <td>6646.5</td>
      <td>2658.4</td>
      <td>5.8</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>68</td>
      <td>14709.5</td>
      <td>4136.0</td>
      <td>9.7</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4</td>
      <td>36</td>
      <td>11006.4</td>
      <td>2473.1</td>
      <td>6.1</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5</td>
      <td>20</td>
      <td>9853.2</td>
      <td>1804.8</td>
      <td>5.0</td>
    </tr>
    <tr>
      <th>5</th>
      <td>6</td>
      <td>20</td>
      <td>14838.7</td>
      <td>2328.2</td>
      <td>5.5</td>
    </tr>
    <tr>
      <th>6</th>
      <td>7</td>
      <td>16</td>
      <td>10072.1</td>
      <td>1350.2</td>
      <td>3.1</td>
    </tr>
    <tr>
      <th>7</th>
      <td>8</td>
      <td>9</td>
      <td>11278.0</td>
      <td>1329.9</td>
      <td>3.2</td>
    </tr>
    <tr>
      <th>8</th>
      <td>9</td>
      <td>8</td>
      <td>16838.9</td>
      <td>1812.3</td>
      <td>4.3</td>
    </tr>
    <tr>
      <th>9</th>
      <td>10</td>
      <td>14</td>
      <td>15309.4</td>
      <td>1458.6</td>
      <td>5.5</td>
    </tr>
    <tr>
      <th>10</th>
      <td>11</td>
      <td>6</td>
      <td>8631.2</td>
      <td>759.0</td>
      <td>1.5</td>
    </tr>
    <tr>
      <th>11</th>
      <td>12</td>
      <td>6</td>
      <td>58768.7</td>
      <td>4656.2</td>
      <td>14.0</td>
    </tr>
    <tr>
      <th>12</th>
      <td>13</td>
      <td>5</td>
      <td>15259.0</td>
      <td>1124.4</td>
      <td>4.6</td>
    </tr>
    <tr>
      <th>13</th>
      <td>14</td>
      <td>7</td>
      <td>13462.6</td>
      <td>935.3</td>
      <td>3.6</td>
    </tr>
    <tr>
      <th>14</th>
      <td>15</td>
      <td>6</td>
      <td>13018.3</td>
      <td>847.0</td>
      <td>2.3</td>
    </tr>
    <tr>
      <th>15</th>
      <td>16</td>
      <td>7</td>
      <td>15353.9</td>
      <td>947.1</td>
      <td>2.4</td>
    </tr>
    <tr>
      <th>16</th>
      <td>17</td>
      <td>3</td>
      <td>12081.0</td>
      <td>679.3</td>
      <td>2.7</td>
    </tr>
    <tr>
      <th>17</th>
      <td>18</td>
      <td>1</td>
      <td>17360.0</td>
      <td>944.0</td>
      <td>6.0</td>
    </tr>
    <tr>
      <th>18</th>
      <td>19</td>
      <td>4</td>
      <td>18347.5</td>
      <td>944.3</td>
      <td>4.5</td>
    </tr>
    <tr>
      <th>19</th>
      <td>20</td>
      <td>1</td>
      <td>18032.0</td>
      <td>864.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>20</th>
      <td>21</td>
      <td>9</td>
      <td>24950.0</td>
      <td>1147.3</td>
      <td>1.3</td>
    </tr>
    <tr>
      <th>21</th>
      <td>22</td>
      <td>2</td>
      <td>16437.0</td>
      <td>730.5</td>
      <td>4.0</td>
    </tr>
    <tr>
      <th>22</th>
      <td>23</td>
      <td>3</td>
      <td>10673.7</td>
      <td>452.7</td>
      <td>1.7</td>
    </tr>
    <tr>
      <th>23</th>
      <td>24</td>
      <td>4</td>
      <td>11319.3</td>
      <td>461.8</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>24</th>
      <td>25</td>
      <td>3</td>
      <td>12954.7</td>
      <td>502.7</td>
      <td>2.3</td>
    </tr>
    <tr>
      <th>25</th>
      <td>26</td>
      <td>1</td>
      <td>13534.0</td>
      <td>503.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>26</th>
      <td>27</td>
      <td>6</td>
      <td>15642.8</td>
      <td>565.2</td>
      <td>1.2</td>
    </tr>
    <tr>
      <th>27</th>
      <td>28</td>
      <td>2</td>
      <td>11922.0</td>
      <td>417.5</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>28</th>
      <td>29</td>
      <td>1</td>
      <td>14977.0</td>
      <td>500.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>29</th>
      <td>30</td>
      <td>1</td>
      <td>11400.0</td>
      <td>379.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>30</th>
      <td>31</td>
      <td>1</td>
      <td>14555.0</td>
      <td>455.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>31</th>
      <td>35</td>
      <td>2</td>
      <td>18180.5</td>
      <td>512.5</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>32</th>
      <td>36</td>
      <td>2</td>
      <td>50401.5</td>
      <td>1372.5</td>
      <td>2.5</td>
    </tr>
    <tr>
      <th>33</th>
      <td>37</td>
      <td>3</td>
      <td>30459.3</td>
      <td>808.0</td>
      <td>3.3</td>
    </tr>
    <tr>
      <th>34</th>
      <td>38</td>
      <td>2</td>
      <td>14596.5</td>
      <td>378.5</td>
      <td>1.5</td>
    </tr>
    <tr>
      <th>35</th>
      <td>40</td>
      <td>1</td>
      <td>51299.0</td>
      <td>1278.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>36</th>
      <td>43</td>
      <td>1</td>
      <td>16743.0</td>
      <td>388.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>37</th>
      <td>44</td>
      <td>1</td>
      <td>18163.0</td>
      <td>409.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>38</th>
      <td>45</td>
      <td>1</td>
      <td>19001.0</td>
      <td>418.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>39</th>
      <td>46</td>
      <td>2</td>
      <td>12497.5</td>
      <td>269.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>40</th>
      <td>49</td>
      <td>1</td>
      <td>23217.0</td>
      <td>467.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>41</th>
      <td>50</td>
      <td>1</td>
      <td>20143.0</td>
      <td>396.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>42</th>
      <td>52</td>
      <td>1</td>
      <td>7761.0</td>
      <td>148.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>43</th>
      <td>53</td>
      <td>1</td>
      <td>20400.0</td>
      <td>383.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>44</th>
      <td>55</td>
      <td>1</td>
      <td>263074.0</td>
      <td>4770.0</td>
      <td>3.0</td>
    </tr>
    <tr>
      <th>45</th>
      <td>57</td>
      <td>2</td>
      <td>8762.5</td>
      <td>151.5</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>46</th>
      <td>61</td>
      <td>1</td>
      <td>40988.0</td>
      <td>662.0</td>
      <td>4.0</td>
    </tr>
    <tr>
      <th>47</th>
      <td>64</td>
      <td>1</td>
      <td>32877.0</td>
      <td>506.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>48</th>
      <td>66</td>
      <td>1</td>
      <td>24995.0</td>
      <td>374.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>49</th>
      <td>75</td>
      <td>1</td>
      <td>21990.0</td>
      <td>291.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>50</th>
      <td>88</td>
      <td>1</td>
      <td>30168.0</td>
      <td>339.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>51</th>
      <td>98</td>
      <td>1</td>
      <td>40378.0</td>
      <td>410.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>52</th>
      <td>104</td>
      <td>2</td>
      <td>64876.5</td>
      <td>618.0</td>
      <td>1.5</td>
    </tr>
    <tr>
      <th>53</th>
      <td>105</td>
      <td>1</td>
      <td>28879.0</td>
      <td>274.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>54</th>
      <td>110</td>
      <td>2</td>
      <td>131446.5</td>
      <td>1192.5</td>
      <td>6.5</td>
    </tr>
    <tr>
      <th>55</th>
      <td>142</td>
      <td>1</td>
      <td>305987.0</td>
      <td>2140.0</td>
      <td>2.0</td>
    </tr>
    <tr>
      <th>56</th>
      <td>158</td>
      <td>1</td>
      <td>225952.0</td>
      <td>1421.0</td>
      <td>10.0</td>
    </tr>
    <tr>
      <th>57</th>
      <td>160</td>
      <td>1</td>
      <td>385017.0</td>
      <td>2406.0</td>
      <td>12.0</td>
    </tr>
  </tbody>
</table>
</div>



We need to further drill down inside the mose populated group.
We build a detailed grouping of the transactions where the ratio <= 1.9,
comparing times in `verifyScript` and `validateTxStateful` metrics.


```python
pd.read_sql_query(f"""
select round(t.time_ratio * 0.1, 1) as time_ratio,
       count(*) as tx_count,
       round(avg(t.tx_time_us), 1) as avg_tx_time_us,
       round(avg(t.script_time_us), 1) as avg_script_time_us,
       round(avg(t.script_count), 1) as avg_script_count
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
order by t.time_ratio;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>time_ratio</th>
      <th>tx_count</th>
      <th>avg_tx_time_us</th>
      <th>avg_script_time_us</th>
      <th>avg_script_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1.0</td>
      <td>1241467</td>
      <td>2049.9</td>
      <td>2005.7</td>
      <td>3.2</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1.1</td>
      <td>140241</td>
      <td>373.7</td>
      <td>333.1</td>
      <td>1.2</td>
    </tr>
    <tr>
      <th>2</th>
      <td>1.2</td>
      <td>7639</td>
      <td>715.8</td>
      <td>583.3</td>
      <td>1.6</td>
    </tr>
    <tr>
      <th>3</th>
      <td>1.3</td>
      <td>531</td>
      <td>2484.9</td>
      <td>1841.5</td>
      <td>4.8</td>
    </tr>
    <tr>
      <th>4</th>
      <td>1.4</td>
      <td>914</td>
      <td>1056.6</td>
      <td>731.4</td>
      <td>2.3</td>
    </tr>
    <tr>
      <th>5</th>
      <td>1.5</td>
      <td>100</td>
      <td>4563.4</td>
      <td>2956.9</td>
      <td>7.4</td>
    </tr>
    <tr>
      <th>6</th>
      <td>1.6</td>
      <td>61</td>
      <td>6348.2</td>
      <td>3862.5</td>
      <td>6.8</td>
    </tr>
    <tr>
      <th>7</th>
      <td>1.7</td>
      <td>47</td>
      <td>14166.8</td>
      <td>8054.3</td>
      <td>17.6</td>
    </tr>
    <tr>
      <th>8</th>
      <td>1.8</td>
      <td>27</td>
      <td>9443.3</td>
      <td>5183.7</td>
      <td>11.7</td>
    </tr>
    <tr>
      <th>9</th>
      <td>1.9</td>
      <td>24</td>
      <td>14075.8</td>
      <td>7210.2</td>
      <td>20.3</td>
    </tr>
  </tbody>
</table>
</div>



Count transactions where script validation <= 80% of `validateStatefull`.
This DOESN'T show big potential for optimizing `validateStateful` outside script evaluation.
Comparing times in `verifyScript` and `validateTxStateful` metrics.


```python
pd.read_sql_query(f"""
select count(*) as tx_count
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
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>tx_count</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>9792</td>
    </tr>
  </tbody>
</table>
</div>



Conclusions:
- Creating UtxoState after application of transactions requires profiling and optimization.
- `UtxoState.applyTransactions` method need optimizations.
- In most of the cases the time to validate transaction (`validateStateful` method) is dominated by
the script validation time
- There is a few transactions where script validation not greater than 80% of tx validation time.
- The results suggest that `validateStateful` doesn't require optimizations, or at least it can
be done after the other parts of block validation had been optimized.

## v5.0 vs v4.0 Cross Analysis

### Cross Version validation
- Make sure recorded data for v5 and v4 have the same `height` and `tx_num` for all blockIds



```python
checks = dict(
    appendFullBlockOk=pd.read_sql_query(f"""
        -- validate appendFullBlock tables
        select *
        from appendFullBlock a1
                 join appendFullBlock4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0,
    applyTransactionsOk=pd.read_sql_query(f"""
        select *
        from applyTransactions a1
                 join applyTransactions4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0,
    createUtxoStateOk=pd.read_sql_query(f"""
        select *
        from createUtxoState a1
                 join createUtxoState4 a2 on a1.blockId = a2.blockId
        where a1.height != a2.height
           or a1.tx_num != a2.tx_num;
        """, conn).size == 0
)
if not (checks.get("appendFullBlockOk") and checks.get("applyTransactionsOk") and checks.get(
        "createUtxoStateOk")):
    ok = checks
else:
    ok = "ok"
print(checks)

```

    {'appendFullBlockOk': True, 'applyTransactionsOk': True, 'createUtxoStateOk': True}


### Block Validation Times
First count recorded block validations of v4 and v5 and how much of them can be
joined. This is also validation check, because `common_rows` should be equal to the minimal count.


```python
pd.read_sql_query(f"""
select *, count_rows5 - count_rows4 as v5_v4_rows_diff
from (select count(*) as count_rows5 from applyTransactions),
     (select count(*) as count_rows4 from applyTransactions4),
     (select count(*) as common_rows
      from applyTransactions as t5 join applyTransactions4 t4 on t5.blockId = t4.blockId);
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>count_rows5</th>
      <th>count_rows4</th>
      <th>common_rows</th>
      <th>v5_v4_rows_diff</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>501900</td>
      <td>473000</td>
      <td>473000</td>
      <td>28900</td>
    </tr>
  </tbody>
</table>
</div>



The new v5 script interpreter is expected to perform faster comparing to v4.
The following shows the distribution of the blocks across ranges of speedup. We see that for the
most of the blocks the speedup is in a range from 1 (no speedup) to 2 (twice faster).


```python
pd.read_sql_query(f"""
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
group by speedup order by speedup;
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>12044</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>447487</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>8592</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1774</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>1041</td>
    </tr>
    <tr>
      <th>5</th>
      <td>5</td>
      <td>626</td>
    </tr>
    <tr>
      <th>6</th>
      <td>6</td>
      <td>558</td>
    </tr>
    <tr>
      <th>7</th>
      <td>7</td>
      <td>193</td>
    </tr>
    <tr>
      <th>8</th>
      <td>8</td>
      <td>188</td>
    </tr>
    <tr>
      <th>9</th>
      <td>9</td>
      <td>124</td>
    </tr>
    <tr>
      <th>10</th>
      <td>10</td>
      <td>94</td>
    </tr>
    <tr>
      <th>11</th>
      <td>11</td>
      <td>49</td>
    </tr>
    <tr>
      <th>12</th>
      <td>12</td>
      <td>34</td>
    </tr>
    <tr>
      <th>13</th>
      <td>13</td>
      <td>26</td>
    </tr>
    <tr>
      <th>14</th>
      <td>14</td>
      <td>21</td>
    </tr>
    <tr>
      <th>15</th>
      <td>15</td>
      <td>17</td>
    </tr>
    <tr>
      <th>16</th>
      <td>16</td>
      <td>11</td>
    </tr>
    <tr>
      <th>17</th>
      <td>17</td>
      <td>7</td>
    </tr>
    <tr>
      <th>18</th>
      <td>18</td>
      <td>10</td>
    </tr>
    <tr>
      <th>19</th>
      <td>19</td>
      <td>9</td>
    </tr>
    <tr>
      <th>20</th>
      <td>20</td>
      <td>9</td>
    </tr>
    <tr>
      <th>21</th>
      <td>21</td>
      <td>5</td>
    </tr>
    <tr>
      <th>22</th>
      <td>22</td>
      <td>3</td>
    </tr>
    <tr>
      <th>23</th>
      <td>23</td>
      <td>7</td>
    </tr>
    <tr>
      <th>24</th>
      <td>24</td>
      <td>1</td>
    </tr>
    <tr>
      <th>25</th>
      <td>25</td>
      <td>8</td>
    </tr>
    <tr>
      <th>26</th>
      <td>26</td>
      <td>8</td>
    </tr>
    <tr>
      <th>27</th>
      <td>27</td>
      <td>6</td>
    </tr>
    <tr>
      <th>28</th>
      <td>28</td>
      <td>4</td>
    </tr>
    <tr>
      <th>29</th>
      <td>29</td>
      <td>6</td>
    </tr>
    <tr>
      <th>30</th>
      <td>30</td>
      <td>3</td>
    </tr>
    <tr>
      <th>31</th>
      <td>31</td>
      <td>7</td>
    </tr>
    <tr>
      <th>32</th>
      <td>32</td>
      <td>6</td>
    </tr>
    <tr>
      <th>33</th>
      <td>33</td>
      <td>2</td>
    </tr>
    <tr>
      <th>34</th>
      <td>34</td>
      <td>2</td>
    </tr>
    <tr>
      <th>35</th>
      <td>35</td>
      <td>4</td>
    </tr>
    <tr>
      <th>36</th>
      <td>36</td>
      <td>2</td>
    </tr>
    <tr>
      <th>37</th>
      <td>37</td>
      <td>2</td>
    </tr>
    <tr>
      <th>38</th>
      <td>38</td>
      <td>3</td>
    </tr>
    <tr>
      <th>39</th>
      <td>40</td>
      <td>2</td>
    </tr>
    <tr>
      <th>40</th>
      <td>46</td>
      <td>1</td>
    </tr>
    <tr>
      <th>41</th>
      <td>51</td>
      <td>1</td>
    </tr>
    <tr>
      <th>42</th>
      <td>53</td>
      <td>1</td>
    </tr>
    <tr>
      <th>43</th>
      <td>57</td>
      <td>1</td>
    </tr>
    <tr>
      <th>44</th>
      <td>62</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>



We see outliers, i.e. blocks which executed slower in v5 than in v4. This can be attributed to
measurement fluctuations. Such blocks will be simply filtered out in the following analysis.

We also see a long tail of blocks with higher than 2 speedups.


```python
pd.read_sql_query(f"""
select sum(num_blocks) from (
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where speedup >= 2
group by speedup order by speedup
)
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>sum(num_blocks)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>13469</td>
    </tr>
  </tbody>
</table>
</div>



Let's see how the speedup is spread over ranges of the blockchain.
The block `b` falls in `range` if `range = b.height / 100000`. The blocks of each range are counted,
and the average speedup over the range is computed.


```python
pd.read_sql_query(f"""
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*)                                      as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time / t5.time >= 1
group by range order by range;
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>range</th>
      <th>avg_speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>1.20</td>
      <td>96792</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>1.24</td>
      <td>98003</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>1.30</td>
      <td>98005</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1.38</td>
      <td>97122</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>1.42</td>
      <td>71034</td>
    </tr>
  </tbody>
</table>
</div>



We observe that the average speedup increase towards the most recent range, which is expected, because
more and more complex contracts are used on chain over time. The v5 script interpreter is by design
faster on complex contracts.

If we further constrain speedup to be above 1.2, we get the following distribution of blocks.


```python
pd.read_sql_query(f"""
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time * 100 / t5.time * 0.01 >= 1.2
group by range order by range
""", conn)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>range</th>
      <th>avg_speedup</th>
      <th>num_blocks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>1.37</td>
      <td>31170</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>1.36</td>
      <td>45936</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>1.40</td>
      <td>62381</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>1.45</td>
      <td>73320</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>1.51</td>
      <td>53584</td>
    </tr>
  </tbody>
</table>
</div>



Note, this speedups are due to improvements in script evaluation which is further analyzed in the
[next section](#script-validation-time). However, as shown in [Comparing Stages of Block
Validation](#comparing-stages-of-block-validation), for most of the blocks, script evaluation is
less than 50% of the block validation time, so the impact of script speedups is limited by this
factor.


### Script Validation Time
In this section we focus on script evaluation part only (i.e. Interpreter.verify method), which is
measured and recorded in `verifyScript` table.

First count recorded script validations of v4 and v5 and how much of them can be
joined. This is also validation check, because `common_rows` should be equal to the minimal count.


```python
pd.read_sql_query(f"""
select *, total_rows5 - total_rows4 as v5_v4_rows_diff from
(select count(*) as total_rows5
from verifyScript as s),
(select count(*) as total_rows4
from verifyScript4 as s),
(select count(*) as common_rows
from verifyScript as t5 join verifyScript4 t4
  on t5.blockId = t4.blockId and t5.txId = t4.txId and t5.boxIndex = t4.boxIndex);
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>total_rows5</th>
      <th>total_rows4</th>
      <th>common_rows</th>
      <th>v5_v4_rows_diff</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>4195700</td>
      <td>3537350</td>
      <td>3537350</td>
      <td>658350</td>
    </tr>
  </tbody>
</table>
</div>



  For common recorded script we compute the total time spend in script validation for both v4 and v5
  and compare them. Then compute the expected percent of total script time reduction after switching
  to v5.0.


```python
pd.read_sql_query(f"""
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
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>total_time4_us</th>
      <th>total_time5_us</th>
      <th>total_diff_us</th>
      <th>percent_of_reduction</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>3175910119</td>
      <td>2249239412</td>
      <td>926670707</td>
      <td>30.0</td>
    </tr>
  </tbody>
</table>
</div>



### Block Validation Cost Analysis
In this section we compare block validation costs against block validation time in order to see how
accurate cost estimation predicts the actual execution time.

The validation complexity is estimated in cost units, one cost unit corresponds approximately to
1 microsecond of execution time, thus when cost is 1000 then execution time is expected to be 1000
microseconds.

Since cost predition is a security measure, we want to be conservative and require that for most
blocks the predicted cost is larger than execution time in microseconds.

The following table counts blocks with execution time exceeding predicted cost. The blocks are
grouped by `time / cost` ratio. We see that in v4.x only 50 blocks were validated longer than
predicted. The one outlier block is measurement artefact and can be ignored.



```python
pd.read_sql_query(f"""
-- find blocks where cost is less than time_us
select min(ratio / 10) as ratio, count(*)
from (select height,
             tx_num,
             cost,
             time / 1000          as time_us,
             (time / 1000) * 10 / cost as ratio
      from applyTransactions4
      where time_us > cost)
group by ratio / 10
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>39</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>61</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>




The same query with v5.0 execution data shows similar results.


```python
pd.read_sql_query(f"""
-- find blocks where cost is less than time_us
select min(ratio / 10) as ratio, count(*)
from (select height,
             tx_num,
             cost as full_cost,
             time / 1000          as time_us,
             (time / 1000) * 10 / cost as ratio
      from applyTransactions
      where time_us > full_cost)
group by ratio / 10
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>333</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2</td>
      <td>45</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3</td>
      <td>3</td>
    </tr>
    <tr>
      <th>3</th>
      <td>23</td>
      <td>1</td>
    </tr>
    <tr>
      <th>4</th>
      <td>334</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>



We can conclude that both v4.x and v5.0 costing can be used as the upper bound of the actual block
validation time, i.e. for most of the blocks the cost value is larger than execution time in
microseconds. How accurate this bound?

In v4.x the `cost / time` ratio is in [20 .. 70) range which is quite conservative, leaving a lot of
room for improvement.


```python
pd.read_sql_query(f"""
-- group and count blocks by cost/time ratio (v4)
select min(ratio), count(*), round(avg(tx_num), 2) as avg_tx_num
from (select height,
             tx_num,
             cost as full_cost,
             time / 1000          as time_us,
             cost / (time / 1000)  as ratio
      from applyTransactions4
      where time_us <= full_cost)
group by ratio / 10
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>min(ratio)</th>
      <th>count(*)</th>
      <th>avg_tx_num</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>3044</td>
      <td>2.53</td>
    </tr>
    <tr>
      <th>1</th>
      <td>10</td>
      <td>21624</td>
      <td>2.06</td>
    </tr>
    <tr>
      <th>2</th>
      <td>20</td>
      <td>328774</td>
      <td>1.28</td>
    </tr>
    <tr>
      <th>3</th>
      <td>30</td>
      <td>42982</td>
      <td>4.46</td>
    </tr>
    <tr>
      <th>4</th>
      <td>40</td>
      <td>34351</td>
      <td>5.64</td>
    </tr>
    <tr>
      <th>5</th>
      <td>50</td>
      <td>26201</td>
      <td>9.07</td>
    </tr>
    <tr>
      <th>6</th>
      <td>60</td>
      <td>15744</td>
      <td>10.41</td>
    </tr>
    <tr>
      <th>7</th>
      <td>70</td>
      <td>49</td>
      <td>5.90</td>
    </tr>
    <tr>
      <th>8</th>
      <td>80</td>
      <td>59</td>
      <td>8.20</td>
    </tr>
    <tr>
      <th>9</th>
      <td>90</td>
      <td>108</td>
      <td>9.34</td>
    </tr>
    <tr>
      <th>10</th>
      <td>100</td>
      <td>22</td>
      <td>9.77</td>
    </tr>
    <tr>
      <th>11</th>
      <td>110</td>
      <td>1</td>
      <td>15.00</td>
    </tr>
  </tbody>
</table>
</div>



Indeed, as can be seen in the following table, in v5.0 cost prediction is significantly improved so
that the `cost / time` ratio is in [1 .. 19) range for most blocks.



```python
pd.read_sql_query(f"""
-- group and count blocks by cost/time ratio (v5)
select min(ratio), count(*), round(avg(tx_num), 2) as avg_tx_num
from (select height,
             tx_num,
             cost as full_cost,
             time / 1000          as time_us,
             cost / (time / 1000)  as ratio
      from applyTransactions
      where time_us <= full_cost)
group by ratio / 10
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>min(ratio)</th>
      <th>count(*)</th>
      <th>avg_tx_num</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>210011</td>
      <td>3.94</td>
    </tr>
    <tr>
      <th>1</th>
      <td>10</td>
      <td>289260</td>
      <td>1.85</td>
    </tr>
    <tr>
      <th>2</th>
      <td>20</td>
      <td>2246</td>
      <td>12.82</td>
    </tr>
  </tbody>
</table>
</div>



The maximal cost of each block is limited by maxBlockCost parameter stored in block parameters
section of each block. This parameter can be changed by miners via voting. The following table shows
the distribution of blocks over range of cost limits.



```python
pd.read_sql_query(f"""
select min(maxCost), count(*) from applyTransactions
group by maxCost / 1000000
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>min(maxCost)</th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1000000</td>
      <td>276298</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2006718</td>
      <td>41984</td>
    </tr>
    <tr>
      <th>2</th>
      <td>3017580</td>
      <td>70656</td>
    </tr>
    <tr>
      <th>3</th>
      <td>4026959</td>
      <td>56320</td>
    </tr>
    <tr>
      <th>4</th>
      <td>5012407</td>
      <td>26624</td>
    </tr>
    <tr>
      <th>5</th>
      <td>6055523</td>
      <td>27648</td>
    </tr>
    <tr>
      <th>6</th>
      <td>7030268</td>
      <td>2370</td>
    </tr>
  </tbody>
</table>
</div>



We see the cost limit significantly increased by the miners (in fact by the pool operators) from
initial 1000000 up to 7030268 current value.

Now that we have cost limits for each block, let's see how far the actual block validation costs
from that limits.



```python
pd.read_sql_query(f"""
select maxCost / cost as ratio, count(*), min(height) from applyTransactions
group by ratio
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>count(*)</th>
      <th>min(height)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>2</td>
      <td>48</td>
      <td>193540</td>
    </tr>
    <tr>
      <th>1</th>
      <td>3</td>
      <td>785</td>
      <td>164828</td>
    </tr>
    <tr>
      <th>2</th>
      <td>4</td>
      <td>764</td>
      <td>36023</td>
    </tr>
    <tr>
      <th>3</th>
      <td>5</td>
      <td>1951</td>
      <td>3869</td>
    </tr>
    <tr>
      <th>4</th>
      <td>6</td>
      <td>1952</td>
      <td>39862</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>294</th>
      <td>547</td>
      <td>2195</td>
      <td>485376</td>
    </tr>
    <tr>
      <th>295</th>
      <td>552</td>
      <td>432</td>
      <td>490498</td>
    </tr>
    <tr>
      <th>296</th>
      <td>558</td>
      <td>861</td>
      <td>491523</td>
    </tr>
    <tr>
      <th>297</th>
      <td>563</td>
      <td>2615</td>
      <td>493568</td>
    </tr>
    <tr>
      <th>298</th>
      <td>569</td>
      <td>1018</td>
      <td>499712</td>
    </tr>
  </tbody>
</table>
<p>299 rows × 3 columns</p>
</div>



We see that for all blocks the actual cost is at least 2x less then cost limit. However, we also see
that for many blocks the cost is much smaller (569 times) than the limit value .
Lets investigate further the blocks with lowest and highest ratios.



```python
pd.read_sql_query(f"""
select round(maxCost * 10 / cost * 0.1, 1) as ratio, count(*), min(height) from applyTransactions
where ratio <= 3
group by ratio
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>count(*)</th>
      <th>min(height)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>2.4</td>
      <td>1</td>
      <td>429142</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2.6</td>
      <td>3</td>
      <td>419360</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2.7</td>
      <td>9</td>
      <td>238226</td>
    </tr>
    <tr>
      <th>3</th>
      <td>2.8</td>
      <td>11</td>
      <td>193540</td>
    </tr>
    <tr>
      <th>4</th>
      <td>2.9</td>
      <td>24</td>
      <td>226115</td>
    </tr>
    <tr>
      <th>5</th>
      <td>3.0</td>
      <td>33</td>
      <td>193294</td>
    </tr>
  </tbody>
</table>
</div>



What is the block with lowest ratio?



```python
pd.read_sql_query(f"""
select round(t5.maxCost * 10 / t5.cost * 0.1, 1) as ratio,
       t5.height, t5.tx_num, t5.maxCost,
       t5.cost as cost_v5,
       t5.time / 1000 as time_t5_us,
       7030268 / (t5.time / 1000) as scalability_v5,
       t4.cost as cost_v4,
       t4.time / 1000 as time_t4_us
from applyTransactions t5
         join applyTransactions4 t4 on t5.blockId = t4.blockId
where ratio < 2.6
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>height</th>
      <th>tx_num</th>
      <th>maxCost</th>
      <th>cost_v5</th>
      <th>time_t5_us</th>
      <th>scalability_v5</th>
      <th>cost_v4</th>
      <th>time_t4_us</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>2.4</td>
      <td>429142</td>
      <td>114</td>
      <td>4769136</td>
      <td>1947948</td>
      <td>70212</td>
      <td>100</td>
      <td>4753081</td>
      <td>84152</td>
    </tr>
  </tbody>
</table>
</div>



This is the block deep in the blockchain with 114 transactions.
Note, the v4 cost hit the cost limit, so this number of transaction was maximum possible at that
time. This is a good example to illustrate the benefits of v5.0. Not only it executed the same
transaction slightly faster, but also due to more accurate costing, the predicted cost is 2.5 times
lower than v4 cost. This means the operator would have been able to include more than 300
transactions in the block thus increasing network throughput and reduce congestion.

Since then, the cost limit was increased by pool operators up to 7030268, almost 2x.
Using these numbers we can roughly estimate the number of transactions in a block as
114 * (7030268(maxCost) / 1947948(cost_v5)) = 411 transactions.

We can also estimate the potential scalability of v5.0 if we compare the actual execution time with
the current cost limit (see `scalability_v5`). With further tuning of the cost parameters (possible
via voting) the number of such transactions in one block can be 114 * 100 = 11400 (even with the
current unoptimized state management), or 11400 / 120 = 95 tx/second.

At the same time, in this case, the total block validation would take 70212 * 100 = 7021200
microseconds, which is above the recommended time limit of 5 seconds. This suggest that _pool
operators need to postpone further increasing of the maxBlockCost parameter and instead switch on to tuning
the cost parameters to make the cost prediction more accurate_.

Now, what about the other side of the maxCost/cost ratio spectrum, where cost limit is much larger
than the actual block cost.



```python
pd.read_sql_query(f"""
select round(maxCost * 10 / cost * 0.1, 1) as ratio,
       height, tx_num, maxCost, cost,
       time / 1000                         as time_us
from applyTransactions
where ratio >= 569
order by height desc
limit 20
""", conn)

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>ratio</th>
      <th>height</th>
      <th>tx_num</th>
      <th>maxCost</th>
      <th>cost</th>
      <th>time_us</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>569.5</td>
      <td>502080</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1043</td>
    </tr>
    <tr>
      <th>1</th>
      <td>569.5</td>
      <td>502077</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1160</td>
    </tr>
    <tr>
      <th>2</th>
      <td>569.5</td>
      <td>502075</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1043</td>
    </tr>
    <tr>
      <th>3</th>
      <td>569.5</td>
      <td>502072</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1072</td>
    </tr>
    <tr>
      <th>4</th>
      <td>569.5</td>
      <td>502070</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1039</td>
    </tr>
    <tr>
      <th>5</th>
      <td>569.5</td>
      <td>502068</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1048</td>
    </tr>
    <tr>
      <th>6</th>
      <td>569.5</td>
      <td>502066</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1108</td>
    </tr>
    <tr>
      <th>7</th>
      <td>569.5</td>
      <td>502063</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1100</td>
    </tr>
    <tr>
      <th>8</th>
      <td>569.5</td>
      <td>502061</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1123</td>
    </tr>
    <tr>
      <th>9</th>
      <td>569.5</td>
      <td>502059</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1079</td>
    </tr>
    <tr>
      <th>10</th>
      <td>569.5</td>
      <td>502056</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1078</td>
    </tr>
    <tr>
      <th>11</th>
      <td>569.5</td>
      <td>502054</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1094</td>
    </tr>
    <tr>
      <th>12</th>
      <td>569.5</td>
      <td>502052</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>2452</td>
    </tr>
    <tr>
      <th>13</th>
      <td>569.5</td>
      <td>502050</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1091</td>
    </tr>
    <tr>
      <th>14</th>
      <td>569.5</td>
      <td>502047</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1065</td>
    </tr>
    <tr>
      <th>15</th>
      <td>569.5</td>
      <td>502045</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1090</td>
    </tr>
    <tr>
      <th>16</th>
      <td>569.5</td>
      <td>502043</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1082</td>
    </tr>
    <tr>
      <th>17</th>
      <td>569.5</td>
      <td>502041</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1073</td>
    </tr>
    <tr>
      <th>18</th>
      <td>569.5</td>
      <td>502039</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1052</td>
    </tr>
    <tr>
      <th>19</th>
      <td>569.5</td>
      <td>502036</td>
      <td>1</td>
      <td>7030268</td>
      <td>12344</td>
      <td>1055</td>
    </tr>
  </tbody>
</table>
</div>



We see many recent blocks (the highest `height`) with single simple transaction.

#### Conclusions
What this analysis of actual execution of Ergo Node v5.0 tell us:
- the new cost estimation is properly estimates the actual execution time of all the existing
blocks.
- the cost estimation is reasonably conservative, i.e. for 95% blocks it overestimates the actual
costs, but this overestimation is significantly lower than in v4.x
- for all blocks the estimated costs are more than 2.5 times lower than the cost limits.
- In the low boundary case shown above, the block which is the closest to the cost limit has 100+
transactions. With v5.0 this number could be higher
