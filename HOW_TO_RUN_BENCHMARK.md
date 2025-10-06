# How to Run the Performance Benchmark

## Quick Start

```bash
cd /Users/mkuratczyk/workspace/otp
./bin/escript performance_benchmark.erl
```

## What It Tests

The benchmark simulates the **RabbitMQ use case** from issue #10211:

- **21-field tuples** (1 key + 20 data fields)
- **10 bytes per field** (realistic data size)
- **Extract 3 fields** from positions 7, 14, and 19
- **10,000 iterations** on a table with 10,000 records

## Three Methods Compared

### Method 1: Three `lookup_element/3` calls
```erlang
Field7 = ets:lookup_element(T, Key, 7),
Field14 = ets:lookup_element(T, Key, 14),
Field19 = ets:lookup_element(T, Key, 19)
```
- Hashes the key 3 times
- Acquires lock 3 times
- Traverses bucket 3 times

### Method 2: One `lookup_elements/3` call ⭐
```erlang
{Field7, Field14, Field19} = ets:lookup_elements(T, Key, [7, 14, 19])
```
- Hashes the key once
- Acquires lock once
- Traverses bucket once
- **WINNER: 1.07× faster than Method 1, 1.40× faster than Method 3**

### Method 3: `lookup/2` + `element/2` calls
```erlang
[Tuple] = ets:lookup(T, Key),
Field7 = element(7, Tuple),
Field14 = element(14, Tuple),
Field19 = element(19, Tuple)
```
- Copies entire 21-field tuple (210 bytes)
- Only needs 3 fields (30 bytes)
- 86% memory waste

## Expected Output

```
=== ETS lookup_elements Performance Benchmark ===
Scenario: 20-field tuples, extracting 3 fields

Setup:
  - Table size: 10000 records
  - Iterations: 10000 lookups
  - Tuple size: 21 elements (key + 20 fields)
  - Field size: 10 bytes each
  - Fields to extract: 3 (positions 7, 14, 19)

Running benchmarks...

  [1/3] Testing 3× lookup_element/3... 1065 µs
  [2/3] Testing 1× lookup_elements/3... 994 µs
  [3/3] Testing 1× lookup/2 + 3× element/2... 1388 µs

=== Results ===

Method 1: 3× lookup_element/3 calls
  Time: 1065 µs
  Per lookup: 0.11 µs

Method 2: 1× lookup_elements/3 call
  Time: 994 µs
  Per lookup: 0.10 µs
  Speedup vs Method 1: 1.07x

Method 3: 1× lookup/2 + 3× element/2 calls
  Time: 1388 µs
  Per lookup: 0.14 µs
  Speedup vs Method 3: 1.40x

=== Summary ===

Winner: lookup_elements/3 ✓
  1.07x faster than 3× lookup_element
  1.40x faster than lookup + element

Key Benefits of lookup_elements:
  ✓ Single key hash (vs 3 hashes)
  ✓ Single lock acquisition (vs 3 locks)
  ✓ Single bucket traversal (vs 3 traversals)
  ✓ Only copies needed fields (vs entire 21-field tuple)
```

## Running from Erlang Shell

```erlang
%% Start Erlang
cd /Users/mkuratczyk/workspace/otp
./bin/erl

%% Compile and run
c(performance_benchmark).
performance_benchmark:main([]).
```

## Customizing the Benchmark

Edit `performance_benchmark.erl` to change:

```erlang
%% Line 11-12: Adjust test parameters
NumRecords = 10000,      %% Table size
NumIterations = 10000,   %% Number of lookups

%% Line 43-48: Change tuple structure
%% Currently: 21 fields (key + 20 data fields)
%% Modify make_field/1 to change field size

%% Line 83, 98, 113: Change positions to extract
%% Currently: positions 7, 14, 19
%% Change to any positions you want to test
```

## Interpreting Results

### Good Performance Indicators
- ✅ `lookup_elements` faster than both alternatives
- ✅ Speedup increases with more fields extracted
- ✅ Speedup increases with larger tuples

### What Affects Performance
1. **Number of fields extracted:** More fields = bigger advantage
2. **Tuple size:** Larger tuples = more benefit vs `lookup`
3. **Table size:** Scales linearly for all methods
4. **System load:** Run on idle system for consistent results

## Scaling to Production

For **100,000 lookups/second** (typical RabbitMQ):
- Method 1: ~10.65 ms CPU/sec
- **Method 2: ~9.94 ms CPU/sec** ✓
- Method 3: ~13.88 ms CPU/sec

For **1,000,000 lookups/second** (high load):
- Method 1: ~106.5 ms CPU/sec
- **Method 2: ~99.4 ms CPU/sec** ✓
- Method 3: ~138.8 ms CPU/sec

**Savings:** 7-40% CPU reduction depending on alternative

## Troubleshooting

### Benchmark runs too fast
Increase `NumIterations`:
```erlang
NumIterations = 100000,  %% 10x more iterations
```

### Want more realistic data
Modify `make_field/1` to use your data structure:
```erlang
make_field(N) ->
    %% Your custom data here
    {complex, structure, with, N, fields}.
```

### Test different positions
Change the positions in all three benchmark functions:
```erlang
%% Instead of [7, 14, 19], try:
ets:lookup_elements(T, Key, [2, 10, 20])  %% Different positions
ets:lookup_elements(T, Key, [2, 3, 4, 5]) %% More fields
```

## See Also

- **PERFORMANCE_ANALYSIS.md** - Detailed analysis and scaling projections
- **TEST_RESULTS.md** - Functional test results
- **FINAL_SUMMARY.md** - Complete implementation overview