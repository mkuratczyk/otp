-module(performance_benchmark).

%% Performance benchmark for ets:lookup_elements
%% Simulates RabbitMQ use case: 20-field records, extracting 3 fields

-compile(export_all).

main() ->
    io:format("~n=== ETS lookup_elements Performance Benchmark ===~n"),
    io:format("Scenario: 20-field tuples, extracting 3 fields~n~n"),
    
    %% Setup
    NumRecords = 100000,
    NumIterations = 100000,
    
    io:format("Setup:~n"),
    io:format("  - Table size: ~p records~n", [NumRecords]),
    io:format("  - Iterations: ~p lookups~n", [NumIterations]),
    io:format("  - Tuple size: 21 elements (key + 20 fields)~n"),
    io:format("  - Field size: 10 bytes each~n"),
    io:format("  - Fields to extract: 3 (positions 7, 14, 19)~n~n"),
    
    %% Create table and populate with realistic data
    T = ets:new(benchmark, [set]),
    populate_table(T, NumRecords),
    
    io:format("Running benchmarks...~n~n"),
    
    %% Method 1: Three separate lookup_element calls
    Time1 = benchmark_three_lookup_element(T, NumIterations),
    
    %% Method 2: Single lookup_elements call
    Time2 = benchmark_lookup_elements(T, NumIterations),
    
    %% Method 3: One lookup + three element extractions
    Time3 = benchmark_lookup_plus_element(T, NumIterations),
    
    %% Cleanup
    ets:delete(T),
    
    %% Results
    io:format("~n=== Results ===~n~n"),
    io:format("Method 1: 3× lookup_element/3 calls~n"),
    io:format("  Time: ~p µs~n", [Time1]),
    io:format("  Per lookup: ~.2f µs~n~n", [Time1 / NumIterations]),
    
    io:format("Method 2: 1× lookup_elements/3 call~n"),
    io:format("  Time: ~p µs~n", [Time2]),
    io:format("  Per lookup: ~.2f µs~n", [Time2 / NumIterations]),
    io:format("  Speedup vs Method 1: ~.2fx~n~n", [Time1 / Time2]),
    
    io:format("Method 3: 1× lookup/2 + 3× element/2 calls~n"),
    io:format("  Time: ~p µs~n", [Time3]),
    io:format("  Per lookup: ~.2f µs~n", [Time3 / NumIterations]),
    io:format("  Speedup vs Method 3: ~.2fx~n~n", [Time3 / Time2]),
    
    %% Summary
    io:format("=== Summary ===~n~n"),
    
    Best = lists:min([Time1, Time2, Time3]),
    
    io:format("Winner: "),
    case Best of
        Time2 -> 
            io:format("lookup_elements/3 ✓~n"),
            io:format("  ~.2fx faster than 3× lookup_element~n", [Time1/Time2]),
            io:format("  ~.2fx faster than lookup + element~n~n", [Time3/Time2]);
        Time1 -> 
            io:format("3× lookup_element (unexpected)~n~n");
        Time3 -> 
            io:format("lookup + element (unexpected)~n~n")
    end,
    
    io:format("Key Benefits of lookup_elements:~n"),
    io:format("  ✓ Single key hash (vs 3 hashes)~n"),
    io:format("  ✓ Single lock acquisition (vs 3 locks)~n"),
    io:format("  ✓ Single bucket traversal (vs 3 traversals)~n"),
    io:format("  ✓ Only copies needed fields (vs entire 21-field tuple)~n~n"),
    
    ok.

%% Populate table with realistic 20-field records
populate_table(T, NumRecords) ->
    lists:foreach(fun(N) ->
        %% Create a tuple with key + 20 fields of 10 bytes each
        Tuple = {{N, {resource, <<"%2F">>, queue, <<"some long queue name below 64 bytes">>}},
                 make_field(1), make_field(2), make_field(3), make_field(4),
                 make_field(5), make_field(6), make_field(7), make_field(8),
                 make_field(9), make_field(10), make_field(11), make_field(12),
                 make_field(13), make_field(14), make_field(15), make_field(16),
                 make_field(17), make_field(18), make_field(19), make_field(20)},
        ets:insert(T, Tuple)
    end, lists:seq(1, NumRecords)).

%% Create a 10-byte binary field
make_field(N) ->
    <<N:8, "field_", N:8, N:8, N:8>>.

%% Benchmark Method 1: Three separate lookup_element calls
benchmark_three_lookup_element(T, NumIterations) ->
    io:format("  [1/3] Testing 3× lookup_element/3..."),
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = {(N rem 10000) + 1, {resource, <<"%2F">>, queue, <<"some long queue name below 64 bytes">>}},
        _Field7 = ets:lookup_element(T, Key, 7),
        _Field14 = ets:lookup_element(T, Key, 14),
        _Field19 = ets:lookup_element(T, Key, 19),
        ok
    end, lists:seq(1, NumIterations)),
    
    End = erlang:monotonic_time(microsecond),
    Time = End - Start,
    io:format(" ~p µs~n", [Time]),
    Time.

%% Benchmark Method 2: Single lookup_elements call
benchmark_lookup_elements(T, NumIterations) ->
    io:format("  [2/3] Testing 1× lookup_elements/3..."),
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = {(N rem 10000) + 1, {resource, <<"%2F">>, queue, <<"some long queue name below 64 bytes">>}},
        _Fields = ets:lookup_elements(T, Key, [7, 14, 19]),
        ok
    end, lists:seq(1, NumIterations)),
    
    End = erlang:monotonic_time(microsecond),
    Time = End - Start,
    io:format(" ~p µs~n", [Time]),
    Time.

%% Benchmark Method 3: One lookup + three element extractions
benchmark_lookup_plus_element(T, NumIterations) ->
    io:format("  [3/3] Testing 1× lookup/2 + 3× element/2..."),
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(N) ->
        Key = {(N rem 10000) + 1, {resource, <<"%2F">>, queue, <<"some long queue name below 64 bytes">>}},
        [Tuple] = ets:lookup(T, Key),
        _Field7 = element(7, Tuple),
        _Field14 = element(14, Tuple),
        _Field19 = element(19, Tuple),
        ok
    end, lists:seq(1, NumIterations)),
    
    End = erlang:monotonic_time(microsecond),
    Time = End - Start,
    io:format(" ~p µs~n", [Time]),
    Time.
