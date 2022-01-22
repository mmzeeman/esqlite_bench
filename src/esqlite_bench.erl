
-module(esqlite_bench).

-export([main/0]).

main() ->
    {ok, Conn} = esqlite3:open(":memory:"),

    [] = esqlite3:q("create table bench (a int, b int, c int, d int, e int, f int, g int, h int, i int, j int)", Conn),

    Values = lists:seq(1, 10),
    Range = lists:seq(1, 1_000_000),

    %% Insert 1_000_000 records
    {Time, _} = timer:tc(fun() ->
                                 ok = esqlite3:exec("begin;", Conn),

                                 {ok, Stmt} = esqlite3:prepare("insert into bench values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?);", Conn),
                                 lists:foreach(fun(_) ->
                                                       ok = esqlite3:bind(Stmt, Values),
                                                       '$done' = esqlite3:step(Stmt)
                                               end,
                                               Range),

                                 ok = esqlite3:exec("commit;", Conn)
                         end),

    io:fwrite("1_000_000 inserts took: ~p milliseconds~n", [Time/1000]),

    %% Select 1_000_000 
    {SelectTime, Rows} = timer:tc(fun() ->
                                          esqlite3:q("select * from bench;", Conn)
                                  end),
    
    io:fwrite("Select of ~p rows took: ~p milliseconds~n", [length(Rows), SelectTime/1000]),

    ok.


