
-module(esqlite_bench).

-export([main/0]).

main() ->
    {ok, Conn} = esqlite3:open(":memory:"),

    [] = esqlite3:q("create table bench (a int, b int, c int, d int, e int, f int, g int, h int, i int, j int)", Conn),

    Values = lists:seq(1, 10),
    Range = lists:seq(0, 100_000),

    %% Insert 100_000 records
    {Time, _} = timer:tc(fun() ->
                                 ok = esqlite3:exec("begin;", Conn),
                                 {ok, Stmt} = esqlite3:prepare("insert into bench values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning rowid;", Conn),
                                 lists:foreach(fun(_) ->
                                                       ok = esqlite3:bind(Stmt, Values),
                                                       {row, {_}} = esqlite3:step(Stmt)
                                               end,
                                               Range),

                                 %% make sure to step the statement until a $done is returned,
                                 %% otherwise it is not possible to comit the transaction.
                                 '$done' = esqlite3:step(Stmt),

                                 ok = esqlite3:exec("commit;", Conn)
                         end),

    erlang:display(commit),
    io:fwrite("100_000 inserts took: ~p milliseconds~n", [Time/1000]),


    %% Select 100_000 
    {SelectTime, _} = timer:tc(fun() ->
                                       esqlite3:q("select * from bench;", Conn)
                               end),
    
    io:fwrite("100_000 select took: ~p milliseconds~n", [SelectTime/1000]),

    ok.


