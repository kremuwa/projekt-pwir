-module(golMain).
-export([main/2, collectResults/3]).

%% ******************************************************************************
%% glowna procedura, nalezy ja wywolac po podlaczeniu node'ow i przeslaniu 
%% im skompilowanego kodu: nl(golMain) i ew. wygenerowaniu losowej planszy
%% ******************************************************************************

main(FileName,IterNum) ->
	Nodes = length(nodes()),
	if
        Nodes == 0 ->
            io:format("Najpierw podlacz node'y i przeslij do nich kod poleceniem nl!~n");
        true ->
			Board = golIO:loadBoard(FileName),
			Pids = prepareNodes(),
			%golIO:printBoard(Board, 1),
			io:format("Nodes: ~b~n", [length(Pids)]),
			{NewBoard, L} = gameLoop(Board,Pids,IterNum,[]),
			Length = length(L),
			Min = lists:min(L),
			Max = lists:max(L),
			Med = lists:nth(round((Length / 2)), lists:sort(L)),
			Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
			io:format("Range: ~b - ~b mics~n"
				  "Median: ~b mics~n"
				  "Average: ~b mics~n",
				  [Min, Max, Med, Avg]),
			Med
			%golIO:printBoard(NewBoard, 1)
    end.

%% stworzenie procesow dla przefiltrowanych wezlow
	
prepareNodes() ->
	Nodes = filterNodes(nodes()),
	prepareNodes(Nodes,[]).
prepareNodes([], Pids) ->
	Pids;
prepareNodes([Head | Tail],Pids) ->
	Parent = self(),
	prepareNodes(Tail,Pids ++ [spawn(Head, golLogic, nodeStart, [Parent])]).

%% przefiltrowanie wezlow i pozostawienie tylko tych z "l@le" w nazwie
	
filterNodes(Nodelist) ->
	filterNodes(Nodelist, []).
filterNodes([], Res) ->
	Res;
filterNodes(Nodelist,Res) -> 
	[Head|Tail] = Nodelist,
	[HeadString] = io_lib:format("~p",[Head]),
	Pos = string:str(HeadString, "l@le"),
	if 
		Pos > 0 -> 
			filterNodes(Tail,Res ++ [Head]);
		true ->
			filterNodes(Tail,Res)
	end.
	
%% petla w ktorej wysylamy fragmenty Board do wezlow i odbieramy ich policzone wersje
	
gameLoop(Board, _Pids, 0, Times) ->
	{Board, Times};
gameLoop(Board, Pids, IterNum, Times) ->
	sendData(Board, Pids),
    {T, NewBoard} = timer:tc(golMain, collectResults, [Board, length(Board), length(Pids)]),
	gameLoop(NewBoard, Pids, IterNum - 1, [T | Times]).
	
%% rozeslanie wlasciwych czesci Board do wezlow
	
sendData(Board, Pids) ->
	sendData(Board, length(Board), Pids, length(Pids), 1, 0).
sendData(_Board, _BoardLen, _Pids, PidsLen, PidIterator, _CurAdded) when PidIterator == PidsLen + 1 ->
	ok;
sendData(Board, BoardLen, Pids, PidsLen, PidIterator, CurAdded) ->
	Div = BoardLen div PidsLen,		% ile co najmniej wierszy na wezel
	Rem = BoardLen rem PidsLen,
	
	if
		PidIterator =< Rem ->
			Adding = 1,
			NextAdded = CurAdded + 1;
		true ->
			Adding = 0,
			NextAdded = CurAdded
	end,
	
	if
		PidIterator == 1 ->
			From = 1,
			To = Div + Adding + 1;
		PidIterator == PidsLen ->
			From = CurAdded + (PidIterator - 1) * Div,
			To = BoardLen;
		true ->
			From = CurAdded + (PidIterator - 1) * Div,
			To = CurAdded + PidIterator * Div + Adding + 1
	end,
	lists:nth(PidIterator, Pids) ! {lists:sublist(Board, From, To - From + 1), From, To},
	sendData(Board, BoardLen, Pids, PidsLen, PidIterator + 1, NextAdded).
	
%% odebranie odpowiedzi od wezlow

collectResults(Board, _Len, 0) ->
	Board;
collectResults(Board, Len, NodesCnt) ->
	receive
		{BoardPart, From, To} ->	
			%io:format("Otrzymalem odpowiedz z fragmentem planszy od ~B do ~B~n",[From,To]),
			if
				(From == 1) and (To == Len) ->
					NewBoard = Board;
				From == 1 ->
					NewBoard = 
					lists:sublist(BoardPart, 1, length(BoardPart) - 1)
					++
					lists:sublist(Board, To, length(Board) - length(BoardPart) + 1);
				To == Len ->
					NewBoard = 
					lists:sublist(Board, 1, From)
					++
					lists:sublist(BoardPart, 2, length(BoardPart) - 1);
				true ->
					NewBoard = 
					lists:sublist(Board, 1, From)
					++
					lists:sublist(BoardPart, 2, length(BoardPart) - 2)
					++
					lists:sublist(Board, To, Len)
			end,
		
			collectResults(NewBoard, Len, NodesCnt - 1) 
	end.