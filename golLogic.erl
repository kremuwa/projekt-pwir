% Moduł liczący. To właśnie ten moduł będzie na węźle.

-module(golLogic).
-export([nodeStart/1]).

nodeStart(Parent) ->
	receive
		{[Head | Tail], From, To} ->
			%io:format("Prototyp tablicy wynikow:~n~w~n",[zeros2D(length(Head),length([Head | Tail]))]),
			%io:format("Otrzymalem fragment planszy od ~B do ~B:~n~w~n",[From, To, [Head | Tail]]),
			Result = computeRows([Head | Tail], zeros2D(length(Head),length([Head | Tail])), 1, length([Head | Tail])),
			Parent ! {
						Result,
						From,
						To
					},
			%io:format("Po obliczeniach, fragment planszy od ~B do ~B wyglad tak:~n~w~n",[From, To, Result]),
			nodeStart(Parent);
		exit ->
			ok
	end.

% Liczy argument dla kilku wierszy
%
% ResRows przechowuje wynik. Liczy dla wierszy z zakesu From-To.
% Przyjmuje Rows w postaci listy list. ResRows musi być listą list z zerami o tych samych wymiarach, co Rows.
computeRows(Rows, ResRows, From, To) -> 
if
	To == From-1 -> ResRows;
	true -> computeRows(Rows, 
			% Poprzednie wiersze
			if
				To > 1 -> lists:sublist(ResRows, 1, To-1);
				To == 1 -> []
			end
			
			++
			
			% Liczony wiersz
			[computeRow(
				% Aktualny wiersz
				lists:nth(To, Rows),
				
				% Wiersz nad
				if
					To == 1 -> zeros(length(lists:nth(To, Rows)));
					true -> lists:nth(To-1, Rows)
				end,
				
				% Wiersz pod
				if
					To == length(Rows) -> zeros(length(lists:nth(To, Rows)));
					true -> lists:nth(To+1, Rows)
				end,
				
				% Wiersz wynikowy
				zeros(length(lists:nth(To, Rows))),
				
				% Liczba
				length(lists:nth(To, Rows))
			)]
			
			++
			
			% Następne wiersze
			lists:sublist(ResRows, To+1, length(ResRows)),
		From, To-1)
end.

% Liczy wynik dla wiersza.
%
% Argument ResRow służy do przekazywania wyniku pomiędzy wywołaniami
% Argument Count służy do zliczania - na początku powinien być równy ilości komórek w wierszu
% Przyjmuje cztery listy - pierwszą z wierszem, kolejne dwie z wierszami nad i pod, ostatnia ma być wierszem z zerami o tej samej długości.
% Oprócz tego dodajemy liczbę elementów w wierszu - Count.
computeRow(Row, RowAbove, RowBelow, ResRow, Count) ->
case Count of
	0 -> ResRow;
	_ -> computeRow(Row, RowAbove, RowBelow,
			lists:append([
				lists:sublist(ResRow, 1, Count-1),
				[computeField(lists:nth(Count, Row),
					lists:append([
						
						% Elementy po lewej i prawej
						if
							Count > 1 -> lists:sublist(Row, Count-1, 1);  % Jeżeli Count == 1, elementu po lewej nie ma
							Count == 1 -> []
						end,
						lists:sublist(Row, Count+1, 1),  
						
						% Sąsiedzi od góry - musimy sprawdzić, czy element na górze po lewej istnieje
						lists:sublist(RowAbove,
							if
								Count > 1 -> Count-1;
								Count == 1 -> Count
							end,
							if
								Count > 1 -> 3;
								Count == 1 -> 2
							end),
						
						% Sąsiedzi od dołu - musimy sprawdzić, czy element na dole po lewej istnieje
						lists:sublist(RowBelow,
							if
								Count > 1 -> Count-1;
								Count == 1 -> Count
							end,
							if
								Count > 1 -> 3;
								Count == 1 -> 2
							end)
					]))],
				lists:sublist(ResRow, Count+1, length(ResRow))
			]),
			Count-1)
end.

% Liczy wynik dla pola
computeField(Value, Neighbors) ->
	% io:format("Sasiedzi: ~n~B~n~B~n~B~n~B~n~B~n", [lists:nth(1,Neighbors),lists:nth(2,Neighbors),lists:nth(3,Neighbors),lists:nth(4,Neighbors),lists:nth(5,Neighbors)]),
	
if
	Value == 0 -> case lists:sum(Neighbors) of
		3 -> 1;
		_ -> 0
	end;
	
	Value == 1 -> case lists:sum(Neighbors) of
		2 -> 1;
		3 -> 1;
		_ -> 0
	end
end.

% Funkcja pomocnicza - tworzy listę z zerami
zeros(Number) -> 
if
	Number == 0 -> [];
	Number > 0 -> zeros([0], Number-1)
end.

zeros(List, Number) ->
if
	Number == 0 -> List;
	Number > 0 -> zeros(lists:append(List, [0]), Number-1)
end.

zeros2D(Number,Count) ->
	zeros2D([], Number, Count).
	
zeros2D(List, Number, Count) ->
if
	Count == 0 -> List;
	Count > 0 -> zeros2D(List ++ [zeros(Number)], Number, Count-1)
end.
