-module(golIO).
-export([loadBoard/1, generateRandomBoard/2, printBoard/2]).

%% ******************************************************************************
%% input z pliku (na bazie laboratorium)
%% ******************************************************************************
			
%% procedura odczytuj¹ca plansze z pliku,
%% zwraca liste wierszy, z ktorych kazdy jest lista 0 i 1

loadBoard(FileName) ->
	{FD,Size} = openFile(FileName),
	Len = trunc(math:pow(2,Size)), 	% rozmiar boku planszy
	io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
	Board = getData(FD,Len,Len,[]),
	file:close(FD),
	Board.
		
%% otwarcie pliku do wczytywania
%% zwracany jest deskryptor pliku i rozmiar danych/planszy	

openFile(FileName) ->
	{ok,FD} = file:open(FileName,[read,compressed]),
	case file:read(FD,1) of 
		{ok,[Data]} -> {FD,Data};
		eof -> io:format("~nKoniec~n",[]);
		{error,Reason} -> io:format("~s~n",[Reason])
	end.

%% odczytywanie wiersz po wierszu
		
getData(_FD,_Len,0,Board) -> Board;
getData(FD,Len,Count,Board) ->
	Row = readRow(FD,Len),
	getData(FD,Len,Count-1,Board ++ [[Cell - 48 || Cell <- Row]]).
		
%% odczytanie jednego wiersza o zadanym rozmiarze

readRow(FD,Length) -> 
	case file:read(FD,Length) of 
		{ok,Data} -> Data;
		eof -> io:format("~nKoniec~n",[]);
		{error,Reason} -> io:format("~s~n",[Reason])
	end.

		
%% ******************************************************************************		
%% output do pliku (na bazie laboratorium)	
%% ******************************************************************************			
		
%% procedura testowa, zapisujaca wskazana plansze w pliku o podanej nazwie

% boardSave(FileName, Board, Size) ->
	% {ok,FD} = file:open(FileName,[write,compressed]),  % otwarcie/utworzenie pliku
	% file:write(FD,Size),
	% % file:write(FD,[Size]),?
	% Len = trunc(math:pow(2,Size)),
	% feedData(FD,Board,Len,Len),
	% file:close(FD).
		
% %% zapisanie jednego wiersza

% writeData(FD,Data) ->
	% file:write(FD,Data).

% %% zapisanie planszy do pliku o podanym deskryptorze
	
% feedData(_FD,0,_Len)-> ok;
% feedData(FD,Count,Len) ->
	% Data = [random:uniform(2)+47 || _ <- lists:seq(1, Len)],
	% writeData(FD,Data),
	% feedData(FD,Count-1,Len).

	
%% ******************************************************************************		
%% generowanie losowej planszy (na bazie laboratorium)
%% ******************************************************************************	
	
generateRandomBoard(FileName, Size) ->
	{ok,FD} = file:open(FileName,[write,compressed]),  % otwarcie/utworzenie pliku
	% file:write(FD, Size), to chyba nie potrzene?
	file:write(FD, [Size]),
	random:seed(now()),
	file:write(FD, [random:uniform(2) + 47 || _ <- lists:seq(1, trunc(math:pow(2,Size*2)))]),
	file:close(FD).
	
%% ******************************************************************************		
%% output na ekran - pomocniczo, do weryfikacji poprawnosci
%% ******************************************************************************	

printBoard(Board, Len) when Len > length(Board) ->
	ok;
printBoard(Board, Len) ->
	io:format([Elem + 48 || Elem <- lists:nth(Len, Board)]),
	io:format("~n"),
	printBoard(Board, Len + 1).