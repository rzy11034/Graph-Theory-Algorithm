unit GTA.SlidingPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.UString;

type
  TSolution = class(TObject)
  private const
    X_DIRS: TArr_int = (0, 1, 0, -1);
    Y_DIRS: TArr_int = (-1, 0, 1, 0);

  private
    function __InArea(x, y: integer): boolean;
    function __BoardToString(board: TArr2D_int): UString;
    function __StringToBoard(str: UString): TArr2D_int;
    function __GetNextList(str: UString): IList_str;

  public
    function SlidingPuzzle(board: TArr2D_int): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  board: TArr2D_int;
begin
  with TSolution.Create do
  begin
    board := [[1, 2, 3], [4, 0, 5]];

    board := [[1, 2, 3], [5, 4, 0]];

    board := [[4, 1, 2], [5, 0, 3]];
  end;
end;

{ TSolution }

function TSolution.SlidingPuzzle(board: TArr2D_int): integer;
var
  queue: IQueue_str;
  visited: IMap_str_int;
  initalString, cur: UString;
  nextlist: IList_str;
begin
  initalString := __BoardToString__(board);
  if initalString = '123450' then Exit(0);

  queue := TQueue_str.Create;
  visited := THashMap_str_int.Create;

  queue.EnQueue(initalString);
  visited.Add(initalString, 0);

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;

    nextList := __GetNextList__(cur);
  end;
end;

function TSolution.__BoardToString(board: TArr2D_int): UString;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    for i := 0 to High(board) do
      for j := 0 to High(board) do
        sb.Append(board[i, j]);

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TSolution.__GetNextList(str: UString): IList_str;
var
  res:IList_str;
  curBoard: TArr2D_int;
  i,x,y: Integer;
begin
  res := TList_str.Create;
  curBoard := __StringToBoard(str);

  for i := 0 to high(X_DIRS) do
  begin
    x :=
  end;
end;

function TSolution.__InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < 2) and (y >= 0) and (y < 3);
end;

function TSolution.__StringToBoard(str: UString): TArr2D_int;
var
  res: TArr2D_int;
  n, i: integer;
begin
  SetLength(res, 2, 3);
  n := str.Length;

  for i := 0 to n - 1 do
    res[i div n, i mod n] := Ord(str.Chars[i]) - Ord('0');

  Result := res;
end;

end.
