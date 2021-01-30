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
    XY_DIRS: TArr2D_int = ((0, -1), (1, 0), (0, 1), (-1, 0));

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
    WriteLn(SlidingPuzzle(board));

    board := [[1, 2, 3], [5, 4, 0]];
    WriteLn(SlidingPuzzle(board));

    board := [[4, 1, 2], [5, 0, 3]];
    WriteLn(SlidingPuzzle(board));

    board := [[3, 2, 4], [1, 5, 0]];
    WriteLn(SlidingPuzzle(board));

    Free;
  end;
end;

{ TSolution }

function TSolution.SlidingPuzzle(board: TArr2D_int): integer;
var
  queue: IQueue_str;
  visited: IMap_str_int;
  initalString, cur: UString;
  nextlist: IList_str;
  i: integer;
begin
  initalString := __BoardToString(board);
  if initalString = '123450' then Exit(0);

  queue := TQueue_str.Create;
  visited := THashMap_str_int.Create;

  queue.EnQueue(initalString);
  visited.Add(initalString, 0);

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;
    nextList := __GetNextList(cur);

    for i := 0 to nextlist.Count - 1 do
    begin
      if not visited.ContainsKey(nextlist[i]) then
      begin
        queue.EnQueue(nextlist[i]);
        visited.Add(nextlist[i], visited[cur] + 1);

        if nextlist[i] = '123450' then
          Exit(visited[nextlist[i]]);
      end;
    end;
  end;

  Result := -1;
end;

function TSolution.__BoardToString(board: TArr2D_int): UString;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    for i := 0 to High(board) do
      for j := 0 to High(board[i]) do
        sb.Append(board[i, j]);

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TSolution.__GetNextList(str: UString): IList_str;
var
  res: IList_str;
  curBoard, temp: TArr2D_int;
  i, x, y, nextX, nextY: integer;
begin
  for i := 0 to str.Length - 1 do
    if str.Chars[i] = '0' then
    begin
      x := i div 3;
      y := i mod 3;
      Break;
    end;

  res := TArrayList_str.Create;
  curBoard := __StringToBoard(str);

  for i := 0 to High(XY_DIRS) do
  begin
    nextX := x + XY_DIRS[i, 0];
    nextY := y + XY_DIRS[i, 1];
    temp := TArrayUtils_int.CopyArray2D(curBoard);

    if __InArea(nextX, nextY) then
    begin
      TUtils_int.Swap(temp[x, y], temp[nextX, nextY]);
      res.AddLast(__BoardToString(temp));
    end;
  end;

  Result := res;
end;

function TSolution.__InArea(x, y: integer): boolean;
begin
  Result := (x in [0..1]) and (y in [0..2]);
end;

function TSolution.__StringToBoard(str: UString): TArr2D_int;
var
  res: TArr2D_int;
  i: integer;
begin
  SetLength(res, 2, 3);

  for i := 0 to str.Length - 1 do
    res[i div 3, i mod 3] := Ord(str.Chars[i]) - Ord('0');

  Result := res;
end;

end.
