unit GTA.Leetcode_1091;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

type
  TSolution = class
  private const
    X_DIRS: array[0..7] of integer = (1, 0, -1, -1, -1, 0, 1, 1);
    Y_DIRS: array[0..7] of integer = (1, 1, 1, 0, -1, -1, -1, 0);

  private
    _c: integer;
    _r: integer;
    _visited: TArr2D_int;

    function __inArea(x, y: integer): boolean;


  public
    function ShortestPathBinaryMatrix(grid: TArr2D_int): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  a: TArr2D_int;
begin
  with TSolution.Create do
  begin
    a := [[0, 1], [1, 0]];
    WriteLn(ShortestPathBinaryMatrix(a));

    ///////////////////////////////////
    a := [[0, 0, 0], [1, 1, 0], [1, 1, 0]];
    WriteLn(ShortestPathBinaryMatrix(a));
    Free;
  end;
end;

{ TSolution }

function TSolution.ShortestPathBinaryMatrix(grid: TArr2D_int): integer;
var
  i, cur, path, curX, curY, nextX, nextY: integer;
  queue: TQueue_int;
begin
  _r := Length(grid);
  _c := Length(grid[0]);

  SetLength(_visited, _r, _c);
  for i := 0 to High(_visited) do
    TArrayUtils_int.FillArray(_visited[i], -1);

  queue := TQueue_int.Create;
  try
    _visited[0, 0] := 0;
    queue.EnQueue(0);
    path := 1;

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;
      curX := cur div _c;
      curY := cur mod _c;

      for i := 0 to High(X_DIRS) do
      begin
        nextX := curX + X_DIRS[i];
        nextY := curY + Y_DIRS[i];

        if __inArea(nextX, nextY)
          and (_visited[nextX, nextY] = -1)
          and (grid[nextX, nextY] = 0) then
        begin
          queue.EnQueue(nextX * _c + nextY);
          _visited[0, 0] := 0;
          path += 1;

          if (nextX = _r - 1) and (nextY = _c - 1) then
            Exit(path);
        end;
      end;
    end;
  finally
    queue.Free;
  end;

  Result := -1;
end;

function TSolution.__inArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (y >= 0) and (x < _r) and (y < _c);
end;

end.
