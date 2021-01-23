unit GTA.Leetcode_1091;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

type
  TSolution = class
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
const
  X_DIRS: array[0..7] of integer = (1, 0, -1, -1, -1, 0, 1, 1);
  Y_DIRS: array[0..7] of integer = (1, 1, 1, 0, -1, -1, -1, 0);
var
  c, r, i, cur, curX, curY, nextX, nextY: integer;
  queue: TQueue_int;
  visited, dis: TArr2D_int;

  function __inArea__(x, y: integer): boolean;
  begin
    Result := (x >= 0) and (y >= 0) and (x < r) and (y < c);
  end;

begin
  r := Length(grid);
  c := Length(grid[0]);

  SetLength(dis, r, c);
  SetLength(visited, r, c);
  for i := 0 to High(visited) do
    TArrayUtils_int.FillArray(visited[i], -1);

  queue := TQueue_int.Create;
  try
    visited[0, 0] := 0;
    queue.EnQueue(0);
    dis[0, 0] := 1;

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;
      curX := cur div c;
      curY := cur mod c;

      for i := 0 to High(X_DIRS) do
      begin
        nextX := curX + X_DIRS[i];
        nextY := curY + Y_DIRS[i];

        if __inArea__(nextX, nextY)
          and (visited[nextX, nextY] = -1)
          and (grid[nextX, nextY] = 0) then
        begin
          queue.EnQueue(nextX * c + nextY);
          visited[0, 0] := 0;
          dis[nextX, nextY] := dis[curX, curY] + 1;

          if (nextX = r - 1) and (nextY = c - 1) then
            Exit(dis[nextX, nextY]);
        end;
      end;
    end;
  finally
    queue.Free;
  end;

  Result := -1;
end;

end.
