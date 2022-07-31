unit GTA.Leetcode_695;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils;

// 695. 岛屿的最大面积
type
  TSolution = class
  private const
    DIRS: TArr2D_int = ((-1, 0), (0, 1), (1, 0), (0, -1));

  private
    _c: integer;
    _r: integer;
    _visited: TArr2D_bool;
    _grid: TArr2D_int;

    function __inArea(x, y: integer): boolean;
    function __dfs(x, y: integer): integer;

  public
    function MaxAreaOfIsland(grid: TArr2D_int): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  a: TArr2D_int;
begin
  with TSolution.Create do
  begin
    a := [
      [0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
      [0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0],
      [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]];
    WriteLn(MaxAreaOfIsland(a));

    ///////////////////////////////////
    a := [[0, 0, 0, 0, 0, 0, 0, 0]];
    WriteLn(MaxAreaOfIsland(a));
    Free;
  end;
end;

{ TSolution }

function TSolution.MaxAreaOfIsland(grid: TArr2D_int): integer;
var
  res, i, j: integer;
begin
  if grid = nil then Exit(0);

  _r := Length(grid);
  if _r = 0 then Exit(0);

  _c := Length(grid[0]);
  if _c = 0 then Exit(0);

  _grid := grid;
  SetLength(_visited, _r, _c);
  res := 0;

  for i := 0 to High(grid) do
  begin
    for j := 0 to High(grid[i]) do
    begin
      if (_visited[i, j] = false) and (grid[i, j] = 1) then
        res := Max(res, __dfs(i, j));
    end;
  end;

  Result := res;
end;

function TSolution.__dfs(x, y: integer): integer;
var
  res, i, nextX, nextY: integer;
begin
  _visited[x, y] := true;
  res := 1;

  for i := 0 to High(DIRS) do
  begin
    nextX := x + DIRS[i, 0];
    nextY := y + DIRS[i, 1];

    if (__inArea(nextX, nextY))
      and (not _visited[nextX, nextY])
      and (_grid[nextX, nextY] = 1) then
    begin
      res += __dfs(nextX, nextY);
    end;
  end;

  Result := res;
end;

function TSolution.__inArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (y >= 0) and (x < _r) and (y < _c);
end;

end.
