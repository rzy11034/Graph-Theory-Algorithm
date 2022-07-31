unit GTA.UniquePathsIII_Memory_Search;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  // Leetcode 980. 不同路径 III
  // 1 表示起始方格。且只有一个起始方格。
  // 2 表示结束方格，且只有一个结束方格。
  // 0 表示我们可以走过的空方格。
  // -1 表示我们无法跨越的障碍。
  TSolution = class(TObject)
  private
    _G: TArr2D_int;
    _R: integer;
    _C: integer;
    _Start: integer;
    _End: integer;
    _Pre: TArr2D_int;
    _Left: integer;
    _Memo: TArr2D_int;

    function __Dfs(visited, v, parent, left: integer): integer;
    function __InArea(x, y: integer): boolean;
    procedure __AllPath;

  public
    function UniquePathsIII(gird: TArr2D_int): integer;

  end;

procedure Main;

implementation

procedure Main;
var
  a: integer;
begin
  //示例 1：
  //输入：[[1,0,0,0],[0,0,0,0],[0,0,2,-1]]
  //输出：2
  //解释：我们有以下两条路径：
  //1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2)
  //2. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2)
  with TSolution.Create do
  begin
    a := UniquePathsIII([[1, 0, 0, 0], [0, 0, 0, 0], [0, 0, 2, -1]]);
    WriteLn(a);
    Free;
  end;
  DrawLineBlockEnd;

  //示例 2：
  //输入：[[1,0,0,0],[0,0,0,0],[0,0,0,2]]
  //输出：4
  //解释：我们有以下四条路径：
  //1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2),(2,3)
  //2. (0,0),(0,1),(1,1),(1,0),(2,0),(2,1),(2,2),(1,2),(0,2),(0,3),(1,3),(2,3)
  //3. (0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(1,1),(0,1),(0,2),(0,3),(1,3),(2,3)
  //4. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2),(2,3)
  with TSolution.Create do
  begin
    a := UniquePathsIII([[1, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 2]]);
    WriteLn(a);
    Free;
  end;
  DrawLineBlockEnd;

  //示例 3：
  //输入：[[0,1],[2,0]]
  //输出：0
  //解释：
  //没有一条路能完全穿过每一个空的方格一次。
  //请注意，起始和结束方格可以位于网格中的任意位置。
  with TSolution.Create do
  begin
    a := UniquePathsIII([[0, 1], [2, 0]]);
    WriteLn(a);
    Free;
  end;
end;

{ TSolution }
function TSolution.UniquePathsIII(gird: TArr2D_int): integer;
var
  j, i, Visited: integer;
begin
  _G := gird;
  _R := Length(gird);
  _C := Length(gird[0]);
  _Left := _R * _C;

  Visited := 0;
  SetLength(_Pre, _R, _C);
  for i := 0 to High(_Pre) do
    TArrayUtils_int.FillArray(_Pre[i], -1);

  SetLength(_Memo, 1 shl (_R * _C), _R * _C);
  for i := 0 to High(_Memo) do
    TArrayUtils_int.FillArray(_Memo[i], -1);

  for i := 0 to _R - 1 do
    for j := 0 to _C - 1 do
      case gird[i, j] of
        1: _Start := i * _C + j;
        2: _End := i * _C + j;
        -1: _Left -= 1;
      end;

  Result := __Dfs(Visited, _Start, _Start, _Left);
end;

procedure TSolution.__AllPath;
  procedure __GetXY__(const temp: integer; var curX, curY: integer);
  begin
    curX := temp div _C;
    curY := temp mod _C;
  end;

var
  curX, curY, temp, i: integer;
  list: IList_int;
begin
  temp := _End;
  list := TArrayList_int.Create;
  __GetXY__(temp, curX, curY);

  while _Pre[curX, curY] <> _Start do
  begin
    list.AddLast(temp);
    curX := temp div _C;
    curY := temp mod _C;
    temp := _Pre[curX, curY];
  end;
  list.AddLast(_Start);

  list.Reverse;

  //TArrayUtils_int.Print(list.ToArray);

  __GetXY__(list[0], curX, curY);
  Write('(', curX, ',', curY, ')');
  for i := 1 to list.Count - 1 do
  begin
    __GetXY__(list[i], curX, curY);
    Write(', (', curX, ',', curY, ')');
  end;
  WriteLn;
end;

function TSolution.__Dfs(visited, v, parent, left: integer): integer;
var
  curX, curY, nextX, nextY, Next, i: integer;
begin
  if _Memo[visited, v] <> -1 then
    Exit(_Memo[visited, v]);

  curX := v div _C;
  curY := v mod _C;

  visited += 1 shl v;
  _Pre[curX, curY] := parent;
  left -= 1;

  if (left = 0) and (v = _End) then
  begin
    visited -= 1 shl v;
    _Memo[visited, v] := 1;
    __AllPath;
    Exit(1);
  end;

  Result := 0;
  for i := 0 to High(XY_DIRS_4) do
  begin
    nextX := curX + XY_DIRS_4[i, 0];
    nextY := curY + XY_DIRS_4[i, 1];
    Next := nextX * _C + nextY;

    if __InArea(nextX, nextY)
      and (visited and (1 shl Next) = 0)
      and (_G[nextX, nextY] <> -1) then
    begin
      Result += __Dfs(visited, Next, v, left);
    end;
  end;

  visited -= 1 shl v;
  _Memo[visited, v] := Result;
end;

function TSolution.__InArea(x, y: integer): boolean;
begin
  Result := (x in [0.._R - 1]) and (y in [0.._C - 1]);
end;

end.
