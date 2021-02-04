unit GTA.UniquePathsIII;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils, DeepStar.DSA.Linear.ArrayList,
  GTA.Utils;

type
  // Leetcode 980. 不同路径 III
  // 1 表示起始方格。且只有一个起始方格。
  // 2 表示结束方格，且只有一个结束方格。
  // 0 表示我们可以走过的空方格。
  // -1 表示我们无法跨越的障碍。
  TSolution = class(TObject)
  private type
    TList_TPoint = specialize TArrayList<TPoint>;

  private
    _G: TArr2D_int;
    _R: integer;
    _C: integer;
    _Start: integer;
    _End: integer;
    _Visited: TArr2D_bool;
    _Left: integer;
    _List: TList_TPoint;


    function __Dfs(v, parent, left: integer): integer;

  public
    function UniquePathsIII(gird: TArr2D_int): integer;
  end;

procedure Main;

implementation

procedure Main;
begin
  //示例 1：
  //输入：[[1,0,0,0],[0,0,0,0],[0,0,2,-1]]
  //输出：2
  //解释：我们有以下两条路径：
  //1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2)
  //2. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2)
  with TSolution.Create do
  begin
    UniquePathsIII([[1, 0, 0, 0], [0, 0, 0, 0], [0, 0, 2, -1]]);

    Free;
  end;


  //示例 2：
  //输入：[[1,0,0,0],[0,0,0,0],[0,0,0,2]]
  //输出：4
  //解释：我们有以下四条路径：
  //1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2),(2,3)
  //2. (0,0),(0,1),(1,1),(1,0),(2,0),(2,1),(2,2),(1,2),(0,2),(0,3),(1,3),(2,3)
  //3. (0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(1,1),(0,1),(0,2),(0,3),(1,3),(2,3)
  //4. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2),(2,3)


  //示例 3：
  //输入：[[0,1],[2,0]]
  //输出：0
  //解释：
  //没有一条路能完全穿过每一个空的方格一次。
  //请注意，起始和结束方格可以位于网格中的任意位置。
end;

{ TSolution }

function TSolution.UniquePathsIII(gird: TArr2D_int): integer;
var
  j, i: integer;
begin
  _G := gird;
  _R := Length(gird);
  _C := Length(gird[0]);
  _Left := _R * _C;
  SetLength(_Visited, _R, _C);
  _Left := TList_TPoint.Create;

  for i := 0 to _R - 1 do
    for j := 0 to _C - 1 do
      case gird[i, j] of
        1: begin
          _Start := i * _C + j;
          gird[i, j] := 0;
        end;

        2: begin
          _End := i * _C + j;
          gird[i, j] := 0;
        end;

        -1: begin
          _Left -= 1;
        end;
      end;

  Result := __Dfs

  _List.Free;
end;

function TSolution.__Dfs(v, parent, left: integer): integer;
begin

end;

end.
