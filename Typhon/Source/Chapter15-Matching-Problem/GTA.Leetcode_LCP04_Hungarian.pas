unit GTA.Leetcode_LCP04_Hungarian;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Hungarian,
  GTA.Utils;

//LCP 04. 覆盖
//你有一块棋盘，棋盘上有一些格子已经坏掉了。你还有无穷块大小为1 * 2的多米诺骨牌，
//你想把这些骨牌不重叠地覆盖在完好的格子上，请找出你最多能在棋盘上放多少块骨牌？
//这些骨牌可以横着或者竖着放。
//
//输入：n, m代表棋盘的大小；broken是一个b * 2的二维数组，
//其中每个元素代表棋盘上每一个坏掉的格子的位置。
//
//输出：一个整数，代表最多能在棋盘上放的骨牌数。
//
//示例 1：
//输入：n = 2, m = 3, broken = [[1, 0], [1, 1]]
//输出：2
//解释：我们最多可以放两块骨牌：[[0, 0], [0, 1]]以及[[0, 2], [1, 2]]。
//
//示例 2：
//输入：n = 3, m = 3, broken = []
//输出：4

type
  TSolution = class(TObject)
  public
    function Domino(n, m: integer; broken: TArr2D_int): integer;
  end;

procedure Main;

implementation

procedure Main;
begin
  //示例 1：
  //输入：n = 2, m = 3, broken = [[1, 0], [1, 1]]
  //输出：2
  with TSolution.Create do
  begin
    WriteLn(Domino(2, 3, [[1, 0], [1, 1]]));
    Free;
  end;

  DrawLineBlockEnd;

  //示例 2：
  //输入：n = 3, m = 3, broken = []
  //输出：4
  with TSolution.Create do
  begin
    WriteLn(Domino(3, 3, []));
    Free;
  end;
end;

{ TSolution }

function TSolution.Domino(n, m: integer; broken: TArr2D_int): integer;
var
  g: IGraph;
  p: TArr_int;
  broad: TArr2D_int;
  i, j: integer;
  hgrn: THungarian;
begin
  TArrayUtils_int.SetLengthAndFill(broad, n, m, 0);

  for p in broken do
    broad[p[0], p[1]] := 1;

  g := TGraph.Create(n * m);
  for i := 0 to High(broad) do
  begin
    for j := 0 to High(broad[i]) do
    begin
      if (j + 1 < m) and (broad[i, j] = 0) and (broad[i, j + 1] = 0) then
        (g as TGraph).AddEdge(i * m + j, i * m + (j + 1));

      if (i + 1 < n) and (broad[i, j] = 0) and (broad[i + 1, j] = 0) then
        (g as TGraph).AddEdge(i * m + j, (i + 1) * m + j);
    end;
  end;

  hgrn := THungarian.Create(g);
  try
    Result := hgrn.MaxMatching;
  finally
    hgrn.Free;
  end;
end;

end.
