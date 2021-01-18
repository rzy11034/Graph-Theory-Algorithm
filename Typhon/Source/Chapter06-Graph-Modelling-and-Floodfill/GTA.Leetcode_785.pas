unit GTA.Leetcode_785;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

// 785. 判断二分图
type
  TSolution = class
  private type
    TColor = (red, green, null);

  private
    _visited: TArr_bool;
    _colors: array of TColor;
    _g: TArr2D_int;

    function __dfs(v: integer; color: TColor): boolean;

  public
    function IsBipartite(graph: TArr2D_int): boolean;
  end;

procedure Main;

implementation

procedure Main;
var
  graph: TArr2D_int;
begin
  with TSolution.Create do
  begin
    graph := [[1, 3], [0, 2], [1, 3], [0, 2]];
    WriteLn(IsBipartite(graph));
    Free;
  end;


  with TSolution.Create do
  begin
    graph := [[1, 2, 3], [0, 2], [0, 1, 3], [0, 2]];
    WriteLn(IsBipartite(graph));
    Free;
  end;
end;

{ TSolution }

function TSolution.IsBipartite(graph: TArr2D_int): boolean;
var
  v, i: integer;
begin
  v := Length(graph);
  _g := graph;

  SetLength(_visited, v);
  SetLength(_colors, v);
  for i := 0 to v - 1 do
  begin
    _visited[i] := false;
    _colors[i] := TColor.null;
  end;

  for i := 0 to v - 1 do
  begin
    if not _visited[i] then
      if not __dfs(i, TColor.red) then
        Exit(false);
  end;

  Result := true;
end;

function TSolution.__dfs(v: integer; color: TColor): boolean;
var
  w: integer;
  res: boolean;
begin
  _visited[v] := true;
  _colors[v] := color;

  for w in _g[v] do
  begin
    if not _visited[w] then
    begin
      case color of
        TColor.red:
          res := __dfs(w, TColor.green);

        TColor.green:
          res := __dfs(w, TColor.red);

        else;
      end;

      if not res then
        Exit(false);
    end
    else if _colors[w] = _colors[v] then
      Exit(false);
  end;

  Result := true;
end;

end.

