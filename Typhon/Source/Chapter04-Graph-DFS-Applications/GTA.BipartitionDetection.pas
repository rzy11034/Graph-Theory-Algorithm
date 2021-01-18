unit GTA.BipartitionDetection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TBipartitionDetection = class(TObject)
  private type
    TColor = (red, green, null);

  private
    _g: IGraph;
    _visited: TArr_bool;
    _colors: array of TColor;
    _isBipartite: boolean;

    function __dfs(v: integer; color: TColor): boolean;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  bd: TBipartitionDetection;
begin
  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g.txt'));
  bd := TBipartitionDetection.Create(g);
  WriteLn(bd._isBipartite);
  bd.Free;

  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g3.txt'));
  bd := TBipartitionDetection.Create(g);
  WriteLn(bd._isBipartite);
  bd.Free;
end;

{ TBipartitionDetection }

constructor TBipartitionDetection.Create(g: IGraph);
var
  v, i: integer;
begin
  _g := g;
  _isBipartite := true;
  SetLength(_visited, g.V);
  SetLength(_colors, g.V);
  for i := 0 to High(_colors) do
    _colors[i] := TColor.null;

  /////////////////////////////////////
  for v := 0 to g.V - 1 do
  begin
    if not _visited[v] then
      if not __dfs(v, TColor.red) then
      begin
        _isBipartite := false;
        Break;
      end;
  end;
end;

destructor TBipartitionDetection.Destroy;
begin
  inherited Destroy;
end;

function TBipartitionDetection.__dfs(v: integer; color: TColor): boolean;
var
  w: integer;
  res: boolean;
begin
  _visited[v] := true;
  _colors[v] := color;

  for w in _g.Adj(v) do
  begin
    if not _visited[w] then
    begin
      case color of
        TColor.green:
          res := __dfs(w, TColor.red);

        TColor.red:
          res := __dfs(w, TColor.green);

        TColor.null: ;
      end;

      if res then
        Exit(false);
    end
    else if _colors[w] = _colors[v] then
      Exit(false);
  end;

  Result := true;
end;

end.

