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
  public type
    TColor = (red, green, null);
    TArr_Color = array of TColor;

  private
    _Graph: IGraph;
    _visited: TArr_bool;
    _Colors: TArr_Color;
    _IsBipartite: boolean;

    function __DFS(v: integer; color: TColor): boolean;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    property IsBipartite: boolean read _IsBipartite;
    property Colors: TArr_Color read _Colors;
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

  g := TGraph.Create(FileName('Chapter15-Matching-Problem', 'g.txt'));
  bd := TBipartitionDetection.Create(g);
  WriteLn(bd._isBipartite);
  bd.Free;
end;

{ TBipartitionDetection }

constructor TBipartitionDetection.Create(g: IGraph);
var
  v, i: integer;
begin
  _Graph := g;
  _isBipartite := true;
  SetLength(_visited, g.Vertex);
  SetLength(_colors, g.Vertex);
  for i := 0 to High(_colors) do
    _colors[i] := TColor.null;

  /////////////////////////////////////
  for v := 0 to g.Vertex - 1 do
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

function TBipartitionDetection.__DFS(v: integer; color: TColor): boolean;
var
  w: integer;
  res: boolean;
begin
  _visited[v] := true;
  _colors[v] := color;

  for w in _Graph.Adj(v) do
  begin
    if not _visited[w] then
    begin
      case color of
        TColor.green: res := __DFS(w, TColor.red);
        TColor.red: res := __DFS(w, TColor.green);
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
