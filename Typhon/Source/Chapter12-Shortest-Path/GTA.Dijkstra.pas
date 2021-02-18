unit GTA.Dijkstra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TDijkstra = class(TObject)
  private
    _Dis: TArr_int;
    _Graph: TWeightedGraph;
    _Visited: TArr_bool;

  public
    constructor Create(g: IWeightedGraph; s: integer);
    destructor Destroy; override;

    function IsConnectedTo(v: integer): boolean;
    function DistTo(v: integer): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TWeightedGraph;
  v: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g.txt'));
  with TDijkstra.Create(g as IWeightedGraph, 0) do
  begin
    for v := 0 to g.Vertex - 1 do
      Write(DistTo(v), ' ');
    WriteLn;

    Free;
  end;
end;

{ TDijkstra }

constructor TDijkstra.Create(g: IWeightedGraph; s: integer);
var
  v, curV, curDis, tempWeight, W: integer;
begin
  _Graph := g as TWeightedGraph;
  SetLength(_Dis, _Graph.Vertex);
  TArrayUtils_int.FillArray(_Dis, integer.MaxValue);
  SetLength(_Visited, _Graph.Vertex);

  _Dis[s] := 0;

  while true do
  begin
    curV := -1;

    for v := 0 to _Graph.Vertex - 1 do
      if (not _Visited[v]) and (_Dis[v] < integer.MaxValue) then
      begin
        curDis := _Dis[v];
        curV := v;
      end;

    if curV = -1 then Break;

    _Visited[curV] := true;
    for W in _Graph.Adj(curV) do
    begin
      tempWeight := curDis + _Graph.GetWeight(curV, w);

      if (not _Visited[w]) and (_Dis[w] > tempWeight) then
        _Dis[w] := tempWeight;
    end;
  end;
end;

destructor TDijkstra.Destroy;
begin
  inherited Destroy;
end;

function TDijkstra.DistTo(v: integer): integer;
begin
  _Graph.ValidateVertex(v);
  Result := _Dis[v];
end;

function TDijkstra.IsConnectedTo(v: integer): boolean;
begin
  _Graph.ValidateVertex(v);
  Result := _Visited[v];
end;

end.
