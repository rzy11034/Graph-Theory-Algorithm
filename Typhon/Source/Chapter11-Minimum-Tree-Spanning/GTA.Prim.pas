unit GTA.Prim;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.WeightedEdge,
  GTA.WeightedCC,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.Utils;

type
  TPrim = class(TObject)
  public type
    TList_TWeightedEdge = specialize TArrayList<TWeightedEdge>;
    TArr_TWeightedEdge = array of TWeightedEdge;

  private
    _Graph: TWeightedGraph;
    _Mst: TList_TWeightedEdge;

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Return: TArr_TWeightedEdge;
  end;

procedure Main;

implementation

procedure Main;
var
  i: integer;
  e: TWeightedEdge;
begin
  with TPrim.Create(TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt'))) do
  begin
    for e in Return do
      Write(e.ToString, ' ');
    WriteLn;

    Free;
  end;
end;

{ TPrim }

constructor TPrim.Create(g: IWeightedGraph);
var
  cc: TWeightedCC;
  visited: TArr_bool;
  minEdge, temp: TWeightedEdge;
  i, v, w: integer;
begin
  _Graph := g as TWeightedGraph;
  _Mst := TList_TWeightedEdge.Create;

  cc := TWeightedCC.Create(g);
  try
    if cc.Count > 1 then
      Exit;
  finally
    cc.Free;
  end;

  // Prim
  SetLength(visited, g.Vertex);
  visited[0] := true;

  for i := 1 to g.Vertex - 1 do
  begin
    minEdge := TWeightedEdge.Create(-1, -1, MaxInt);
    for v := 0 to g.Vertex - 1 do
      if visited[v] then
        for w in g.Adj(v) do
          if (not visited[w]) and (g.GetWeight(v, w) < minEdge.Weight) then
          begin
            minEdge.Free;
            minEdge := TWeightedEdge.Create(v, w, g.GetWeight(v, w));
          end;

    _Mst.AddLast(minEdge);
    visited[minEdge.VertexV] := true;
    visited[minEdge.VertexW] := true;
  end;
end;

destructor TPrim.Destroy;
var
  i: integer;
begin
  for i := 0 to _Mst.Count - 1 do
    _Mst[i].Free;
  _Mst.Free;

  inherited Destroy;
end;

function TPrim.Return: TArr_TWeightedEdge;
begin
  Result := _Mst.ToArray;
end;

end.
