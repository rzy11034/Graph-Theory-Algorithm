unit GTA.Kruskal;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList,
  GTA.WeightedCC,
  GTA.Utils;

type
  TKruskal = class(TObject)
  private type
    TList_TWeightedEdge = specialize TArrayList<TWeightedEdge>;
    TArr_TWeightedEdge = array of TWeightedEdge;

  private
    _Mst: TList_TWeightedEdge;
    _Graph: TWeightedGraph;

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Return: TArr_TWeightedEdge;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TWeightedGraph;
  k: TKruskal;
begin
  g := TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt'));

  with TKruskal.Create(g) do
  begin

  end;
end;

{ TKruskal }

constructor TKruskal.Create(g: IWeightedGraph);
var
  cc: TWeightedCC;
  v, w: integer;
  edges: TList_TWeightedEdge;
begin
  _Graph := (g as TWeightedGraph);
  _Mst := TList_TWeightedEdge.Create;

  cc := TWeightedCC.Create(g);
  try
    if cc.Count > 1 then
      Exit;
  finally
    cc.Free;
  end;

  edges := TList_TWeightedEdge.Create(@TWeightedEdge(nil).Compare);
  try
    for v := 0 to g.Vertex - 1 do
      for w in g.Adj(v) do
        if (v < w) then
          edges.AddLast(TWeightedEdge.Create(v, w, g.GetWeight(v, w)));

    edges.Sort;
  finally
    edges.Free;
  end;
end;

destructor TKruskal.Destroy;
begin
  inherited Destroy;
end;

function TKruskal.Return: TArr_TWeightedEdge;
begin
  Result := _Mst.ToArray;
end;

end.
