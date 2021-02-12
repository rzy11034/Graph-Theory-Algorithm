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
  TKruskal = class(TInterfacedObject)
  private type
    TList_IWeightedEdge = specialize TArrayList<IWeightedEdge>;
    TArr_IWeightedEdge = array of IWeightedEdge;

  private
    _Mst: TList_IWeightedEdge;
    _Graph: TWeightedGraph;

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Return: TArr_IWeightedEdge;
  end;

procedure Main;

implementation

procedure Main;
begin

end;

{ TKruskal }

constructor TKruskal.Create(g: IWeightedGraph);
var
  cc: TWeightedCC;
  v, w: integer;
  edges: TList_IWeightedEdge;
  cmp: TList_IWeightedEdge.TImpl.ICmp;
begin
  _Graph := (g as TWeightedGraph);
  _Mst := TList_IWeightedEdge.Create;

  cc := TWeightedCC.Create(g);
  try
    if cc.Count > 1 then
      Exit;
  finally
    cc.Free;
  end;

  edges := TList_IWeightedEdge.Create(@TWeightedEdge(nil).Compare);
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

function TKruskal.Return: TArr_IWeightedEdge;
begin
  Result := _Mst.ToArray;
end;

end.
