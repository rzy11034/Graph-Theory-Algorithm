unit GTA.Kruskal;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Linear.ArrayList,
  GTA.WeightedCC,
  GTA.UnionFind,
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
  i: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt'));

  with TKruskal.Create(g) do
  begin
    for i := 0 to High(Return) do
      Write(Return[i].ToString, ' ');
    WriteLn;

    Free;
  end;
end;

{ TKruskal }

constructor TKruskal.Create(g: IWeightedGraph);
var
  cc: TWeightedCC;
  v, w: integer;
  edges: TList_TWeightedEdge;
  cmp: TList_TWeightedEdge.ICmp;
  uf: TUnionFind;
  we: TWeightedEdge;
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

  cmp := TList_TWeightedEdge.TCmp.Construct(@TWeightedEdge(nil).Compare);
  edges := TList_TWeightedEdge.Create(cmp);
  try
    for v := 0 to g.Vertex - 1 do
      for w in g.Adj(v) do
        if (v < w) then
          edges.AddLast(TWeightedEdge.Create(v, w, g.GetWeight(v, w)));

    edges.Sort;

    uf := TUnionFind.Create(_Graph.Vertex);
    try
      for we in edges.ToArray do
      begin
        v := we.VertexV;
        w := we.VertexW;
        if uf.IsConnected(v, w) then
        begin
          we.Free;
          Continue;
        end
        else
        begin
          _Mst.AddLast(we);
          uf.Union(v, w);
        end;
      end;
    finally
      uf.Free;
    end;
  finally
    edges.Free;
  end;
end;

destructor TKruskal.Destroy;
var
  e: TWeightedEdge;
begin
  for e in _Mst.ToArray do
    e.Free;

  _Mst.Free;
  inherited Destroy;
end;

function TKruskal.Return: TArr_TWeightedEdge;
begin
  Result := _Mst.ToArray;
end;

end.
