unit GTA.Prim_Algorithm_Optimized;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.WeightedEdge,
  GTA.WeightedCC,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Tree.PriorityQueue,
  DeepStar.Utils;

type
  TPrim = class(TObject)
  public type
    TList_TWeightedEdge = specialize TArrayList<TWeightedEdge>;
    TArr_TWeightedEdge = array of TWeightedEdge;
    TQueue_TWeightedEdge = specialize TPriorityQueue<TWeightedEdge>;

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
  minEdge: TWeightedEdge;
  v, w: integer;
  queue: TQueue_TWeightedEdge;
  cmp: TQueue_TWeightedEdge.ICmp;
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

  cmp := TQueue_TWeightedEdge.TCmp.Construct(@TWeightedEdge(nil).Compare);
  queue := TQueue_TWeightedEdge.Create(cmp, THeapkind.Min);
  try
    for w in _Graph.Adj(0) do
      queue.EnQueue(TWeightedEdge.Create(0, w, _Graph.GetWeight(0, w)));

    while not queue.IsEmpty do
    begin
      minEdge := queue.DeQueue;

      if visited[minEdge.VertexV] and visited[minEdge.VertexW] then
      begin
        minEdge.Free;
        Continue;
      end
      else
      begin
        _Mst.AddLast(minEdge);
        v := IfThen(visited[minEdge.VertexV], minEdge.VertexW, minEdge.VertexV);
        visited[v] := true;
        for w in _Graph.Adj(v) do
        begin
          if not visited[w] then
            queue.EnQueue(TWeightedEdge.Create(v, w, _Graph.GetWeight(v, w)));
        end;
      end;
    end;
  finally
    queue.Free;
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
