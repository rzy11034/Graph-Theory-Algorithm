unit GTA.Dijkstra_Algorithm_Optimized;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.PriorityQueue,
  DeepStar.Utils,
  GTA.Utils;

type
  TDijkstra = class(TObject)
  private type
    TPair = record
      Vertex: integer;
      Weight: integer;
      class function Create(newVertex, newWeight: integer): TPair; static;
      class function Comparer(constref a, b: TPair): integer; static;
    end;

    TQueue_TPair = specialize TPriorityQueue<TPair>;

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
  g: IWeightedGraph;
  v: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g.txt'));
  with TDijkstra.Create(g, 0) do
  begin
    for v := 0 to g.Vertex - 1 do
      Write(DistTo(v), ' ');
    WriteLn;

    Free;
  end;
end;

{ TDijkstra.TPair }

class function TDijkstra.TPair.Comparer(constref a, b: TPair): integer;
begin
  Result := a.Weight - b.Weight;
end;

class function TDijkstra.TPair.Create(newVertex, newWeight: integer): TPair;
var
  res: TPair;
begin
  res.Vertex := newVertex;
  res.Weight := newWeight;
  Result := res;
end;

{ TDijkstra }

constructor TDijkstra.Create(g: IWeightedGraph; s: integer);
var
  v, w, tempDis: integer;
  queue: TQueue_TPair;
begin
  _Graph := g as TWeightedGraph;
  TArrayUtils_int.SetLengthAndFill(_Dis, _Graph.Vertex, integer.MaxValue);
  TArrayUtils_bool.SetLengthAndFill(_Visited, _Graph.Vertex);

  _Dis[s] := 0;
  queue := TQueue_TPair.Create(TQueue_TPair.TCmp.Construct(@TPair.Comparer));
  try
    queue.EnQueue(TPair.Create(s, 0));

    while not queue.IsEmpty do
    begin
      v := queue.DeQueue.Vertex;

      if _Visited[v] then Continue;

      _Visited[v] := true;
      for w in _Graph.Adj(v) do
      begin
        if (not _Visited[w]) then
        begin
          _Dis[w] := _Dis[v] + _Graph.GetWeight(v, w);
          queue.EnQueue(TPair.Create(w, _Dis[w]));
        end;
      end;
    end;
  finally
    queue.Free;
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
