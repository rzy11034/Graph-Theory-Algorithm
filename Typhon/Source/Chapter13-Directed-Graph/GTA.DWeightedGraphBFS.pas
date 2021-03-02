unit GTA.DWeightedGraphBFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TWeightedGraphBFS = class(TObject)
  private
    _Graph: TWeightedGraph;
    _Visited: TArr_bool;
    _Order: IList_int;

    procedure __Bfs(v: integer);

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Order: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TWeightedGraph;
begin
  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg.txt'), true);
  with TWeightedGraphBFS.Create(g) do
  begin
    TArrayUtils_int.Print(Order);
    Free;
  end;
end;

{ TWeightedGraphBFS }

constructor TWeightedGraphBFS.Create(g: IWeightedGraph);
var
  v: integer;
begin
  _Graph := g as TWeightedGraph;
  SetLength(_Visited, g.Vertex);
  _Order := TArrayList_int.Create;

  for v := 0 to g.Vertex - 1 do
    if not _Visited[v] then
      __Bfs(v);
end;

destructor TWeightedGraphBFS.Destroy;
begin
  inherited Destroy;
end;

function TWeightedGraphBFS.Order: TArr_int;
begin
  Result := _Order.ToArray;
end;

procedure TWeightedGraphBFS.__Bfs(v: integer);
var
  queue: TQueue_int;
  cur, w: integer;
begin
  queue := TQueue_int.Create;
  try
    queue.EnQueue(v);
    _Visited[v] := true;
    _Order.AddLast(v);

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;

      for w in _Graph.Adj(cur) do
        if not _Visited[v] then
        begin
          queue.EnQueue(w);
          _Visited[w] := true;
          _Order.AddLast(v);
        end;
    end;

  finally
    queue.Free;
  end;
end;

end.
