unit GTA.GraphBFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TGraphBFS = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _order: TArrayList_int;

    procedure __bfs(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function Order: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  graphBFS: TGraphBFS;
begin
  g := TGraph.Create(FileName('Chapter05-Graph-BFS', 'g.txt'));
  graphBFS := TGraphBFS.Create(g);
  TArrayUtils_int.Print(graphBFS.Order);
  graphBFS.Free;

end;

{ TGraphBFS }

constructor TGraphBFS.Create(g: IGraph);
var
  v: integer;
begin
  _g := g;
  SetLength(_visited, g.V);
  _order := TArrayList_int.Create;

  for v := 0 to g.V - 1 do
  begin
    if not _visited[v] then
      __bfs(v);
  end;
end;

destructor TGraphBFS.Destroy;
begin
  _order.Free;
  inherited Destroy;
end;

function TGraphBFS.Order: TArr_int;
begin
  Result := _order.ToArray;
end;

procedure TGraphBFS.__bfs(v: integer);
var
  queue: TQueue_int;
  cur, w: integer;
begin
  queue := TQueue_int.Create;
  try
    queue.EnQueue(v);
    _visited[v] := true;
    _order.AddLast(v);

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;

      for w in _g.Adj(cur) do
      begin
        if not _visited[w] then
        begin
          queue.EnQueue(w);
          _visited[w] := true;
          _order.AddLast(w);
        end;
      end;
    end;
  finally
    queue.Free;
  end;
end;

end.
