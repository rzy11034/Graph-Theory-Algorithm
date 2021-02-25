unit GTA.DGraphBFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TGraphBFS = class(TObject)
  private
    _Graph: TGraph;
    _Visited: TArr_bool;
    _Order: IList_int;

    procedure __Bfs(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function Order: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TGraphBFS.Create(g) do
  begin
    TArrayUtils_int.Print(Order);
    Free;
  end;
end;

{ TGraphBFS }

constructor TGraphBFS.Create(g: IGraph);
var
  v: integer;
begin
  _Graph := g as TGraph;
  SetLength(_Visited, g.Vertex);
  _Order := TArrayList_int.Create;

  for v := 0 to _Graph.Vertex - 1 do
    if not _Visited[v] then
      __Bfs(v);
end;

destructor TGraphBFS.Destroy;
begin
  inherited Destroy;
end;

function TGraphBFS.Order: TArr_int;
begin
  Result := _Order.ToArray;
end;

procedure TGraphBFS.__Bfs(v: integer);
var
  queue: IQueue_int;
  w: integer;
begin
  queue := TQueue_int.Create;

  queue.EnQueue(v);
  _Visited[v] := true;
  _Order.AddLast(v);

  while not queue.IsEmpty do
  begin
    v := queue.DeQueue;

    for w in _Graph.Adj(v) do
    begin
      if not _Visited[w] then
      begin
        queue.EnQueue(w);
        _Visited[w] := true;
        _Order.AddLast(w);
      end;
    end;
  end;
end;

end.
