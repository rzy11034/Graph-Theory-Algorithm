unit GTA.UnweightedSingleSourceShortestPath;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

// Unweighted Single Source Shortest Path
type

  TUnweightedSingleSourceShortestPath = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _pre, _dis: TArr_int;
    _src: integer;

    procedure __bfs(v: integer);

  public
    constructor Create(g: IGraph; s: integer);
    destructor Destroy; override;

    function Path(t: integer): TArr_int;
    function IsConnectedTo(t: integer): boolean;
    function Dis(t: integer): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  ussPath: TUnweightedSingleSourceShortestPath;
begin
  g := TGraph.Create(FileName('Chapter05-Graph-BFS', 'g.txt'));
  ussPath := TUnweightedSingleSourceShortestPath.Create(g, 0);
  Write('0 -> 6 : ');
  TArrayUtils_int.Print(ussPath.Path(6));
  WriteLn(ussPath.Dis(6));
  ussPath.Free;
end;

{ TUnweightedSingleSourceShortestPath }

constructor TUnweightedSingleSourceShortestPath.Create(g: IGraph; s: integer);
begin
  _g := g;
  _src := s;
  SetLength(_visited, g.Vertex);
  SetLength(_pre, g.Vertex);
  TArrayUtils_int.FillArray(_pre, -1);
  _dis := TArrayUtils_int.CopyArray(_pre);

  ///////////////////////////////
  __bfs(s);
  TArrayUtils_int.Print(_dis);
end;

destructor TUnweightedSingleSourceShortestPath.Destroy;
begin
  inherited Destroy;
end;

function TUnweightedSingleSourceShortestPath.Dis(t: integer): integer;
begin
  _g.ValidateVertex(t);
  Result := _dis[t];
end;

function TUnweightedSingleSourceShortestPath.IsConnectedTo(t: integer): boolean;
begin
  _g.ValidateVertex(t);
  Result := _visited[t];
end;

function TUnweightedSingleSourceShortestPath.Path(t: integer): TArr_int;
var
  res: TArr_int;
  list: TArrayList_int;
  cur: integer;
begin
  if not IsConnectedTo(t) then Exit(nil);

  list := TArrayList_int.Create;
  try
    cur := t;
    while cur <> _src do
    begin
      list.AddLast(cur);
      cur := _pre[cur];
    end;
    list.AddLast(_src);

    res := list.ToArray;
    TArrayUtils_int.Reverse(res);

    Result := res;
  finally
    list.Free;
  end;
end;

procedure TUnweightedSingleSourceShortestPath.__bfs(v: integer);
var
  queue: TQueue_int;
  cur, w: integer;
begin
  queue := TQueue_int.Create;
  try
    queue.EnQueue(v);
    _visited[v] := true;
    _pre[v] := v;
    _dis[v] := 0;

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;

      for w in _g.Adj(cur) do
      begin
        if not _visited[w] then
        begin
          queue.EnQueue(w);
          _visited[w] := true;
          _pre[w] := cur;
          _dis[w] := _dis[cur] + 1;
        end;
      end;
    end;
  finally
    queue.Free;
  end;
end;

end.
