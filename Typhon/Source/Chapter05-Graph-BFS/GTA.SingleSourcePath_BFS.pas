unit GTA.SingleSourcePath_BFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TSingleSourcePath = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _pre: TArr_int;
    _src: integer;

    procedure __bfs(v: integer);

  public
    constructor Create(g: IGraph; s: integer);
    destructor Destroy; override;

    function Path(t: integer): TArr_int;
    function IsConnectedTo(t: integer): boolean;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  ssPath: TSingleSourcePath;
begin
  g := TGraph.Create(FileName('Chapter05-Graph-BFS', 'g.txt'));
  ssPath := TSingleSourcePath.Create(g, 0);
  Write('0 -> 6 : ');
  TArrayUtils_int.Print(ssPath.Path(6));
  ssPath.Free;
end;

{ TSingleSourcePath }

constructor TSingleSourcePath.Create(g: IGraph; s: integer);
begin
  _g := g;
  _src := s;
  SetLength(_visited, g.Vertex);
  SetLength(_pre, g.Vertex);
  TArrayUtils_int.FillArray(_pre, -1);

  ///////////////////////////////
  __bfs(s);
end;

destructor TSingleSourcePath.Destroy;
begin
  inherited Destroy;
end;

function TSingleSourcePath.IsConnectedTo(t: integer): boolean;
begin
  _g.ValidateVertex(t);
  Result := _visited[t];
end;

function TSingleSourcePath.Path(t: integer): TArr_int;
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

procedure TSingleSourcePath.__bfs(v: integer);
var
  queue: TQueue_int;
  cur, w: integer;
begin
  queue := TQueue_int.Create;
  try
    queue.EnQueue(v);
    _visited[v] := true;
    _pre[v] := v;

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
        end;
      end;
    end;
  finally
    queue.Free;
  end;
end;

end.
