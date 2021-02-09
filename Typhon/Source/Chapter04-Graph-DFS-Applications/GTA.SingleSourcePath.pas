unit GTA.SingleSourcePath;

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
    _src: integer;
    _visited: TArr_bool;
    _pre: TArr_int;

    procedure __dfs(v, parent: integer);

  public
    constructor Create(g: IGraph; src: integer);
    destructor Destroy; override;

    function IsConnectedTo(t: integer): boolean;
    function Path(t: integer): TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  ssPath: TSingleSourcePath;
begin
  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g.txt'));
  ssPath := TSingleSourcePath.Create(g, 0);

  Write('0 -> 6 : ');
  TArrayUtils_int.Print(ssPath.Path(6));

  Write('0 -> 5 : ');
  TArrayUtils_int.Print(ssPath.Path(5));

  ssPath.Free;
end;

{ TSingleSourcePath }

constructor TSingleSourcePath.Create(g: IGraph; src: integer);
begin
  g.ValidateVertex(src);

  _g := g;
  _src := src;

  SetLength(_visited, g.Vertex);
  SetLength(_pre, _g.Vertex);
  TArrayUtils_int.FillArray(_pre, -1);

  __dfs(_src, src);
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

procedure TSingleSourcePath.__dfs(v, parent: integer);
var
  w: integer;
begin
  _visited[v] := true;
  _pre[v] := parent;

  for w in _g.Adj(v) do
  begin
    if _visited[w] <> true then
      __dfs(w, v);
  end;
end;

end.
