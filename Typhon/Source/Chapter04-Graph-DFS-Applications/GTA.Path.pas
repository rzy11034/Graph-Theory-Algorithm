unit GTA.Path;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TPath = class(TObject)
  private
    _g: IGraph;
    _src: integer;
    _t: integer;
    _visited: TArr_bool;
    _pre: TArr_int;

    function __dfs(v, parent: integer): boolean;

  public
    constructor Create(g: IGraph; src, t: integer);
    destructor Destroy; override;

    function IsConnected: boolean;
    function Path: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  p: TPath;
begin
  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g.txt'));

  p := TPath.Create(g, 0, 6);
  Write('0 -> 6 : ');
  TArrayUtils_int.Print(p.Path);
  p.Free;

  p := TPath.Create(g, 0, 1);
  Write('0 -> 1 : ');
  TArrayUtils_int.Print(p.Path);
  p.Free;

  p := TPath.Create(g, 0, 5);
  Write('0 -> 5 : ');
  TArrayUtils_int.Print(p.Path);
  p.Free;
end;

{ TPath }

constructor TPath.Create(g: IGraph; src, t: integer);
begin
  g.ValidateVertex(src);
  g.ValidateVertex(t);

  _g := g;
  _src := src;
  _t := t;

  SetLength(_visited, g.Vertex);
  SetLength(_pre, _g.Vertex);
  TArrayUtils_int.FillArray(_pre, -1);

  __dfs(_src, src);
end;

destructor TPath.Destroy;
begin
  inherited Destroy;
end;

function TPath.IsConnected: boolean;
begin
  Result := _visited[_t];
end;

function TPath.Path: TArr_int;
var
  res: TArr_int;
  list: TArrayList_int;
  cur, i, n: integer;
begin
  if not IsConnected then Exit(nil);

  list := TArrayList_int.Create;
  try
    cur := _t;
    while cur <> _src do
    begin
      list.AddLast(cur);
      cur := _pre[cur];
    end;
    list.AddLast(_src);

    n := 0;
    SetLength(res, list.Count);
    for i := list.Count - 1 downto 0 do
    begin
      res[n] := list[i];
      n += 1;
    end;

    Result := res;
  finally
    list.Free;
  end;
end;

function TPath.__dfs(v, parent: integer): boolean;
var
  w: integer;
begin
  _visited[v] := true;
  _pre[v] := parent;

  if v = _t then Exit(true);

  for w in _g.Adj(v) do
    if _visited[w] <> true then
      if __dfs(w, v) then
        Exit(true);

  Result := false;
end;

end.
