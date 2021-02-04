unit GTA.HamiltonPath;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.UString,
  DeepStar.Utils,
  GTA.Utils;

type
  THamiltonPath = class(TObject)
  private
    _Visited: TArr_bool;
    _Graph: IGraph;
    _Pre: TArr_int;
    _End: integer;
    _S: integer;

    function __Dfs(v, parent, left: integer): boolean;

  public
    constructor Create(g: IGraph; s: integer);
    destructor Destroy; override;

    function Return: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  chapter: UString = 'Chapter09-Hamilton-Loop-and-Path';
  g: IGraph;
begin
  g := TGraph.Create(FileName(chapter, 'g_path.txt'));

  with THamiltonPath.Create(g, 0) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  with THamiltonPath.Create(g, 1) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ THamiltonPath }

constructor THamiltonPath.Create(g: IGraph; s: integer);
begin
  _Graph := g;
  SetLength(_Visited, g.V);
  SetLength(_Pre, g.V);
  _S := s;
  _End := -1;

  __Dfs(s, s, g.V);
end;

destructor THamiltonPath.Destroy;
begin
  inherited Destroy;
end;

function THamiltonPath.Return: TArr_int;
var
  list: IList_int;
  res: TArr_int;
  cur: integer;
begin
  if _End = -1 then Exit([]);

  list := TArrayList_int.Create;
  cur := _End;
  while cur <> _S do
  begin
    list.AddLast(cur);
    cur := _Pre[cur];
  end;
  list.AddLast(_S);

  res := list.ToArray;
  TArrayUtils_int.Reverse(res);
  Result := res;
end;

function THamiltonPath.__Dfs(v, parent, left: integer): boolean;
var
  w: integer;
begin
  _Visited[v] := true;
  _Pre[v] := parent;
  left -= 1;

  if (left = 0) then
  begin
    _End := v;
    Exit(true);
  end;

  for w in _Graph.Adj(v) do
    if not _Visited[w] then
      if __Dfs(w, v, left) then
        Exit(true);

  _Visited[v] := false;
  Result := false;
end;

end.
