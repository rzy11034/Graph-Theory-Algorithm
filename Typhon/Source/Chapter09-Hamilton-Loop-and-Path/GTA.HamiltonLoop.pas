unit GTA.HamiltonLoop;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.UString,
  GTA.Utils;

type
  THamiltonLoop = class(TObject)
  private
    _Visited: TArr_bool;
    _Graph: IGraph;
    _Pre: TArr_int;
    _End: integer;

    function __Dfs(v: integer; parent: integer): boolean;
    function __AllVisited: boolean;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function Return: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  chapter: UString = 'Chapter09-Hamilton-Loop-and-Path';
begin
  with THamiltonLoop.Create(TGraph.Create(FileName(chapter, 'g.txt'))) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  with THamiltonLoop.Create(TGraph.Create(FileName(chapter, 'g2.txt'))) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ THamiltonLoop }

constructor THamiltonLoop.Create(g: IGraph);
begin
  _Graph := g;
  SetLength(_Visited, g.Vertex);
  SetLength(_Pre, g.Vertex);
  _End := -1;

  __Dfs(0, 0);
end;

destructor THamiltonLoop.Destroy;
begin
  inherited Destroy;
end;

function THamiltonLoop.Return: TArr_int;
var
  list: IList_int;
  res: TArr_int;
  cur: integer;
begin
  if _End = -1 then Exit([]);

  list := TArrayList_int.Create;
  cur := _End;
  while cur <> 0 do
  begin
    list.AddLast(cur);
    cur := _Pre[cur];
  end;
  list.AddLast(0);

  res := list.ToArray;
  TArrayUtils_int.Reverse(res);
  Result := res;
end;

function THamiltonLoop.__AllVisited: boolean;
var
  res: boolean;
begin
  for res in _Visited do
  begin
    if res = false then
      Break;
  end;

  Result := res;
end;

function THamiltonLoop.__Dfs(v: integer; parent: integer): boolean;
var
  w: Integer;
begin
  _Visited[v] := true;
  _Pre[v] := parent;

  for w in _Graph.Adj(v) do
  begin
    if not _Visited[w] then
    begin
      if __Dfs(w, v) then
        Exit(true);
    end
    else if (w = 0) and __AllVisited then
    begin
      _End := v;
      Exit(true);
    end;
  end;

  _Visited[v] := false;
  Result := false;
end;

end.
