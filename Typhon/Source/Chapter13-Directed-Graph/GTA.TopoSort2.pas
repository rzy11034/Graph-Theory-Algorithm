unit GTA.TopoSort2;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TTopoSort = class(TObject)
  private
    _Graph: TGraph;
    _HasCycle: boolean;
    _list: IList_int;
    _Visited, _OnPath: TArr_bool;

    function __DirectedCycleDetection(v, parent: integer): boolean;
    procedure __DFS(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;
    function Return: TArr_int;

    property HasCycle: boolean read _HasCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TTopoSort.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug2.txt'), true);
  with TTopoSort.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ TTopoSort }

constructor TTopoSort.Create(g: IGraph);
var
  v: integer;
begin
  _Graph := g as TGraph;
  _list := TLinkedList_int.Create;
  _HasCycle := false;
  SetLength(_Visited, g.Vertex);
  SetLength(_OnPath, g.Vertex);

  for v := 0 to g.Vertex - 1 do
    if not _Visited[v] then
    begin
      _HasCycle := __DirectedCycleDetection(v, v);

      if _HasCycle = true then
        Exit;
    end;

  TArrayUtils_bool.FillArray(_Visited, false);
  for v := 0 to _Graph.Vertex - 1 do
    if not _Visited[v] then
      __Dfs(v);
end;

destructor TTopoSort.Destroy;
begin
  inherited Destroy;
end;

function TTopoSort.Return: TArr_int;
begin
  Result := _list.ToArray;
  TArrayUtils_int.Reverse(Result);
end;

procedure TTopoSort.__DFS(v: integer);
var
  w: integer;
begin
  _Visited[v] := true;

  for w in _Graph.Adj(v) do
    if not _Visited[w] then
      __Dfs(w);

  _list.AddLast(v);
end;

function TTopoSort.__DirectedCycleDetection(v, parent: integer): boolean;
var
  w: integer;
begin
  _Visited[v] := true;
  _OnPath[v] := true;

  for w in _Graph.Adj(v) do
  begin
    if not _Visited[w] then
    begin
      if __DirectedCycleDetection(w, v) then
        Exit(true);
    end
    else if _OnPath[w] then
      Exit(true);
  end;

  _OnPath[v] := false;
  Result := false;
end;

end.
