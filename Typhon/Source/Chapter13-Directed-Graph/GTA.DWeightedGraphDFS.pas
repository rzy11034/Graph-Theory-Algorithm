unit GTA.DWeightedGraphDFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TWeightedGraphDFS = class(TObject)
  private
    _Graph: TWeightedGraph;
    _Visited: TArr_bool;
    _PreOrder: IList_int;
    _PostOrder: IList_int;

    procedure __Dfs(v: integer);

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function PreOrder: TArr_int;
    function PostOrder: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TWeightedGraph;
begin
  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg.txt'), true);
  with TWeightedGraphDFS.Create(g) do
  begin
    TArrayUtils_int.Print(PreOrder);
    TArrayUtils_int.Print(PostOrder);
    Free;
  end;
end;

{ TWeightedGraphDFS }

constructor TWeightedGraphDFS.Create(g: IWeightedGraph);
var
  v: integer;
begin
  _Graph := g as TWeightedGraph;
  SetLength(_Visited, g.Vertex);
  _PostOrder := TArrayList_int.Create;
  _PreOrder := TArrayList_int.Create;

  for v := 0 to g.Vertex - 1 do
    if not _Visited[v] then
      __Dfs(v);
end;

destructor TWeightedGraphDFS.Destroy;
begin
  inherited Destroy;
end;

function TWeightedGraphDFS.PostOrder: TArr_int;
begin
  Result := _PostOrder.ToArray;
end;

function TWeightedGraphDFS.PreOrder: TArr_int;
begin
  Result := _PreOrder.ToArray;
end;

procedure TWeightedGraphDFS.__Dfs(v: integer);
var
  w: integer;
begin
  _Visited[v] := true;
  _PreOrder.AddLast(v);

  for w in _Graph.Adj(v) do
    if not _Visited[w] then
      __Dfs(w);

  _PostOrder.AddLast(v);
end;

end.
