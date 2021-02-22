unit GTA.DGraphDFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TGraphDFS = class(TObject)
  private
    _Graph: TGraph;
    _Visited: TArr_bool;
    _PreOrder: IList_int;
    _PostOrder: IList_int;

    procedure __Dfs(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function PreOrder: TArr_int;
    function PostOrder: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TGraphDFS.Create(g) do
  begin
    TArrayUtils_int.Print(PreOrder);
    TArrayUtils_int.Print(PostOrder);

    Free;
  end;
end;

{ TGraphDFS }

constructor TGraphDFS.Create(g: IGraph);
var
  v: integer;
begin
  _Graph := g as TGraph;
  SetLength(_Visited, _Graph.Vertex);
  _PreOrder := TArrayList_int.Create;
  _PostOrder := TArrayList_int.Create;

  for v := 0 to _Graph.Vertex - 1 do
    if not _Visited[v] then
      __Dfs(v);
end;

destructor TGraphDFS.Destroy;
begin
  inherited Destroy;
end;

function TGraphDFS.PostOrder: TArr_int;
begin
  Result := _PostOrder.ToArray;
end;

function TGraphDFS.PreOrder: TArr_int;
begin
  Result := _PreOrder.ToArray;
end;

procedure TGraphDFS.__Dfs(v: integer);
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
