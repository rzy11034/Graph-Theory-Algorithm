﻿unit GTA.GraphDFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TGraphDFS = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _preOrder: TArrayList_int;
    _postOrder: TArrayList_int;

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
  g: IGraph;
  graphDFS: TGraphDFS;
begin
  g := TGraph.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));
  graphDFS := TGraphDFS.Create(g);

  TArrayUtils_int.Print(graphDFS.PreOrder);
  TArrayUtils_int.Print(graphDFS.PostOrder);

  graphDFS.Free;
end;

{ TGraphDFS }

constructor TGraphDFS.Create(g: IGraph);
var
  v: integer;
begin
  _g := g;
  SetLength(_visited, g.Vertex);
  _preOrder := TArrayList_int.Create;
  _postOrder := TArrayList_int.Create;

  for v := 0 to g.Vertex - 1 do
  begin
    if not _visited[v] then
      __Dfs(v);
  end;
end;

destructor TGraphDFS.Destroy;
begin
  _preOrder.Free;
  _postOrder.Free;
  inherited Destroy;
end;

function TGraphDFS.PostOrder: TArr_int;
begin
  Result := _postOrder.ToArray;
end;

function TGraphDFS.PreOrder: TArr_int;
begin
  Result := _preOrder.ToArray;
end;

procedure TGraphDFS.__Dfs(v: integer);
var
  w: integer;
begin
  _visited[v] := true;
  _preOrder.AddLast(v);

  for w in _g.Adj(v) do
  begin
    if _visited[w] <> true then
      __dfs(w);
  end;

  _postOrder.AddLast(v);
end;

end.
