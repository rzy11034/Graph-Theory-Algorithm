unit GTA.AdjMatrixDFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.AdjMatrix,
  GTA.Utils,
  DeepStar.Utils;

type
  TAdjMatrixDFS = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _preOrder: TArrayList_int;
    _postOrder: TArrayList_int;

    procedure __dfs(v: integer);

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
  graphDFS: TAdjMatrixDFS;
begin
  g := TAdjMatrix.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));
  graphDFS := TAdjMatrixDFS.Create(g);

  TArrayUtils_int.Print(graphDFS.PreOrder);
  TArrayUtils_int.Print(graphDFS.PostOrder);

  //g.Free;
  graphDFS.Free;
end;

{ TAdjMatrixDFS }

constructor TAdjMatrixDFS.Create(g: IGraph);
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
      __dfs(v);
  end;
end;

destructor TAdjMatrixDFS.Destroy;
begin
  _preOrder.Free;
  _postOrder.Free;
  inherited Destroy;
end;

function TAdjMatrixDFS.PostOrder: TArr_int;
begin
  Result := _postOrder.ToArray;
end;

function TAdjMatrixDFS.PreOrder: TArr_int;
begin
  Result := _preOrder.ToArray;
end;

procedure TAdjMatrixDFS.__dfs(v: integer);
var
  temp: integer;
begin
  _visited[v] := true;
  _preOrder.AddLast(v);

  for temp in _g.Adj(v) do
  begin
    if _visited[temp] <> true then
      __dfs(temp);
  end;

  _postOrder.AddLast(v);
end;

end.
