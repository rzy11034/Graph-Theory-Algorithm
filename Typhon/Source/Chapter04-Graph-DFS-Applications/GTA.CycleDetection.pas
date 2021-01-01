unit GTA.CycleDetection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TCycleDetection = class(TObject)
  private
    _g: IGraph;
    _visited: TArr_bool;
    _preOrder: TList_int;
    _postOrder: TList_int;

    procedure __dfs(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function PreOrder: TArr_int;
    function PostOrder: TArr_int;
  end;

procedure Main;          
  g := TGraph.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));

implementation

procedure Main;
var
  g: IGraph;
  graphDFS: TCycleDetection;
begin
  g := TGraph.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));
  graphDFS := TCycleDetection.Create(g);

  TArrayUtils_int.Print(graphDFS.PreOrder);
  TArrayUtils_int.Print(graphDFS.PostOrder);

  graphDFS.Free;
end;

{ TCycleDetection }

constructor TCycleDetection.Create(g: IGraph);
var
  v: integer;
begin
  _g := g;
  SetLength(_visited, g.V);
  _preOrder := TList_int.Create;
  _postOrder := TList_int.Create;

  for v := 0 to g.V - 1 do
  begin
    if not _visited[v] then
      __dfs(v);
  end;

end;

destructor TCycleDetection.Destroy;
begin
  _preOrder.Free;
  _postOrder.Free;
  inherited Destroy;
end;

function TCycleDetection.PostOrder: TArr_int;
begin
  Result := _postOrder.ToArray;
end;

function TCycleDetection.PreOrder: TArr_int;
begin
  Result := _preOrder.ToArray;
end;

procedure TCycleDetection.__dfs(v: integer);
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

