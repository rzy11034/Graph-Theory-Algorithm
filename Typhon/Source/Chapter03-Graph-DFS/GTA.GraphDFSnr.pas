unit GTA.GraphDFSnr;

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

    procedure __dfs(v: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function PreOrder: TArr_int;
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

  for v := 0 to g.Vertex - 1 do
  begin
    if not _visited[v] then
      __dfs(v);
  end;
end;

destructor TGraphDFS.Destroy;
begin
  _preOrder.Free;
  inherited Destroy;
end;

function TGraphDFS.PreOrder: TArr_int;
begin
  Result := _preOrder.ToArray;
end;

procedure TGraphDFS.__dfs(v: integer);
var
  cur, temp: integer;
  stack: TStack_int;
begin
  stack := TStack_int.Create;
  try
    stack.Push(v);

    while not stack.IsEmpty do
    begin
      cur := stack.Pop;
      _visited[cur] := true;
      _preOrder.AddLast(cur);

      for temp in _g.Adj(cur) do
      begin
        if not _visited[temp] then
          stack.Push(temp);
      end;
    end;
  finally
    stack.Free;
  end;
end;

end.
