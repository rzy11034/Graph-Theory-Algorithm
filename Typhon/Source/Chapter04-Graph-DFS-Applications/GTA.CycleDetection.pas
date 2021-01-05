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
    _hasCycle: boolean;

    // 从顶点 v 开始，判断图中是否有环
    function __dfs(v, parent: integer): boolean;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    property HasCycle: boolean read _HasCycle write _HasCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  cd: TCycleDetection;
begin
  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g.txt'));
  cd := TCycleDetection.Create(g);
  WriteLn(cd._hasCycle);
  cd.Free;

  g := TGraph.Create(FileName('Chapter04-Graph-DFS-Applications', 'g2.txt'));
  cd := TCycleDetection.Create(g);
  WriteLn(cd._hasCycle);
  cd.Free;
end;

{ TCycleDetection }

constructor TCycleDetection.Create(g: IGraph);
var
  v: integer;
begin
  _g := g;
  _hasCycle := false;
  SetLength(_visited, g.V);

  for v := 0 to g.V - 1 do
  begin
    if not _visited[v] then
      if __dfs(v, v) then
      begin
        _hasCycle := true;
        Break;
      end;
  end;
end;

destructor TCycleDetection.Destroy;
begin
  inherited Destroy;
end;

function TCycleDetection.__dfs(v, parent: integer): boolean;
var
  w: integer;
begin
  _visited[v] := true;

  for w in _g.Adj(v) do
  begin
    if _visited[w] <> true then
    begin
      if __dfs(w, v) then
        Exit(true);
    end
    else if w <> parent then
      Exit(true);
  end;

  Result := false;
end;

end.

