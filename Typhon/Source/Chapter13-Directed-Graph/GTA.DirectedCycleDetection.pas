unit GTA.DirectedCycleDetection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TDirectedCycleDetection = class(TObject)
  private
    _Graph: TGraph;
    _Visited: TArr_bool;
    _OnPath: TArr_bool;
    _HasCycle: boolean;

    // 从顶点 v 开始，判断图中是否有环
    function __DFS(v, parent: integer): boolean;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    property HasCycle: boolean read _HasCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TDirectedCycleDetection.Create(g) do
  begin
    WriteLn(HasCycle);
    Free;
  end;

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug2.txt'), true);
  with TDirectedCycleDetection.Create(g) do
  begin
    WriteLn(HasCycle);
    Free;
  end;
end;

{ TDirectedCycleDetection }

constructor TDirectedCycleDetection.Create(g: IGraph);
var
  v: integer;
begin
  if not g.IsDirected then
    raise Exception.Create('DirectedCycleDetection only works in directed graph.');

  _Graph := g as TGraph;
  _HasCycle := false;
  SetLength(_Visited, g.Vertex);
  SetLength(_OnPath, g.Vertex);

  for v := 0 to g.Vertex - 1 do
  begin
    if not _Visited[v] then
      if __DFS(v, v) then
      begin
        _HasCycle := true;
        Break;
      end;
  end;
end;

destructor TDirectedCycleDetection.Destroy;
begin
  inherited Destroy;
end;

function TDirectedCycleDetection.__DFS(v, parent: integer): boolean;
var
  w: integer;
begin
  _Visited[v] := true;
  _OnPath[v] := true;

  for w in _Graph.Adj(v) do
  begin
    if _Visited[w] <> true then
    begin
      if __DFS(w, v) then
        Exit(true);
    end
    else if _OnPath[w] then
      Exit(true);
  end;

  _OnPath[v] := false;
  Result := false;
end;

end.
