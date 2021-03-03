unit GTA.SCC;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TSCC = class(TObject)
  public type
    TArr_TList_int = array of TArrayList_int;

  private
    _Graph: IGraph;
    _Visited: TArr_int;
    _SCCount: integer;

    procedure __Dfs(v, ccid: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function Count: integer;
    function IsConnected(v, w: integer): boolean;
    function Components: TArr_TList_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
  scc: TSCC;
  comp: TSCC.TArr_TList_int;
  ccid: integer;
begin
  g := TGraph.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));
  scc := TSCC.Create(g);

  WriteLn(scc.Count);
  WriteLn(scc.IsConnected(0, 6));
  WriteLn(scc.IsConnected(0, 5));

  comp := scc.Components;
  for ccid := 0 to High(comp) do
  begin
    Write(ccid, ': ');
    TArrayUtils_int.Print(comp[ccid].ToArray);
    comp[ccid].Free;
  end;

  scc.Free;
end;

{ TSCC }

constructor TSCC.Create(g: IGraph);
var
  v: integer;
begin
  if not g.IsDirected then
    raise Exception.Create('SCC only works in directed graph');

  _Graph := g;
  TArrayUtils_int.SetLengthAndFill(_Visited, g.Vertex, -1);
  _SCCount := 0;

  for v := 0 to g.Vertex - 1 do
  begin
    if _Visited[v] = -1 then
    begin
      __Dfs(v, _SCCount);
      _SCCount += 1;
    end;
  end;

end;

function TSCC.Components: TArr_TList_int;
var
  res: TArr_TList_int;
  i, v: integer;
begin
  SetLength(res, _SCCount);

  for i := 0 to _SCCount - 1 do
    res[i] := TArrayList_int.Create;

  for v := 0 to _Graph.Vertex - 1 do
    res[_Visited[v]].AddLast(v);

  Result := res;
end;

function TSCC.Count: integer;
begin
  Result := _SCCount;
end;

destructor TSCC.Destroy;
begin
  inherited Destroy;
end;

function TSCC.IsConnected(v, w: integer): boolean;
begin
  _Graph.ValidateVertex(v);
  _Graph.ValidateVertex(w);

  Result := _Visited[v] = _Visited[w];
end;

procedure TSCC.__Dfs(v, ccid: integer);
var
  temp: integer;
begin
  _Visited[v] := ccid;

  for temp in _Graph.Adj(v) do
  begin
    if _Visited[temp] = -1 then
      __Dfs(temp, ccid);
  end;
end;

end.
