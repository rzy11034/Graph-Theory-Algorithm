unit GTA.SCC;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.DGraphDFS,
  DeepStar.Utils;

type
  TSCC = class(TObject)
  public type
    TArr_TList_int = array of TArrayList_int;

  private
    _Graph: TGraph;
    _Visited: TArr_int;
    _SCCount: integer;

    procedure __Dfs(v, sccid: integer);

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
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'sug.txt'), true);
  scc := TSCC.Create(g);
  WriteLn(scc.Count);
  comp := scc.Components;
  for ccid := 0 to High(comp) do
  begin
    Write(ccid, ': ');
    TArrayUtils_int.Print(comp[ccid].ToArray);
    comp[ccid].Free;
  end;
  scc.Free;
  DrawLineBlockEnd;

  ////////////////////////////////////////////

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'sug2.txt'), true);
  scc := TSCC.Create(g);
  WriteLn(scc.Count);
  comp := scc.Components;
  for ccid := 0 to High(comp) do
  begin
    Write(ccid, ': ');
    TArrayUtils_int.Print(comp[ccid].ToArray);
    comp[ccid].Free;
  end;
  scc.Free;
  DrawLineBlockEnd;

  //////////////////////////////////////////

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'sug3.txt'), true);
  scc := TSCC.Create(g);
  WriteLn(scc.Count);
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
  g_dfs: TGraphDFS;
  order: IList_int;
begin
  if not g.IsDirected then
    raise Exception.Create('SCC only works in directed graph');

  _Graph := g as TGraph;
  TArrayUtils_int.SetLengthAndFill(_Visited, g.Vertex, -1);
  _SCCount := 0;

  g_dfs := TGraphDFS.Create(_Graph.ReverseGraph);
  try
    order := TLinkedList_int.Create;

    for v in g_dfs.PostOrder do
      order.AddFirst(v);

    for v in order.ToArray do
    begin
      if _Visited[v] = -1 then
      begin
        __Dfs(v, _SCCount);
        _SCCount += 1;
      end;
    end;

  finally
    g_dfs.Free;
  end;
end;

function TSCC.Components: TArr_TList_int;
var
  res: TArr_TList_int;
  i, v: integer;
begin
  SetLength(res, _SCCount);

  for i := 0 to High(res) do
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

procedure TSCC.__Dfs(v, sccid: integer);
var
  temp: integer;
begin
  _Visited[v] := sccid;

  for temp in _Graph.Adj(v) do
  begin
    if _Visited[temp] = -1 then
      __Dfs(temp, sccid);
  end;
end;

end.
