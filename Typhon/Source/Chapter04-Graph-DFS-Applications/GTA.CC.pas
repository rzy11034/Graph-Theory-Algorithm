unit GTA.CC;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TCC = class(TObject)
  public type
    TArr_TList_int = array of TArrayList_int;

  private
    _G: IGraph;
    _Visited: TArr_int;
    _CCount: integer;

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
  cc: TCC;
  comp: TCC.TArr_TList_int;
  ccid: integer;
begin
  g := TGraph.Create(FileName('Chapter03-Graph-DFS', 'g.txt'));
  cc := TCC.Create(g);

  WriteLn(cc.Count);
  WriteLn(cc.IsConnected(0, 6));
  WriteLn(cc.IsConnected(0, 5));

  comp := cc.Components;
  for ccid := 0 to High(comp) do
  begin
    Write(ccid, ': ');
    TArrayUtils_int.Print(comp[ccid].ToArray);
    comp[ccid].Free;
  end;

  cc.Free;
end;

{ TCC }

constructor TCC.Create(g: IGraph);
var
  v: integer;
begin
  _G := g;
  SetLength(_Visited, g.Vertex);
  TArrayUtils_int.FillArray(_Visited, -1);
  _CCount := 0;

  for v := 0 to g.Vertex - 1 do
  begin
    if _Visited[v] = -1 then
    begin
      __Dfs(v, _CCount);
      _CCount += 1;
    end;
  end;

end;

function TCC.Components: TArr_TList_int;
var
  res: TArr_TList_int;
  i, v: integer;
begin
  SetLength(res, _CCount);

  for i := 0 to _CCount - 1 do
    res[i] := TArrayList_int.Create;

  for v := 0 to _G.Vertex - 1 do
    res[_Visited[v]].AddLast(v);

  Result := res;
end;

function TCC.Count: integer;
begin
  //TArrayUtils_int.Print(_Visited);
  Result := _CCount;
end;

destructor TCC.Destroy;
begin
  inherited Destroy;
end;

function TCC.IsConnected(v, w: integer): boolean;
begin
  _G.ValidateVertex(v);
  _G.ValidateVertex(w);

  Result := _Visited[v] = _Visited[w];
end;

procedure TCC.__Dfs(v, ccid: integer);
var
  temp: integer;
begin
  _Visited[v] := ccid;

  for temp in _G.Adj(v) do
  begin
    if _Visited[temp] = -1 then
      __Dfs(temp, ccid);
  end;
end;

end.
