unit GTA.WeightedCC;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TWeightedCC = class(TObject)
  public type
    TArr_TList_int = array of TArrayList_int;

  private
    _G: TWeightedGraph;
    _Visited: TArr_int;
    _CCount: integer;

    procedure __Dfs(v, ccid: integer);

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Count: integer;
    function IsConnected(v, w: integer): boolean;
    function Components: TArr_TList_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IWeightedGraph;
  cc: TWeightedCC;
  comp: TWeightedCC.TArr_TList_int;
  ccid: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt'));
  cc := TWeightedCC.Create(g);

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

{ TWeightedCC }

constructor TWeightedCC.Create(g: IWeightedGraph);
var
  v: integer;
begin
  _G := (g as TWeightedGraph);
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

function TWeightedCC.Components: TArr_TList_int;
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

function TWeightedCC.Count: integer;
begin
  Result := _CCount;
end;

destructor TWeightedCC.Destroy;
begin
  inherited Destroy;
end;

function TWeightedCC.IsConnected(v, w: integer): boolean;
begin
  _G.ValidateVertex(v);
  _G.ValidateVertex(w);

  Result := _Visited[v] = _Visited[w];
end;

procedure TWeightedCC.__Dfs(v, ccid: integer);
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
