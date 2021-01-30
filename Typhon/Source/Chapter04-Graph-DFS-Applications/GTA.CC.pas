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
    _g: IGraph;
    _visited: TArr_int;
    _cccount: integer;

    procedure __dfs(v, ccid: integer);

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
  _g := g;
  SetLength(_visited, g.V);
  TArrayUtils_int.FillArray(_visited, -1);
  _cccount := 0;

  for v := 0 to g.V - 1 do
  begin
    if _visited[v] = -1 then
    begin
      __dfs(v, _cccount);
      _cccount += 1;
    end;
  end;

end;

function TCC.Components: TArr_TList_int;
var
  res: TArr_TList_int;
  i, v: integer;
begin
  SetLength(res, _cccount);

  for i := 0 to _cccount - 1 do
    res[i] := TArrayList_int.Create;

  for v := 0 to _g.V - 1 do
    res[_visited[v]].AddLast(v);

  Result := res;
end;

function TCC.Count: integer;
begin
  //TArrayUtils_int.Print(_visited);
  Result := _cccount;
end;

destructor TCC.Destroy;
begin
  inherited Destroy;
end;

function TCC.IsConnected(v, w: integer): boolean;
begin
  _g.ValidateVertex(v);
  _g.ValidateVertex(w);

  Result := _visited[v] = _visited[w];
end;

procedure TCC.__dfs(v, ccid: integer);
var
  temp: integer;
begin
  _visited[v] := ccid;

  for temp in _g.Adj(v) do
  begin
    if _visited[temp] = -1 then
      __dfs(temp, ccid);
  end;
end;

end.
