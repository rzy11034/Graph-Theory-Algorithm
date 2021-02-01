unit GTA.Leetcode_1192;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils;

type
  // 1192. 查找集群内的「关键连接」
  TSolution = class(TObject)
  private type
    TGraph = class(TObject)
    private
      _AdjList: array of IList_int;
      _Vertex: integer;
      _Edge: integer;

    public
      constructor Create(n: integer; const connections: TArr2D_int);
      destructor Destroy; override;

      function Adj(v: integer): TArr_int;
      function Vertex: integer;
      function Edge: integer;
    end;

  private
    _Graph: TGraph;
    _Visited: TArr_bool;
    _Ord: TArr_int;
    _Low: TArr_int;
    _Cnt: integer;
    _Ret: IList_TArr_int;

    procedure __FindBridges;
    procedure __Dfs(v: integer; parent: integer);

  public
    function CriticalConnections(n: integer; connections: TArr2D_int): TArr2D_int;
  end;

procedure Main;

implementation

procedure Main;
var
  res: TArr2D_int;
begin
  with TSolution.Create do
  begin
    res := CriticalConnections(4, [[0, 1], [1, 2], [2, 0], [1, 3]]);
    TArrayUtils_int.Print2D(res);
    Free;
  end;
end;

{ TSolution.TGraph }

constructor TSolution.TGraph.Create(n: integer; const connections: TArr2D_int);
var
  i, v, w: integer;
begin
  Self._Vertex := n;
  Self._Edge := 0;

  SetLength(_AdjList, n);
  for i := 0 to n - 1 do
    _AdjList[i] := TArrayList_int.Create;

  for i := 0 to High(connections) do
  begin
    v := connections[i, 0];
    w := connections[i, 1];

    if _AdjList[v].Contains(w) then Break;

    _AdjList[v].AddLast(w);
    _AdjList[w].AddLast(v);
    _Edge += 1;
  end;
end;

function TSolution.TGraph.Adj(v: integer): TArr_int;
begin
  Result := _AdjList[v].ToArray;
end;

destructor TSolution.TGraph.Destroy;
begin
  inherited Destroy;
end;

function TSolution.TGraph.Edge: integer;
begin
  Result := _Edge;
end;

function TSolution.TGraph.Vertex: integer;
begin
  Result := _Vertex;
end;

{ TSolution }

function TSolution.CriticalConnections(n: integer; connections: TArr2D_int): TArr2D_int;
var
  res: TArr2D_int;
  i: Integer;
begin
  _Graph := TGraph.Create(n, connections);

  //{$ Debug}
  for i := 0 to _Graph.Vertex-1 do
    TArrayUtils_int.Print(_Graph.Adj(i));
  //{$EndIf Debug}

  try
    SetLength(_Visited, n);
    SetLength(_Ord, n);
    SetLength(_Low, n);
    _Cnt := 0;
    _Ret := TArrayList_TArr_int.Create;

    __FindBridges;

    res := _Ret.ToArray;
    Result := res;
  finally
    _Graph.Free;
  end;
end;

procedure TSolution.__Dfs(v: integer; parent: integer);
var
  w: integer;
  temp: IList_int;
  adj: TArr_int;
begin
  _Visited[v] := true;
  _Ord[v] := _Cnt;
  _Low[v] := _Ord[v];
  _Cnt += 1;

  adj := _Graph.Adj(v);
  for w in _Graph.Adj(v) do
  begin
    if not _Visited[w] then
    begin
      __Dfs(w, v);
      _Low[v] := Min(_Low[v], _Low[w]);

      if (_Low[w] > _Ord[v]) then
      begin
        temp := TArrayList_int.Create;
        temp.AddLast(v);
        temp.AddLast(w);
        _Ret.AddLast(temp.ToArray);
      end
      else if w <> parent then
        _Low[v] := Min(_Low[v], _Low[w]);
    end;
  end;
end;

procedure TSolution.__FindBridges;
var
  v: integer;
begin
  for v := 0 to _Graph.Vertex - 1 do
  begin
    if not _Visited[v] then
      __Dfs(v, v);
  end;
end;

end.
