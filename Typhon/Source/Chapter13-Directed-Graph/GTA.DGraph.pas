unit GTA.DGraph;

{$mode objfpc}{$H+}
{$WARN 3018 off : Constructor should be public}

interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  DeepStar.DSA.Tree.TreeSet,
  DeepStar.Utils,
  DeepStar.UString;

type
  // 无权图(支持有向，无向)
  TGraph = class(TInterfacedObject, IGraph)
  public type
    TTreeSet_int = specialize TTreeSet<integer>;
    TArr_TTreeSet_int = array of TTreeSet_int;

  private
    _Adj: TArr_TTreeSet_int;
    _Edge: integer;
    _Vertex: integer;
    _Directed: boolean;
    _inDegree, _outDegree: TArr_int;

    function __GetIntArray(s: UString): TArr_int;
    constructor __Create();

  public
    constructor Create(fileName: UString; directed: boolean = false);
    constructor Create(vertex: integer; directed: boolean);
    destructor Destroy; override;

    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    procedure AddEdge(v, w: integer);
    procedure RemoveEdge(v, w: integer);
    function Clone: TGraph;
    function IsDirected: boolean;
    function InDegree(v: integer): integer;
    function OutDegree(v: integer): integer;

    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

uses
  GTA.Utils;

procedure Main;
var
  v: integer;
begin
  with TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt')) do
  begin
    WriteLn(ToString);
    Free;
  end;

  with TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true) do
  begin
    WriteLn(ToString);

    for v := 0 to Vertex - 1 do
      WriteLn(Format('%d--> %d  %d', [v, InDegree(v), OutDegree(v)]));

    Free;
  end;
end;

{ TGraph }

constructor TGraph.Create(vertex: integer; directed: boolean);
var
  v: integer;
begin
  _Vertex := vertex;
  _Edge := 0;
  _Directed := directed;

  SetLength(_Adj, _Vertex);
  for v := 0 to High(_Adj) do
    _Adj[v] := TTreeSet_int.Create;

  SetLength(_inDegree, _Vertex);
  SetLength(_outDegree, _Vertex);
end;

constructor TGraph.Create(fileName: UString; directed: boolean);
var
  Lines: TArr_int;
  strList: TStringList;
  a, b, i: integer;
begin
  _Directed := directed;

  strList := TStringList.Create;
  try
    strList.LoadFromFile(fileName);
    Lines := __GetIntArray(strList[0]);

    _Vertex := Lines[0];
    if _Vertex < 0 then
      raise Exception.Create('V Must Be non-Negative');

    _Edge := Lines[1];
    if _Edge < 0 then
      raise Exception.Create('E must be non-negative');

    SetLength(_Adj, _Vertex);
    for i := 0 to High(_Adj) do
      _Adj[i] := TTreeSet_int.Create;

    SetLength(_inDegree, _Vertex);
    SetLength(_outDegree, _Vertex);
    for i := 1 to _Edge do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      ValidateVertex(a);
      b := Lines[1];
      ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _Adj[a].Contains(b) then raise Exception.Create('Parallel Edges are Detected!');

      _Adj[a].Add(b);
      if not directed then
        _Adj[b].Add(a)
      else
      begin
        _outDegree[a] += 1;
        _inDegree[b] += 1;
      end;
    end;
  finally
    strList.Free;
  end;
end;

constructor TGraph.__Create();
begin
  inherited Create;
end;

procedure TGraph.AddEdge(v, w: integer);
begin
  _Adj[v].Add(w);
  _Edge += 1;

  if not _Directed then
    _Adj[w].Add(v)
  else
  begin
    _outDegree[v] += 1;
    _inDegree[w] += 1;
  end;
end;

function TGraph.Adj(v: integer): TArr_int;
begin
  ValidateVertex(v);
  Result := _Adj[v].ToArray;
end;

function TGraph.Clone: TGraph;
var
  v, w: integer;
begin
  Result := TGraph.__Create;

  with Result do
  begin
    SetLength(_Adj, Self._Vertex);
    for v := 0 to High(Self._Adj) do
    begin
      _Adj[v] := TTreeSet_int.Create;

      for w in Self._Adj[v].ToArray do
      begin
        _Adj[v].Add(w);
      end;
    end;

    _Edge := self._Edge;
    _Vertex := self._Vertex;
    _Directed := Self._Directed;
    _inDegree := TArrayUtils_int.CopyArray(Self._inDegree);
    _outDegree := TArrayUtils_int.CopyArray(Self._outDegree);
  end;
end;

function TGraph.Degree(v: integer): integer;
begin
  if IsDirected then
    raise Exception.Create('degree only works in undirected graph.');
  ValidateVertex(v);
  Result := Length(Adj(v));
end;

destructor TGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

function TGraph.Edge: integer;
begin
  Result := _Edge;
end;

function TGraph.HasEdge(v, w: integer): boolean;
begin
  ValidateVertex(v);
  ValidateVertex(w);
  Result := _Adj[v].Contains(w);
end;

function TGraph.InDegree(v: integer): integer;
begin
  if not IsDirected then
    raise Exception.Create('Indegree only works in directed graph.');

  ValidateVertex(v);
  Result := _inDegree[v];
end;

function TGraph.IsDirected: boolean;
begin
  Result := _Directed;
end;

function TGraph.OutDegree(v: integer): integer;
begin
  if not IsDirected then
    raise Exception.Create('Outdegree only works in directed graph.');

  ValidateVertex(v);
  Result := _outDegree[v];
end;

procedure TGraph.RemoveEdge(v, w: integer);
begin
  ValidateVertex(v);
  ValidateVertex(w);

  if _Adj[v].Contains(w) then
    Edge -= 1;

  _Adj[v].Remove(w);
  if not IsDirected then
    _Adj[w].Remove(v)
  else
  begin
    _outDegree[v] -= 1;
    _inDegree[w] -= 1;
  end;
end;

function TGraph.ToString: UString;
var
  sb: TStringBuilder;
  i, temp: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d, Directed = %s',
      [_Vertex, _Edge, BoolToStr(_Directed, 'True', 'False')]).AppendLine;

    for i := 0 to High(_Adj) do
    begin
      sb.Append(Format('%d : ', [i]));

      for temp in _Adj[i].ToArray do
        sb.Append(temp).Append(' ');

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TGraph.ValidateVertex(v: integer);
begin
  if (v < 0) or (v >= _Vertex) then
    raise Exception.Create('vertex ' + v.ToString + 'is invalid');
end;

function TGraph.Vertex: integer;
begin
  Result := _Vertex;
end;

function TGraph.__GetIntArray(s: UString): TArr_int;
const
  CHARS: TSysCharSet = ['0' .. '9', '.', '+', '-'];
var
  sb: TStringBuilder;
  list: IList_int;
  i: integer;
begin
  sb := TStringBuilder.Create;
  list := TArrayList_int.Create;
  try
    for i := 0 to s.Length - 1 do
    begin
      if s.Chars[i] in CHARS then
      begin
        sb.Append(s.Chars[i]);
      end
      else
      begin
        list.AddLast(sb.ToString.ToInteger);
        sb.Clear;
      end;

      if i = s.Length - 1 then
        list.AddLast(sb.ToString.ToInteger);
    end;

    Result := list.ToArray;
  finally
    FreeAndNil(sb);
  end;
end;

end.
