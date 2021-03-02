unit GTA.WeightedGraph;

{$mode objfpc}{$H+}
{$WARN 3018 off : Constructor should be public}
interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  DeepStar.Utils,
  DeepStar.UString;

type
  /// 暂时只支持无向带权图
  TWeightedGraph = class(TInterfacedObject, IWeightedGraph)
  public type
    TArr_TreeMap_int_int = array of TTreeMap_int_int;

  private
    _Adj: TArr_TreeMap_int_int;
    _Edge: integer;
    _Vertex: integer;

    function __GetIntArray(s: UString): TArr_int;
    constructor __Create();

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    procedure RemoveEdge(v, w: integer);
    function Clone: TWeightedGraph;
    function GetWeight(v, w: integer): integer;
    function IsDirected: boolean;

    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

uses
  GTA.Utils;

procedure Main;
begin
  with TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt')) do
  begin
    WriteLn(ToString);
    //WriteLn(Degree(0));
    Free;
  end;
end;

{ TWeightedGraph }

constructor TWeightedGraph.Create(fileName: UString);
var
  Lines: TArr_int;
  strList: TStringList;
  a, b, weight, i: integer;
begin
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
      _Adj[i] := TTreeMap_int_int.Create;

    for i := 1 to _Edge do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      ValidateVertex(a);
      b := Lines[1];
      ValidateVertex(b);
      weight := Lines[2];

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _Adj[a].ContainsKey(b) then raise Exception.Create('Parallel Edges are Detected!');

      _Adj[a].Add(b, weight);
      _Adj[b].Add(a, weight);
    end;
  finally
    strList.Free;
  end;
end;

constructor TWeightedGraph.__Create();
begin
  inherited Create;
end;

function TWeightedGraph.Adj(v: integer): TArr_int;
begin
  ValidateVertex(v);
  Result := _Adj[v].Keys;
end;

function TWeightedGraph.Clone: TWeightedGraph;
var
  v, w, weight: integer;
begin
  Result := TWeightedGraph.__Create;

  with Result do
  begin
    SetLength(_Adj, Self._Vertex);
    for v := 0 to High(Self._Adj) do
    begin
      _Adj[v] := TTreeMap_int_int.Create;

      for w in Self._Adj[v].Keys do
      begin
        weight := Self.GetWeight(v, w);
        _Adj[v].Add(w, weight);
      end;
    end;

    _Edge := self._Edge;
    _Vertex := self._Vertex;
  end;
end;

function TWeightedGraph.Degree(v: integer): integer;
begin
  ValidateVertex(v);
  Result := Length(Adj(v));
end;

destructor TWeightedGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

function TWeightedGraph.Edge: integer;
begin
  Result := _Edge;
end;

function TWeightedGraph.GetWeight(v, w: integer): integer;
begin
  if HasEdge(v, w) then
    Result := _Adj[v][w]
  else
    raise Exception.Create(Format('Edge ''%d-%d'' is invalid', [v, w]));
end;

function TWeightedGraph.HasEdge(v, w: integer): boolean;
begin
  ValidateVertex(v);
  ValidateVertex(w);
  Result := _Adj[v].ContainsKey(w);
end;

function TWeightedGraph.IsDirected: boolean;
begin
  Result := false;
end;

procedure TWeightedGraph.RemoveEdge(v, w: integer);
begin
  ValidateVertex(v);
  ValidateVertex(w);

  if _Adj[v].ContainsKey(w) then
    Edge -= 1;

  _Adj[v].Remove(w);
  _Adj[w].Remove(v);
end;

function TWeightedGraph.ToString: UString;
var
  sb: TStringBuilder;
  i, temp, weight: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d', [_Vertex, _Edge]).AppendLine;

    for i := 0 to High(_Adj) do
    begin
      sb.Append(Format('%d : ', [i]));

      for temp in _Adj[i].Keys do
      begin
        weight := Self.GetWeight(i, temp);
        sb.Append('(').Append(temp).Append(':').Append(weight).Append(') ');
      end;

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TWeightedGraph.ValidateVertex(v: integer);
begin
  if (v < 0) or (v >= _Vertex) then
    raise Exception.Create('vertex ' + v.ToString + 'is invalid');
end;

function TWeightedGraph.Vertex: integer;
begin
  Result := _Vertex;
end;

function TWeightedGraph.__GetIntArray(s: UString): TArr_int;
const
  CHARS: TSysCharSet = ['0' .. '9', '.', '+', '-'];
var
  sb: TStringBuilder;
  list: TArrayList_int;
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
    FreeAndNil(list);
    FreeAndNil(sb);
  end;
end;

end.
