unit GTA.DWeightedGraph;

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
  // 带权图(支持有向，无向)
  TWeightedGraph = class(TInterfacedObject, IWeightedGraph)
  public type
    TArr_TreeMap_int_int = array of TTreeMap_int_int;

  private
    _Adj: TArr_TreeMap_int_int;
    _Edge: integer;
    _Vertex: integer;
    _Directed: boolean;
    _inDegree: TArr_int;
    _outDegree: TArr_int;

    function __GetIntArray(s: UString): TArr_int;
    constructor __Create();

  public
    constructor Create(fileName: UString; directed: boolean = false);
    constructor Create(vertex: integer; directed: boolean = false);
    destructor Destroy; override;

    procedure AddEdge(v, w, weighted: integer);
    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    procedure RemoveEdge(v, w: integer);
    function Clone: TWeightedGraph;
    function GetWeight(v, w: integer): integer;
    function IsDirected: boolean;
    procedure SetWeight(v, w, weighted: integer);

    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

uses
  GTA.Utils;

procedure Main;
var
  g: TWeightedGraph;
begin
  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg.txt'));
  with g do
  begin
    WriteLn(ToString);

    RemoveEdge(0, 1);
    WriteLn(ToString);
    Free;
  end;

  DrawLineBlockEnd;

  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg.txt'), true);
  with g do
  begin
    WriteLn(ToString);

    RemoveEdge(0, 1);
    WriteLn(ToString);
    Free;
  end;
end;

{ TWeightedGraph }

constructor TWeightedGraph.Create(vertex: integer; directed: boolean);
var
  i: integer;
begin
  _Vertex := vertex;
  _Directed := directed;
  SetLength(_Adj, _Vertex);

  for i := 0 to High(_Adj) do
    _Adj[i] := TTreeMap_int_int.Create;

  SetLength(_inDegree, _Vertex);
  SetLength(_outDegree, _Vertex);
end;

constructor TWeightedGraph.Create(fileName: UString; directed: boolean);
var
  Lines: TArr_int;
  strList: TStringList;
  a, b, weighted, i: integer;
begin
  _Directed := directed;
  _Edge := 0;

  strList := TStringList.Create;
  try
    strList.LoadFromFile(fileName);
    Lines := __GetIntArray(strList[0]);

    if Lines[0] < 0 then
      raise Exception.Create('V Must Be non-Negative')
    else
      _Vertex := Lines[0];

    if Lines[1] < 0 then
      raise Exception.Create('E must be non-negative')
    else
      _Edge := 0;

    SetLength(_Adj, _Vertex);
    for i := 0 to High(_Adj) do
      _Adj[i] := TTreeMap_int_int.Create;

    SetLength(_inDegree, _Vertex);
    SetLength(_outDegree, _Vertex);

    for i := 1 to strList.Count - 1 do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      b := Lines[1];
      weighted := Lines[2];

      AddEdge(a, b, weighted);
    end;
  finally
    strList.Free;
  end;
end;

constructor TWeightedGraph.__Create();
begin
  inherited Create;
end;

procedure TWeightedGraph.AddEdge(v, w, weighted: integer);
begin
  ValidateVertex(v);
  ValidateVertex(w);

  if v = w then raise Exception.Create('Self Loop is Detected!');
  if _Adj[v].ContainsKey(w) then raise Exception.Create('Parallel Edges are Detected!');

  _Adj[v].Add(w, weighted);
  _Edge += 1;

  if not _Directed then
  begin
    _Adj[w].Add(v, weighted);
    _Edge += 1;
  end
  else
  begin
    _outDegree[v] += 1;
    _inDegree[w] += 1;
  end;
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

    _Edge := Self._Edge;
    _Vertex := Self._Vertex;
    _Directed := Self._Directed;
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
  Result := _Directed;
end;

procedure TWeightedGraph.RemoveEdge(v, w: integer);
begin
  ValidateVertex(v);
  ValidateVertex(w);

  if not _Adj[v].ContainsKey(w) then Exit;

  _Adj[v].Remove(w);
  _Edge -= 1;

  if not IsDirected then
  begin
    _Adj[w].Remove(v);
    _Edge -= 1;
  end
  else
  begin
    _outDegree[v] -= 1;
    _inDegree[w] -= 1;
  end;
end;

procedure TWeightedGraph.SetWeight(v, w, weighted: integer);
begin
  if not HasEdge(v, w) then
    raise Exception.Create(Format('No edge %d-%d', [v, w]));

  _Adj[v][w] := weighted;
  if not IsDirected then
    _Adj[w][v] := weighted;
end;

function TWeightedGraph.ToString: UString;
var
  sb: TStringBuilder;
  i, temp, weight: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d, Directed = %s',
      [_Vertex, _Edge, BoolToStr(_Directed, 'True', 'False')]).AppendLine;

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
  list: IList_int;
  i: integer;
begin
  list := TArrayList_int.Create;

  sb := TStringBuilder.Create;
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
