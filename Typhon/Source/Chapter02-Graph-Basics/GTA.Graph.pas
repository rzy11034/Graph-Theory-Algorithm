unit GTA.Graph;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  DeepStar.DSA.Tree.TreeSet,
  DeepStar.Utils,
  DeepStar.UString;

type
  /// 暂时只支持无向无权图
  TGraph = class(TInterfacedObject, IGraph)
  public type
    TTreeSet_int = specialize TTreeSet<integer>;
    TArr_TTreeSet_int = array of TTreeSet_int;

  private
    _Adj: TArr_TTreeSet_int;
    _Edge: integer;
    _Vertex: integer;
    function __GetIntArray(s: UString): TArr_int;

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);

    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

uses
  GTA.Utils;

procedure Main;
begin
  with TGraph.Create(FileName('Chapter02-Graph-Basics', 'g.txt')) do
  begin
    WriteLn(ToString);
    WriteLn(Degree(0));
    Free;
  end;
end;

{ TGraph }

constructor TGraph.Create(fileName: UString);
var
  Lines: TArr_int;
  strList: TStringList;
  a, b, i: integer;
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
      _Adj[i] := TTreeSet_int.Create;

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
      _Adj[b].Add(a);
    end;
  finally
    strList.Free;
  end;
end;

function TGraph.Adj(v: integer): TArr_int;
begin
  ValidateVertex(v);
  Result := _Adj[v].ToArray;
end;

function TGraph.Degree(v: integer): integer;
begin
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

function TGraph.ToString: UString;
var
  sb: TStringBuilder;
  i, temp: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('V = %d, E = %d'#13, [_Vertex, _Edge]);

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

function TGraph.Vertex: integer;
begin
  Result := _Vertex;
end;

function TGraph.__GetIntArray(s: UString): TArr_int;
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

procedure TGraph.ValidateVertex(v: integer);
begin
  if (v < 0) or (v >= _Vertex) then
    raise Exception.Create('vertex ' + v.ToString + 'is invalid');
end;

end.
