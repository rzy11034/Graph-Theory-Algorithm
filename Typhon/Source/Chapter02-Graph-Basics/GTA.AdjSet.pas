unit GTA.AdjSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.TreeSet,
  DeepStar.Utils,
  DeepStar.UString;

type
  TTreeSet_int = specialize TTreeSet<integer>;
  TArr_TTreeSet_int = array of TTreeSet_int;

  TAdjSet = class(TObject)
  private
    _Adj: TArr_TTreeSet_int;
    _Edge: integer;
    _Vertex: integer;
    function __GetIntArray(s: UString): TArr_int;
    procedure __ValidateVertex(vertex: integer);

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(vertex: integer): TArr_int;
    function Degree(vertex: integer): integer;
    function HasEdge(vertex, w: integer): boolean;
    function ToString: UString; reintroduce;

    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

uses
  GTA.Utils;

procedure Main;
begin
  with TAdjSet.Create(FileName('Chapter02-Graph-Basics', 'g.txt')) do
  begin
    WriteLn(ToString);
    WriteLn(Degree(0));
    Free;
  end;
end;

{ TAdjSet }

constructor TAdjSet.Create(fileName: UString);
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
      raise Exception.Create('Vertex must be non-negative');

    _Edge := Lines[1];
    if _Edge < 0 then
      raise Exception.Create('Edge must be non-negative');

    SetLength(_Adj, _Vertex);
    for i := 0 to High(_Adj) do
      _Adj[i] := TTreeSet_int.Create;

    for i := 1 to _Edge do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      __ValidateVertex(a);
      b := Lines[1];
      __ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _Adj[a].Contains(b) then raise Exception.Create('Parallel Edges are Detected!');

      _Adj[a].Add(b);
      _Adj[b].Add(a);
    end;
  finally
    strList.Free;
  end;
end;

function TAdjSet.Adj(vertex: integer): TArr_int;
begin
  __ValidateVertex(vertex);
  Result := _Adj[vertex].ToArray;
end;

function TAdjSet.Degree(vertex: integer): integer;
begin
  __ValidateVertex(vertex);
  Result := Length(Adj(vertex));
end;

destructor TAdjSet.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

function TAdjSet.Edge: integer;
begin
  Result := _Edge;
end;

function TAdjSet.HasEdge(vertex, w: integer): boolean;
begin
  __ValidateVertex(vertex);
  __ValidateVertex(w);
  Result := _Adj[vertex].Contains(w);
end;

function TAdjSet.ToString: UString;
var
  sb: TStringBuilder;
  i, temp: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d'#13, [_Vertex, _Edge]);

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

function TAdjSet.Vertex: integer;
begin
  Result := _Vertex;
end;

function TAdjSet.__GetIntArray(s: UString): TArr_int;
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

procedure TAdjSet.__ValidateVertex(vertex: integer);
begin
  if (vertex < 0) or (vertex >= _Vertex) then
    raise Exception.Create('vertex ' + vertex.ToString + 'is invalid');
end;

end.
