unit GTA.AdjList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Linear.LinkedList,
  DeepStar.Utils,
  DeepStar.UString;

type
  TLinkedList_int = specialize TLinkedList<integer>;
  TArr_TLinkedList_int = array of TLinkedList_int;

  TAdjList = class(TObject)
  private
    _Adj: TArr_TLinkedList_int;
    _Edge: integer;
    _Vertex: integer;
    function __GetIntArray(s: UString): TArr_int;
    procedure __ValidateVertex(vertex: integer);

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(vertex: integer): IList_int;
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
  with TAdjList.Create(FileName('Chapter02-Graph-Basics', 'g.txt')) do
  begin
    WriteLn(ToString);
    WriteLn(Degree(0));
    Free;
  end;
end;

{ TAdjList }

constructor TAdjList.Create(fileName: UString);
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
      _Adj[i] := TLinkedList_int.Create;

    for i := 1 to _Edge do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      __ValidateVertex(a);
      b := Lines[1];
      __ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _Adj[a].Contains(b) then raise Exception.Create('Parallel Edges are Detected!');

      _Adj[a].AddLast(b);
      _Adj[b].AddLast(a);
    end;
  finally
    strList.Free;
  end;
end;

function TAdjList.Adj(vertex: integer): IList_int;
var
  res: IList_int;
  i: integer;
begin
  __ValidateVertex(vertex);
  res := TArrayList_int.Create;

  for i := 0 to _Adj[vertex].Count - 1 do
    res.AddLast(_Adj[vertex][i]);

  Result := res;
end;

function TAdjList.Degree(vertex: integer): integer;
begin
  __ValidateVertex(vertex);

  with Adj(vertex) do
  begin
    Result := Count;
    Free;
  end;
end;

destructor TAdjList.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

function TAdjList.Edge: integer;
begin
  Result := _Edge;
end;

function TAdjList.HasEdge(vertex, w: integer): boolean;
begin
  __ValidateVertex(vertex);
  __ValidateVertex(w);
  Result := _Adj[vertex].Contains(w);
end;

function TAdjList.ToString: UString;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d'#13, [_Vertex, _Edge]);

    for i := 0 to High(_Adj) do
    begin
      sb.Append(Format('%d : ', [i]));

      for j := 0 to _Adj[i].Count - 1 do
        sb.Append(_Adj[i][j]).Append(' ');

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TAdjList.Vertex: integer;
begin
  Result := _Vertex;
end;

function TAdjList.__GetIntArray(s: UString): TArr_int;
const
  CHARS: TSysCharSet = ['0' .. '9', '.', '+', '-'];
var
  sb: TStringBuilder;
  list: TLinkedList_int;
  i: integer;
begin
  sb := TStringBuilder.Create;
  list := TLinkedList_int.Create;
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

procedure TAdjList.__ValidateVertex(vertex: integer);
begin
  if (vertex < 0) or (vertex >= _Vertex) then
    raise Exception.Create('vertex ' + vertex.ToString + 'is invalid');
end;

end.
