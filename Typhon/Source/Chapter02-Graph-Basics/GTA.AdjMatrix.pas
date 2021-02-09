unit GTA.AdjMatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils,
  DeepStar.UString;

type
  TAdjMatrix = class(TInterfacedObject, IGraph)
  private
    _Adj: TArr2D_int;
    _Edge: integer;
    _Vertex: integer;
    function __GetIntArray(s: UString): TArr_int;

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(Vertex: integer): TArr_int;
    function Degree(Vertex: integer): integer;
    function HasEdge(Vertex, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(Vertex: integer);
    function Vertex: integer;
    function Edge: integer;
  end;

procedure Main;

implementation

procedure Main;
begin
  with TAdjMatrix.Create(FileName('Chapter02-Graph-Basics', 'g.txt')) do
  begin
    WriteLn(ToString);
    WriteLn(Degree(0));
    Free;
  end;
end;

{ TAdjMatrix }

constructor TAdjMatrix.Create(fileName: UString);
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

    SetLength(_Adj, _Vertex, _Vertex);

    for i := 1 to _Edge do
    begin
      Lines := __GetIntArray(strList[i]);

      a := Lines[0];
      ValidateVertex(a);
      b := Lines[1];
      ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _Adj[a, b] = 1 then raise Exception.Create('Parallel Edges are Detected!');

      _Adj[a, b] := 1;
      _Adj[b, a] := 1;
    end;
  finally
    strList.Free;
  end;
end;

function TAdjMatrix.Adj(Vertex: integer): TArr_int;
var
  res: TArrayList_int;
  i: integer;
begin
  ValidateVertex(Vertex);
  res := TArrayList_int.Create;
  try
    for i := 0 to _Vertex - 1 do
    begin
      if _Adj[Vertex, i] = 1 then
        res.AddLast(i);
    end;

    Result := res.ToArray;
  finally
    res.Free;
  end;
end;

function TAdjMatrix.Degree(Vertex: integer): integer;
begin
  ValidateVertex(Vertex);
  Result := Length(Adj(Vertex));
end;

destructor TAdjMatrix.Destroy;
begin
  inherited Destroy;
end;

function TAdjMatrix.Edge: integer;
begin
  Result := _Edge;
end;

function TAdjMatrix.HasEdge(Vertex, w: integer): boolean;
begin
  ValidateVertex(Vertex);
  ValidateVertex(w);
  Result := _Adj[Vertex, w] = 1;
end;

function TAdjMatrix.ToString: UString;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Vertex = %d, Edge = %d'#13, [_Vertex, _Edge]);

    for i := 0 to High(_Adj) do
    begin
      for j := 0 to High(_Adj[i]) do
      begin
        sb.Append(_Adj[i, j]).Append(' ');
      end;

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TAdjMatrix.Vertex: integer;
begin
  Result := _Vertex;
end;

function TAdjMatrix.__GetIntArray(s: UString): TArr_int;
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

procedure TAdjMatrix.ValidateVertex(Vertex: integer);
begin
  if (Vertex < 0) or (Vertex >= _Vertex) then
    raise Exception.Create('vertex ' + Vertex.ToString + 'is invalid');
end;

end.
