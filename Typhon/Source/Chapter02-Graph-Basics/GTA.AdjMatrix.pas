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
    _adj: TArr2D_int;
    _e: integer;
    _v: integer;
    function __getIntArray(s: UString): TArr_int;

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;

    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    function V: integer;
    function E: integer;
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
    Lines := __getIntArray(strList[0]);

    _v := Lines[0];
    if _v < 0 then
      raise Exception.Create('V must be non-negative');

    _e := Lines[1];
    if _e < 0 then
      raise Exception.Create('E must be non-negative');

    SetLength(_adj, _v, _v);

    for i := 1 to _e do
    begin
      Lines := __getIntArray(strList[i]);

      a := Lines[0];
      ValidateVertex(a);
      b := Lines[1];
      ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _adj[a, b] = 1 then raise Exception.Create('Parallel Edges are Detected!');

      _adj[a, b] := 1;
      _adj[b, a] := 1;
    end;
  finally
    strList.Free;
  end;
end;

function TAdjMatrix.Adj(v: integer): TArr_int;
var
  res: TArrayList_int;
  i: integer;
begin
  ValidateVertex(v);
  res := TArrayList_int.Create;
  try
    for i := 0 to _v - 1 do
    begin
      if _adj[v, i] = 1 then
        res.AddLast(i);
    end;

    Result := res.ToArray;
  finally
    res.Free;
  end;
end;

function TAdjMatrix.Degree(v: integer): integer;
begin
  ValidateVertex(v);
  Result := Length(Adj(v));
end;

destructor TAdjMatrix.Destroy;
begin
  inherited Destroy;
end;

function TAdjMatrix.E: integer;
begin
  Result := _e;
end;

function TAdjMatrix.HasEdge(v, w: integer): boolean;
begin
  ValidateVertex(v);
  ValidateVertex(w);
  Result := _adj[v, w] = 1;
end;

function TAdjMatrix.ToString: UString;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('V = %d, E = %d'#13, [_v, _e]);

    for i := 0 to High(_adj) do
    begin
      for j := 0 to High(_adj[i]) do
      begin
        sb.Append(_adj[i, j]).Append(' ');
      end;

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TAdjMatrix.V: integer;
begin
  Result := _v;
end;

function TAdjMatrix.__getIntArray(s: UString): TArr_int;
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

procedure TAdjMatrix.ValidateVertex(v: integer);
begin
  if (v < 0) or (v >= _v) then
    raise Exception.Create('vertex ' + v.ToString + 'is invalid');
end;

end.
