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
    _adj: TArr_TTreeSet_int;
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
    Lines := __getIntArray(strList[0]);

    _v := Lines[0];
    if _v < 0 then
      raise Exception.Create('V must be non-negative');

    _e := Lines[1];
    if _e < 0 then
      raise Exception.Create('E must be non-negative');

    SetLength(_adj, _v);
    for i := 0 to High(_adj) do
      _adj[i] := TTreeSet_int.Create;

    for i := 1 to _e do
    begin
      Lines := __getIntArray(strList[i]);

      a := Lines[0];
      ValidateVertex(a);
      b := Lines[1];
      ValidateVertex(b);

      if a = b then raise Exception.Create('Self Loop is Detected!');
      if _adj[a].Contains(b) then raise Exception.Create('Parallel Edges are Detected!');

      _adj[a].Add(b);
      _adj[b].Add(a);
    end;
  finally
    strList.Free;
  end;
end;

function TGraph.Adj(v: integer): TArr_int;
begin
  ValidateVertex(v);
  Result := _adj[v].ToArray;
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
  for i := 0 to High(_adj) do
    _adj[i].Free;

  inherited Destroy;
end;

function TGraph.E: integer;
begin
  Result := _e;
end;

function TGraph.HasEdge(v, w: integer): boolean;
begin
  ValidateVertex(v);
  ValidateVertex(w);
  Result := _adj[v].Contains(w);
end;

function TGraph.ToString: UString;
var
  sb: TStringBuilder;
  i, temp: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('V = %d, E = %d'#13, [_v, _e]);

    for i := 0 to High(_adj) do
    begin
      sb.Append(Format('%d : ', [i]));

      for temp in _adj[i].ToArray do
        sb.Append(temp).Append(' ');

      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TGraph.V: integer;
begin
  Result := _v;
end;

function TGraph.__getIntArray(s: UString): TArr_int;
const
  CHARS: TSysCharSet = ['0' .. '9', '.', '+', '-'];
var
  sb: TStringBuilder;
  list: TList_int;
  i: integer;
begin
  sb := TStringBuilder.Create;
  list := TList_int.Create;
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
  if (v < 0) or (v >= _v) then
    raise Exception.Create('vertex ' + v.ToString + 'is invalid');
end;

end.
