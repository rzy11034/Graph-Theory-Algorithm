unit GTA.Hungarian_DFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.BipartitionDetection,
  DeepStar.Utils;

type
  THungarian = class(TObject)
  public type
    TColor = TBipartitionDetection.TColor;

  private
    _Graph: IGraph;
    _MaxMatching: integer;
    _Matching: TArr_int;
    _Visited: TArr_bool;

    function __DFS(v: integer): boolean;
    function __GetAugPath(pre: TArr_int; start, &end: integer): TArrayList_int;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function IsPerfectMatching: boolean;

    property MaxMatching: integer read _MaxMatching;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter15-Matching-Problem', 'g.txt'));
  with THungarian.Create(g) do
  begin
    WriteLn(MaxMatching);
    WriteLn(IsPerfectMatching);
    Free;
  end;

  DrawLineBlockEnd;

  g := TGraph.Create(FileName('Chapter15-Matching-Problem', 'g2.txt'));
  with THungarian.Create(g) do
  begin
    WriteLn(MaxMatching);
    WriteLn(IsPerfectMatching);
    Free;
  end;
end;

{ THungarian }

constructor THungarian.Create(g: IGraph);
var
  bd: TBipartitionDetection;
  colors: TBipartitionDetection.TArr_Color;
  v: integer;
begin
  bd := TBipartitionDetection.Create(g);
  try
    if not bd.IsBipartite then
      raise Exception.Create('Hungarian only works for bipartite graph.');

    _Graph := g;
    colors := bd.Colors;
  finally
    FreeAndNil(bd);
  end;

  TArrayUtils_int.SetLengthAndFill(_Matching, _Graph.Vertex, -1);
  TArrayUtils_bool.SetLengthAndFill(_Visited, _Graph.Vertex, false);

  for v := 0 to _Graph.Vertex - 1 do
  begin
    if (colors[v] = TColor.red) and (_Matching[v] = -1) then
    begin
      TArrayUtils_bool.FillArray(_Visited, false);
      if __DFS(v) then _MaxMatching += 1;
    end;
  end;
end;

destructor THungarian.Destroy;
begin
  inherited Destroy;
end;

function THungarian.IsPerfectMatching: boolean;
begin
  Result := MaxMatching * 2 = _Graph.Vertex;
end;

function THungarian.__DFS(v: integer): boolean;
var
  u: integer;
begin
  _Visited[v] := true;

  for u in _Graph.Adj(v) do
  begin
    if not _Visited[u] then
    begin
      _Visited[u] := true;

      if (_Matching[u] = -1) or (__DFS(_Matching[u])) then
      begin
        _Matching[v] := u;
        _Matching[u] := v;
        Result := true;
        Exit;
      end;
    end;
  end;

  Result := false;
end;

function THungarian.__GetAugPath(pre: TArr_int; start, &end: integer): TArrayList_int;
var
  cur: integer;
begin
  Result := TArrayList_int.Create;

  cur := &end;
  while cur <> start do
  begin
    Result.AddLast(cur);
    cur := pre[cur];
  end;
  Result.AddLast(start);
end;

end.
