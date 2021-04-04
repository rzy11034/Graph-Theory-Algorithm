unit GTA.Hungarian;

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

    function __BFS(v: integer): boolean;
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
  for v := 0 to _Graph.Vertex - 1 do
  begin
    if (colors[v] = TColor.red) and (_Matching[v] = -1) then
      if __BFS(v) then _MaxMatching += 1;
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

function THungarian.__BFS(v: integer): boolean;
var
  queue: IQueue_int;
  pre: TArr_int;
  cur, Next, i: integer;
  augPath: TArrayList_int;
begin
  queue := TQueue_int.Create;
  TArrayUtils_int.SetLengthAndFill(pre, _Graph.Vertex, -1);

  queue.EnQueue(v);
  pre[v] := v;

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;

    for Next in _Graph.Adj(cur) do
    begin
      if pre[Next] = -1 then
      begin
        if _Matching[Next] <> -1 then
        begin
          pre[Next] := cur;
          pre[_Matching[Next]] := Next;
          queue.EnQueue(_Matching[Next]);
        end
        else
        begin
          pre[Next] := cur;
          augPath := __GetAugPath(pre, v, Next);
          try
            i := 0;
            while i < augPath.Count do
            begin
              _Matching[augPath[i]] := augPath[i + 1];
              _Matching[augPath[i + 1]] := augPath[i];
              i += 2;
            end;

            Result := true;
            Exit;
          finally
            FreeAndNil(augPath);
          end;
        end;
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
