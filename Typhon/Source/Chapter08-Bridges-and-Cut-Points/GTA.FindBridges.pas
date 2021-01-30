unit GTA.FindBridges;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils,
  DeepStar.UString,
  DeepStar.DSA.Linear.ArrayList,
  GTA.Edge,
  GTA.Utils;

type
  TFindBridges = class(TObject)
  private type
    TArrayList_Edge = specialize TArrayList<TEdge>;

  private
    _G: IGraph;

    _Visited: TArr_bool;
    _Ord, _Low: TArr_int;
    _Cnt: integer;
    _Ret: TArrayList_Edge;

    procedure __Dfs(v: integer; parent: integer);

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function Return: TArr_str;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
  chapter: UString = 'Chapter08-Bridges-and-Cut-Points';
  fb: TFindBridges;
begin
  g := TGraph.Create(FileName(chapter, 'g.txt'));
  fb := TFindBridges.Create(g);
  Write('Bridges in g : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'g2.txt'));
  fb := TFindBridges.Create(g);
  Write('Bridges in g2 : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'g3.txt'));
  fb := TFindBridges.Create(g);
  Write('Bridges in g3 : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'tree.txt'));
  fb := TFindBridges.Create(g);
  Write('Bridges in tree : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;
end;

{ TFindBridges }

constructor TFindBridges.Create(g: IGraph);
var
  v: integer;
begin
  _G := g as TGraph;

  _ret := TArrayList_Edge.Create;
  SetLength(_Visited, _G.V);
  SetLength(_Ord, _G.V);
  SetLength(_Low, _G.V);
  _Cnt := 0;

  for v := 0 to _G.V - 1 do
    if not _Visited[v] then
      __Dfs(v, v);
end;

destructor TFindBridges.Destroy;
var
  i: integer;
begin
  for i := 0 to _Ret.Count - 1 do
    _Ret[i].Free;

  _Ret.Free;

  inherited Destroy;
end;

function TFindBridges.Return: TArr_str;
var
  res: TArr_str;
  i: integer;
begin
  SetLength(res, _Ret.Count);

  for i := 0 to _Ret.Count - 1 do
    res[i] := _Ret[i].ToString;

  Result := res;
end;

procedure TFindBridges.__Dfs(v: integer; parent: integer);
var
  w: integer;
begin
  _Visited[v] := true;
  _Ord[v] := _Cnt;
  _Low[v] := _Ord[v];
  _Cnt += 1;

  for w in _G.Adj(v) do
  begin
    if not _Visited[w] then
    begin
      __Dfs(w, v);
      _Low[v] := Min(_Low[v], _Low[v]);
      if _Low[w] > _Ord[v] then
        _Ret.AddLast(TEdge.Create(v, w));
    end
    else if w <> parent then
      _Low[v] := Min(_Low[v], _Low[w]);
  end;
end;

end.
