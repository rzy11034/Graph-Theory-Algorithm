unit GTA.FindCutPoints;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils,
  DeepStar.UString,
  GTA.Utils;

type
  TFindCutPoints = class(TObject)
  private
    _G: IGraph;

    _Visited: TArr_bool;
    _Ord, _Low: TArr_int;
    _Cnt: integer;
    _Ret: ISet_int;

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
  g: IGraph;
  chapter: UString = 'Chapter08-Bridges-and-Cut-Points';
  fb: TFindCutPoints;
begin
  g := TGraph.Create(FileName(chapter, 'g.txt'));
  fb := TFindCutPoints.Create(g);
  Write('Bridges in g : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'g2.txt'));
  fb := TFindCutPoints.Create(g);
  Write('Bridges in g2 : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'g3.txt'));
  fb := TFindCutPoints.Create(g);
  Write('Bridges in g3 : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;

  g := TGraph.Create(FileName(chapter, 'tree.txt'));
  fb := TFindCutPoints.Create(g);
  Write('Bridges in tree : ');
  TArrayUtils_str.Print(fb.Return);
  fb.Free;
end;

{ TFindCutPoints }

constructor TFindCutPoints.Create(g: IGraph);
var
  v: integer;
begin
  _G := g;

  _Ret := THashSet_int.Create;
  SetLength(_Visited, _G.V);
  SetLength(_Ord, _G.V);
  SetLength(_Low, _G.V);
  _Cnt := 0;

  for v := 0 to _G.V - 1 do
    if not _Visited[v] then
      __Dfs(v, v);
end;

destructor TFindCutPoints.Destroy;
begin
  inherited Destroy;
end;

function TFindCutPoints.Return: TArr_str;
var
  res: IList_str;
  e: integer;
begin
  res := TArrayList_str.Create;

  for e in _Ret.ToArray do
    res.AddLast(e.ToString);

  Result := res.ToArray;
end;

procedure TFindCutPoints.__Dfs(v: integer; parent: integer);
var
  w, child: integer;
begin
  _Visited[v] := true;
  _Ord[v] := _Cnt;
  _Low[v] := _Ord[v];
  _Cnt += 1;
  child := 0;

  for w in _G.Adj(v) do
  begin
    if not _Visited[w] then
    begin
      __Dfs(w, v);
      _Low[v] := Min(_Low[v], _Low[w]);

      if (v <> parent) and (_Low[w] >= _Ord[v]) then
        _Ret.Add(v);

      child += 1;
      if (v = parent) and (child > 1) then
        _Ret.Add(v);
    end
    else if w <> parent then
      _Low[v] := Min(_Low[v], _Low[w]);
  end;
end;

end.
