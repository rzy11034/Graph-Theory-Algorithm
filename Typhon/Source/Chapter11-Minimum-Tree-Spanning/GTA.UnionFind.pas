unit GTA.UnionFind;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

type
  TUnionFind = class(TObject)
  private
    _ID: TArr_int;

  public
    constructor Create(n: integer);
    destructor Destroy; override;

    procedure Union(p, q: integer);
    function Find(p: integer): integer;
    function IsConnected(p, q: integer): boolean;
  end;

implementation

{ TUnionFind }

constructor TUnionFind.Create(n: integer);
var
  i: integer;
begin
  SetLength(_ID, n);
  for i := 0 to High(_ID) do
    _ID[i] := i;
end;

destructor TUnionFind.Destroy;
begin
  inherited Destroy;
end;

function TUnionFind.Find(p: integer): integer;
begin
  if p <> _ID[p] then
    _ID[p] := Find(_ID[p]);

  Result := _ID[p];
end;

function TUnionFind.IsConnected(p, q: integer): boolean;
begin
  Result := Find(p) = Find(q);
end;

procedure TUnionFind.Union(p, q: integer);
var
  pRoot, qRoot: integer;
begin
  pRoot := Find(p);
  qRoot := Find(q);

  if pRoot = qRoot then Exit;

  _ID[pRoot] := qRoot;
end;

end.
