unit GTA.UnionFind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DeepStar;

type
  TUnionFind = class(TObject)
  private
    _Parents: tar

  public
    constructor Create(n: integer);
    destructor Destroy; override;

  end;

implementation

{ TUnionFind }

constructor TUnionFind.Create(n: integer);
begin

end;

destructor TUnionFind.Destroy;
begin
  inherited Destroy;
end;

end.

