unit GTA.DWeightedGraphBFS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TWeightedGraphBFS = class(TObject)
  private
    _Graph: TWeightedGraph;
    _Visited: TArr_bool;
    _Order: IList_int;

    procedure __Bfs(v: integer);

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function Order: TArr_int;
  end;

implementation

{ TWeightedGraphBFS }

constructor TWeightedGraphBFS.Create(g: IWeightedGraph);
begin
  _Graph := g as TWeightedGraph;
end;

destructor TWeightedGraphBFS.Destroy;
begin
  inherited Destroy;
end;

function TWeightedGraphBFS.Order: TArr_int;
begin

end;

procedure TWeightedGraphBFS.__Bfs(v: integer);
var
  queue: TQueue_int;
begin
  queue := TQueue_int.Create;
  try

  finally
    queue.Free;
  end;
end;

end.

