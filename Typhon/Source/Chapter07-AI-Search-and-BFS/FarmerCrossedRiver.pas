unit FarmerCrossedRiver;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.UString;

type
  TFarmerCrossedRiver = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TFarmerCrossedRiver }

constructor TFarmerCrossedRiver.Create;
const

  deads: TArr_str = ('0110', '0101');
var
  deadSet: ISet_str;
  queue: IQueue_str;
begin
  deadSet := TSet_str.Create();
  for i := 0 to high(deads) do
    deadSet.Add(deads[i]);

  queue := TQueue_str.Create;
  queue.EnQueue(')
end;

destructor TFarmerCrossedRiver.Destroy;
begin
  inherited Destroy;
end;

end.
