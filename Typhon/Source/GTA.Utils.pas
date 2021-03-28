unit GTA.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  GTA.DGraph,
  GTA.DWeightedGraph,
  GTA.WeightedEdge,
  DeepStar.UString,
  DeepStar.Utils;

type
  IGraph = GTA.Interfaces.IGraph;
  TGraph = GTA.DGraph.TGraph;

  IWeightedGraph = GTA.Interfaces.IWeightedGraph;
  TWeightedGraph = GTA.DWeightedGraph.TWeightedGraph;

  IWeightedEdge = GTA.Interfaces.IWeightedEdge;
  TWeightedEdge = GTA.WeightedEdge.TWeightedEdge;

const
  XY_DIRS_4: TArr2D_int = ((-1, 0), (0, 1), (1, 0), (0, -1));
  XY_DIRS_8: TArr2D_int = ((-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1));

function FileName(chapter, newFile: UString): UString;

implementation

function FileName(chapter, newFile: UString): UString;
const
  {$IFDEF MSWINDOWS}
  PR_PATH = '..\..\Source\';
  {$ELSE}
  PR_PATH = '../../Source/';
  {$ENDIF}
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append(PR_PATH);
    sb.Append(chapter).Append(PathDelim).Append(newFile);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

end.
