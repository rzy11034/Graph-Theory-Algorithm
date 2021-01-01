unit GTA.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  GTA.Graph,
  DeepStar.UString;

type
  IGraph = GTA.interfaces.IGraph;
  TGraph = GTA.Graph.TGraph;

function FileName(chapter, file_: UString): UString;

implementation

function FileName(chapter, file_: UString): UString;
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
    sb.Append(chapter).Append(PathDelim).Append(file_);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

end.
