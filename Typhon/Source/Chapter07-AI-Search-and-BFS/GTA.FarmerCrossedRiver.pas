unit GTA.FarmerCrossedRiver;

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
    _order: IList_str;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Return;
  end;

procedure Main;

implementation

procedure Main;
var
  fcr: TFarmerCrossedRiver;
begin
  fcr := TFarmerCrossedRiver.Create;
  fcr.Return;

  fcr.Free;
end;

{ TFarmerCrossedRiver }

constructor TFarmerCrossedRiver.Create;
const
  // 农、羊、菜、狼
  DEAD_ARR: TArr_str = ('1000', '1001', '1010', '0111', '0110', '0101');
var
  deadSet: ISet_str;
  queue: IQueue_str;
  visited: ISet_str;
  temp, cur: UString;
  o: UChar;
  arr_Cur: TArr_chr;
  list: IList_str;
  i: integer;
begin
  _order := TList_str.Create;
  queue := TQueue_str.Create;
  visited := THashSet_str.Create;
  list := TList_str.Create;

  deadSet := THashSet_str.Create;
  for i := 0 to High(DEAD_ARR) do
    deadSet.Add(DEAD_ARR[i]);

  visited.Add('1111');
  _order.AddLast('1111');

  temp := '0011';
  queue.EnQueue(temp);
  visited.Add(temp);
  _order.AddLast(temp);
  while not queue.IsEmpty do
  begin
    list.Clear;
    cur := queue.DeQueue;
    arr_Cur := cur.ToCharArray;

    if arr_Cur[0] = '0' then
      arr_Cur[0] := '1'
    else
      arr_Cur[0] := '0';

    temp := UString.Create(arr_Cur);
    list.AddLast(temp);

    for i := 1 to High(arr_Cur) do
    begin
      o := arr_Cur[i];

      if arr_Cur[i] <> arr_Cur[0] then
        arr_Cur[i] := arr_Cur[0];

      temp := UString.Create(arr_Cur);
      list.AddLast(temp);

      arr_Cur[i] := o;
    end;

    for temp in list.ToArray do
    begin
      if (not deadSet.Contains(temp))
        and (not visited.Contains(temp))
        and (temp.Chars[0] <> _order.GetLast.Chars[0]) then
      begin
        queue.EnQueue(temp);
        visited.Add(temp);
        _order.AddLast(temp);

        if temp = '0000' then
          Exit;
      end;
    end;
  end;
end;

destructor TFarmerCrossedRiver.Destroy;
begin
  inherited Destroy;
end;

procedure TFarmerCrossedRiver.Return;
var
  sb: TStringBuilder;
  a, b, c: TArr_str;
  temp: UString;
  i, j: integer;
begin
  c := ['农', '羊', '菜', '狼'];

  sb := TStringBuilder.Create;
  try
    SetLength(a, 4);
    SetLength(b, 4);

    for i := 0 to _order.Count - 1 do
    begin
      TArrayUtils_str.FillArray(a, '  ');
      TArrayUtils_str.FillArray(b, '  ');
      temp := _order[i];

      for j := 0 to temp.Length - 1 do
      begin
        if temp.Chars[j] = '0' then
          b[j] := c[j]
        else
          a[j] := c[j];
      end;

      sb.Append((i + 1).ToString + ': ' + _order[i] + ' -> ');
      for j := 0 to High(a) do
        sb.Append(a[j]);
      sb.Append(#9#9);
      for j := 0 to High(b) do
        sb.Append(b[j]);
      sb.AppendLine;
    end;

    Write(sb.ToString);
  finally
    sb.Free;
  end;
end;

end.
