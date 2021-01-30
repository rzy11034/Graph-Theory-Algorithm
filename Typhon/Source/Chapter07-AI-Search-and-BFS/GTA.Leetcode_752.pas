unit GTA.Leetcode_752;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.UString;

type
  TSolution = class
  public
    function OpenLock(deadends: TArr_str; target: UString): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  deadends: TArr_str;
  targer: UString;
begin
  with TSolution.Create do
  begin
    deadends := ['0201', '0101', '0102', '1212', '2002'];
    targer := '0202';
    WriteLn(OpenLock(deadends, targer));

    //////////////////////////////////
    deadends := ['8888'];
    targer := '0009';
    WriteLn(OpenLock(deadends, targer));

    Free;
  end;
end;

{ TSolution }

function TSolution.OpenLock(deadends: TArr_str; target: UString): integer;
var
  n: integer = 1000;
  temp, curS: UString;
  deadSet: ISet_str;
  queue: IQueue_str;
  visited: IMap_str_int;
  i: integer;
  o: UChar;
  nextS: IList_str;
  arrCur: TArr_chr;
begin
  deadSet := THashSet_str.Create(n);
  for temp in deadends do
    deadSet.Add(temp);

  if deadset.Contains(target) then Exit(-1);
  if deadset.Contains('0000') then Exit(-1);
  if target = '0000' then Exit(0);

  // BFS
  queue := TQueue_str.Create;
  visited := THashMap_str_int.Create(n);

  temp := '0000';
  queue.EnQueue(temp);
  visited.Add(temp, 0);

  while not queue.IsEmpty do
  begin
    curS := queue.DeQueue;
    nextS := TArrayList_str.Create;

    arrCur := curS.ToCharArray;
    for i := 0 to High(arrCur) do
    begin
      o := arrCur[i];

      if arrCur[i] = '9' then
        arrCur[i] := '0'
      else
        arrCur[i] := Succ(arrCur[i]);
      temp := UString.Create(arrCur);
      nextS.AddLast(temp);
      arrCur[i] := o;

      if arrCur[i] = '0' then
        arrCur[i] := '9'
      else
        arrCur[i] := Pred(arrCur[i]);
      temp := UString.Create(arrCur);
      nextS.AddLast(temp);
      arrCur[i] := o;
    end;

    for i := 0 to nextS.Count - 1 do
    begin
      temp := nextS[i];

      if not deadSet.Contains(temp)
        and (not visited.ContainsKey(temp)) then
      begin
        queue.EnQueue(temp);
        visited.Add(temp, visited.GetItem(curS) + 1);

        if temp = target then
          Exit(visited.GetItem(temp));
      end;
    end;
  end;

  Result := -1;
end;

end.
