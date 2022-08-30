Search(const v:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<>0{$ELSE}v<>x^.key{$ENDIF}) do
    if ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<0{$ELSE}v<x^.key{$ENDIF}) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end
