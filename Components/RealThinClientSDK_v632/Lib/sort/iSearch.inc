iSearch(const v:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  while (x<>z) and ({$IFDEF RTC_GENERIC}infoC.Compare(v,x^.info)<>0{$ELSE}v<>x^.info{$ENDIF}) do
    if ({$IFDEF RTC_GENERIC}infoC.Compare(v,x^.info)<0{$ELSE}v<x^.info{$ENDIF}) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end
