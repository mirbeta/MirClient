iSearch_GE(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and ({$IFDEF RTC_GENERIC}infoC.Compare(v,x^.info)<>0{$ELSE}v<>x^.info{$ENDIF}) do
    begin
    if ({$IFDEF RTC_GENERIC}infoC.Compare(x^.info,v)>0{$ELSE}x^.info>v{$ENDIF}) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end
