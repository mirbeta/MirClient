Search_GE(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<>0{$ELSE}v<>x^.key{$ENDIF}) do
    begin
    if ({$IFDEF RTC_GENERIC}itemC.Compare(x^.key,v)>0{$ELSE}x^.key>v{$ENDIF}) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end
