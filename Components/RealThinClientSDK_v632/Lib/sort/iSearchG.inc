iSearch_G(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if ({$IFDEF RTC_GENERIC}infoC.Compare(x^.info,v)>0{$ELSE}x^.info>v{$ENDIF}) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if ({$IFDEF RTC_GENERIC}infoC.Compare(x^.info,v)=0{$ELSE}x^.info=v{$ENDIF}) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end
