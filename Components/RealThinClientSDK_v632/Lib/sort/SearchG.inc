Search_G(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if ({$IFDEF RTC_GENERIC}itemC.Compare(x^.key,v)>0{$ELSE}x^.key>v{$ENDIF}) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if ({$IFDEF RTC_GENERIC}itemC.Compare(x^.key,v)=0{$ELSE}x^.key=v{$ENDIF}) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end
