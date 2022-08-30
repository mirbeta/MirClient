Search_L(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if ({$IFDEF RTC_GENERIC}itemC.Compare(x^.key,v)<0{$ELSE}x^.key<v{$ENDIF}) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if ({$IFDEF RTC_GENERIC}itemC.Compare(x^.key,v)=0{$ELSE}x^.key=v{$ENDIF}) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end
