RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=infoNil;
  head^.key:=itemMin;
{$IFDEF RTC_BINTREE}
  head^.r2:=z;
  head^.l2:=z;
{$ENDIF}
  cnt:=0;
  end
