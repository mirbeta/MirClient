RemoveThis(var t:pnode);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=infoNil;
  t^.key:=itemMin;
{$IFDEF RTC_BINTREE}
  t^.r2:=z;
  t^.l2:=z;
{$ENDIF}
  del_node(t);
  t:=z;
  end
