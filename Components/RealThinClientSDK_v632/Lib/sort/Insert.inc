Insert(const v:itemType;const info:infoType);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<0{$ELSE}v<x^.key{$ENDIF}) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if ({$IFDEF RTC_GENERIC}itemC.Compare(v,p^.key)<0{$ELSE}v<p^.key{$ENDIF}) then p^.l:=x else p^.r:=x;
  Insert_Split;

{$IFDEF RTC_BINTREE}
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,x^.info)<0{$ELSE}xv2<x^.info{$ENDIF}) then x:=x^.l2
    else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,x^.info)>0{$ELSE}xv2>x^.info{$ENDIF}) then x:=x^.r2
    else if ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<0{$ELSE}v<x^.key{$ENDIF}) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,p^.info)<0{$ELSE}xv2<p^.info{$ENDIF}) then p^.l2:=x
  else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,p^.info)>0{$ELSE}xv2>p^.info{$ENDIF}) then p^.r2:=x
  else if ({$IFDEF RTC_GENERIC}itemC.Compare(v,p^.key)<0{$ELSE}v<p^.key{$ENDIF}) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
{$ENDIF}

  Inc(cnt);
  end
