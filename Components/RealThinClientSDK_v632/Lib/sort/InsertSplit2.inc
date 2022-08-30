Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
  {$IFDEF RTC_GENERIC}
    if ( (infoC.Compare(xv2,g^.info)<0) or ((infoC.Compare(xv2,g^.info)=0) and (itemC.Compare(xv,g^.key)<0)) ) <>
       ( (infoC.Compare(xv2,p^.info)<0) or ((infoC.Compare(xv2,p^.info)=0) and (itemC.Compare(xv,p^.key)<0)) ) then
  {$ELSE}
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
  {$ENDIF}
      begin
      // p_rotate_g; ->
      if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,g^.info)<0{$ELSE}xv2<g^.info{$ENDIF}) then c:=g^.l2
      else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,g^.info)>0{$ELSE}xv2>g^.info{$ENDIF}) then c:=g^.r2
      else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,g^.key)<0{$ELSE}xv<g^.key{$ENDIF}) then c:=g^.l2
      else c:=g^.r2;
      if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,c^.info)<0{$ELSE}xv2<c^.info{$ENDIF}) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,c^.info)>0{$ELSE}xv2>c^.info{$ENDIF}) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,c^.key)<0{$ELSE}xv<c^.key{$ENDIF}) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,g^.info)<0{$ELSE}xv2<g^.info{$ENDIF}) then g^.l2:=p
      else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,g^.info)>0{$ELSE}xv2>g^.info{$ENDIF}) then g^.r2:=p
      else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,g^.key)<0{$ELSE}xv<g^.key{$ENDIF}) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,gg^.info)<0{$ELSE}xv2<gg^.info{$ENDIF}) then c:=gg^.l2
    else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,gg^.info)>0{$ELSE}xv2>gg^.info{$ENDIF}) then c:=gg^.r2
    else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,gg^.key)<0{$ELSE}xv<gg^.key{$ENDIF}) then c:=gg^.l2
    else c:=gg^.r2;
    if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,c^.info)<0{$ELSE}xv2<c^.info{$ENDIF}) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,c^.info)>0{$ELSE}xv2>c^.info{$ENDIF}) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,c^.key)<0{$ELSE}xv<c^.key{$ENDIF}) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,gg^.info)<0{$ELSE}xv2<gg^.info{$ENDIF}) then gg^.l2:=x
    else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,gg^.info)>0{$ELSE}xv2>gg^.info{$ENDIF}) then gg^.r2:=x
    else if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,gg^.key)<0{$ELSE}xv<gg^.key{$ENDIF}) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end
