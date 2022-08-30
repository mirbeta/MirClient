Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if {$IFDEF RTC_GENERIC}(itemC.Compare(xv,g^.key)<0)<>(itemC.Compare(xv,p^.key)<0){$ELSE}(xv<g^.key)<>(xv<p^.key){$ENDIF} then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,c^.key)<0{$ELSE}xv<c^.key{$ENDIF}) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,g^.key)<0{$ELSE}xv<g^.key{$ENDIF}) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,c^.key)<0{$ELSE}xv<c^.key{$ENDIF}) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if ({$IFDEF RTC_GENERIC}itemC.Compare(xv,gg^.key)<0{$ELSE}xv<gg^.key{$ENDIF}) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end
