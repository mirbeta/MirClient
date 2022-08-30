Remove(const v:itemType);
  var
    a:byte;

  begin
  Parents^.NodeCount:=0;

  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and ({$IFDEF RTC_GENERIC}itemC.Compare(v,t^.key)<>0{$ELSE}v<>t^.key{$ENDIF}) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if ({$IFDEF RTC_GENERIC}itemC.Compare(v,t^.key)<0{$ELSE}v<t^.key{$ENDIF}) then t:=t^.l else t:=t^.r;
    end;

  if t=z then
    raise Exception.Create('Key not found !');

  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;

{$IFDEF RTC_BINTREE}
  xv2:=t^.info;
  Parents^.NodeCount:=0;

  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<>0{$ELSE}v<>x^.key{$ENDIF}) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,x^.info)<0{$ELSE}xv2<x^.info{$ENDIF}) then x:=x^.l2
    else if ({$IFDEF RTC_GENERIC}infoC.Compare(xv2,x^.info)>0{$ELSE}xv2>x^.info{$ENDIF}) then x:=x^.r2
    else if ({$IFDEF RTC_GENERIC}itemC.Compare(v,x^.key)<0{$ELSE}v<x^.key{$ENDIF}) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise Exception.Create('BinTree -> structure corrupt !');

  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
{$ENDIF}

  t^.info:=infoNil;
  t^.key:=itemMin;
  del_node(t);

  Dec(cnt);
  end
