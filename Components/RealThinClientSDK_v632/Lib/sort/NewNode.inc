New_Node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
  var
    p:pnodearr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(pnodearr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(tnode));
  FillChar(Result^,SizeOf(tnode),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    {$IFDEF RTC_BINTREE}
    l2:=l;
    r2:=r;
    b2:=b;
    {$ENDIF}
    end;
  end
