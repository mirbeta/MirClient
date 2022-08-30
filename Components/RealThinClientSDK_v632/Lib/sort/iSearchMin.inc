iSearch_Min(var i:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end
