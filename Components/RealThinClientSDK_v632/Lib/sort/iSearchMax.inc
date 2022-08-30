iSearch_Max(var i:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end
